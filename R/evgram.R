#' Empirical (semi-)variogram
#'
#' \code{evgram} computes the empirical semivariogram. The
#' variogram is twice the semivariogram.
#'
#' Note that the directions may be different from other
#' packages (e.g., \code{gstat} or \code{geoR} packages)
#' because those packages calculate angles clockwise from
#' the y-axis, which is a convention frequently seen in
#' geostatistics (e.g., the GSLIB software library).
#'
#' Computing the empirical semivariogram for the residuals
#' of \code{lm(response ~ 1)} will produce identical results
#' to simply computing the sample semivariogram from the
#' original response. However, if a trend is specified (the
#' righthand side of ~ has non-trival covariates), then the
#' empirical semivariogram of the residuals will differ
#' from that of the original response.  A trend should be
#' specified when the mean is non-stationary over the
#' spatial domain.
#'
#' @param formula A formula describing the relationship
#'   between the response and any covariates of interest,
#'   e.g., response ~ 1.  The variogram is computed for the
#'   residuals of the linear model \code{lm(formula, data)}.
#' @param data A \code{data.frame},
#'   \code{SpatialPointsDataFrame},
#'   \code{SpatialPixelsDataFrame}, or
#'   \code{SpatialGridDataFrame} object.
#' @param coordnames The columns of \code{data} containing
#'   the spatial coordinates, provided as a formula (e.g.,
#'   \code{~ x + y}), column numbers (e.g., \code{c(1, 2)}),
#'   or column names (e.g., \code{c("x", "y")}). The default
#'   is \code{NULL}.
#' @param nbins The number of bins (tolerance regions) to
#'   use when estimating the sample semivariogram.
#' @param maxd The maximum distance used when calculating
#'   the semivariogram.  Default is NULL, in which case half
#'   the maximum distance between coordinates is used.
#' @param angle A single value (in degrees) indicating the
#'   starting direction for a directional variogram.  The
#'   default is 0.
#' @param ndir The number of directions for which to
#'   calculate a sample semivariogram.  The default is 1,
#'   meaning calculate an omnidirectional semivariogram.
#' @param type The name of the estimator to use in the
#'   estimation process.  The default is \code{"standard"}, the
#'   typical method-of-moments estimator.  Other options
#'   include \code{"cressie"} for the robust Cressie-Hawkins
#'   estimator, and \code{"cloud"} for a semivariogram cloud
#'   based on the standard estimator.  If \code{"cloud"} is specified,
#'   the \code{nbins} argument is ignored.
#' @param npmin The minimum number of pairs of points to use
#'   in the semivariogram estimator.  For any bins with
#'   fewer points, the estimate for that bin is dropped.
#' @param longlat A logical indicating whether Euclidean
#'   (\code{FALSE}) or Great Circle distance (WGS84
#'   ellipsoid) (\code{longlat = TRUE}) should be used.
#'   Default is \code{FALSE}.
#' @param verbose Logical value indicating whether
#' computation information should be printed. Default is
#'   \code{TRUE}.
#' @inheritParams angle2d
#' @return Returns an \code{evgram}.
#' @author Joshua French
#' @export
#' @examples
#' data(co)
#' v = evgram(Al ~ 1, co, ~ easting + northing)
#' plot(v)
#' v2 = evgram(Al ~ 1, co, c("easting", "northing"), angle = 22.5, ndir = 4)
#' plot(v2)
evgram = function(formula, data, coordnames = NULL, nbins = 10,
                  maxd = NULL, angle = 0, ndir = 1,
                  type = "standard", npmin = 2,
                  longlat = FALSE,
                  verbose = TRUE,
                  invert = TRUE) {
  arg_check_evgram(formula, data, coordnames, nbins, maxd,
                   angle, ndir, type, npmin, verbose)
  id = all.vars(formula)[1]
  if (is.data.frame(data)) {
    sp::coordinates(data) = coordnames
  }

  # extract coordinates, determine number of coordinates
  coords = sp::coordinates(data)
  # number of observations
  N = nrow(coords)
  # extract (residual) response
  y = stats::lm(formula, data = data)$resid

  # determine omni-directional or directional
  dirname = ifelse(ndir >= 2, "directional", "omnidirectional")
  # print message if verbose
  if (verbose) {
    message(paste("Computing", dirname,
                  "empirical semivariogram for", id,
                  "using", type, "estimator"))
  }

  # create indexes to use in building of sample semivariograms
  idx1 = rep(1:(N - 1), times = (N - 1):1)
  idx2 = unlist(sapply(1:(N - 1), function(i) (1:N)[-(1:i)]))

  # distances for unique pairs of points
  # d2 = c(dist(coords))
  d = sp::spDists(as.matrix(coords), longlat = longlat)
  d = c(d[lower.tri(d)])

  # difference of unique pairs of points
  diff = y[idx1] - y[idx2]

  # determine maximum distance of estimator
  if (is.null(maxd)) maxd = max(d)/2
  # determine distances of tolerance regions
  # for empirical semivariogram estimator
  bindist = seq(0, maxd, len = nbins + 1)
  # determine classification cuts for unique pairs of points by distance
  dcuts = cut(d, breaks = bindist)

  # default angle_dir, bin_angles
  angle_dir = angle
  bin_angles = "[0, 180)"
  if (ndir > 1) {
    # determine change between angle directions
    angle_change = 180/ndir
    # determine main angles
    angle_dir = (angle + 0:(ndir - 1) * angle_change)
    # determine tolerance bins for angles
    bin_angles = angle + (seq(-1, (2 * ndir - 1), by = 2)) * angle_change/2
    obs_angles = angle2d(coords[idx1, ], coords[idx2, ],
                         invert = invert)
    # adjust depending on whether there are angles < 0 or > 180
    if (min(bin_angles) < 0) {
    # if any angles are negative, correct them by rotating 180 degrees
    # calculate angles between observed data
      max_bin_angle  = max(bin_angles)
      obs_angles[obs_angles > max_bin_angle] = obs_angles[obs_angles > max_bin_angle] - 180
    } else if (max(bin_angles) > 180) {
      min_bin_angle = min(bin_angles)
      obs_angles[obs_angles < min_bin_angle] = obs_angles[obs_angles < min_bin_angle] + 180
    }

    # determine split by angle
    acuts = cut(obs_angles, bin_angles, include.lowest = TRUE,
                labels = angle_dir) # include 0 and 180
    # determine indices of split
    split_idx = split(seq_along(d), acuts)
    # determine semivariograms for each direction
    semi = lapply(split_idx,
                  function(idx) {
                    omni_semivariogram(d[idx], diff[idx], dcuts[idx], npmin, type)
                  })
    # determine angle_bin
    angle_name = rep(names(semi),
                     times = unlist(lapply(semi, nrow), use.names = FALSE))
    # bind semivariograms from each direction
    semi = do.call(rbind.data.frame, semi)
    # remove ugly row names
    row.names(semi) = NULL
    # add name of angle bin
    semi$angle = angle_name
  }else {
    semi = omni_semivariogram(d, diff, dcuts, npmin, type)
  }
  out = list(id = id,
             nbins = nbins,
             bindist = bindist,
             maxd = maxd,
             angle = angle_dir,
             ndir = ndir,
             binangles = bin_angles,
             type = type,
             semivariogram = semi,
             object = data)
  class(out) = "evgram"
  return(out)
}
