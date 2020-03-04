#' Argument check estimate.geolm_cmodStd
#' @param reml  A logical value indicating whether standard
#'   maximum likelihood estimation should be performed
#'   (\code{reml = FALSE}).  If \code{reml = TRUE}, then
#'   restricted maximum likelihood is performed.  Default is
#'   \code{FALSE}.
#' @param noise_type A character vector indicating the type of noise (nugget)
#' variance to estimate. The default is (\code{est = "e"}), indicating
#' the error variance should be estimated. Alternatively, the user
#' can specify (\code{est = "f"}), indicating the finescale
#' variance should be estimated. The other type of noise variance
#' is set to 0, otherwise the model is not identifiable.
#' @param est_nugget A logical value indicating whether the nugget
#'   variance (\code{evar} or \code{fvar}) should be estimated. The default is \code{TRUE}.
#' @param est_par3 A logical value indicating whether \code{par3} should be estimated (for an appropriate covariance model such
#' ash \code{"matern"} or \code{"amatern"}. The default is \code{TRUE}.
#' @param est_angle A logical value indicating whether the geometric anisotropy angle should be estimated. 
#' The default is \code{FALSE}. This argument is ignored of \code{mod$ganiso} is \code{NULL}.
#' @param est_ratio A logical value indicating whether the geometric anisotropy ratio of minor
#' axis length to major axis length should be estimated. The default is \code{FALSE}. This argument is ignored of \code{mod$ganiso} is \code{NULL}.
#' @noRd
arg_check_estimate_geolm_cmodStd = function(reml, noise_type, 
                                            est_nugget,
                                            est_par3,
                                            est_angle,
                                            est_ratio,
                                            control,
                                            verbose) {
  arg_check_reml(reml)
  arg_check_noise_type(noise_type)
  arg_check_est_nugget(est_nugget)
  arg_check_est_par3(est_par3)
  arg_check_est_angle(est_angle)
  arg_check_est_ratio(est_ratio)
  if (!is.list(control)) stop("control must be a list")
  arg_check_verbose(verbose)
}
