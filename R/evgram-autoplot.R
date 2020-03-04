#' Plot \code{evgram} object
#'
#' Plots object of class '\code{evgram}' produced by the
#' \code{\link[gear]{evgram}} function. The plotting
#' function internally calls the
#' \code{\link[lattice]{xyplot}} function.
#'
#' @param object An \code{evgram} object produced by the
#'   \code{\link[gear]{evgram}} function.
#' @param ... Not used
#' @param split A logical value indicating whether, for a
#'   directional semivariogram, the directional
#'   semivariograms should be displayed in a single or split
#'   panels.  Default is \code{FALSE}, for a single panel.
#' @return NULL
#' @author Joshua French
#' @method autoplot evgram
#' @export
#' @examples
#' data(co)
#' v = evgram(Al ~ 1, co, ~ easting + northing)
#' autoplot(v)
#' v2 = evgram(Al ~ 1, co, ~ easting + northing, angle = 22.5, ndir = 4)
#' autoplot(v2)
#' autoplot(v2, split = TRUE)
autoplot.evgram = function(object, ..., split = FALSE) {
  arg_check_split(split)
  # appease R CMD check
  distance = semivariance = angle = NULL
  # map aesthetics based on split
  # construct ggplot object
  g = ggplot2::ggplot(data = object$semivariogram) + 
    ggplot2::xlim(0, max(object$semivariogram$distance)) +
    ggplot2::ylim(0, max(object$semivariogram$semivariance))
  if (object$ndir == 1) {
    g + 
      ggplot2::geom_point(ggplot2::aes(x = distance,
                                       y = semivariance))
  } else if (split) {
    g + 
      ggplot2::geom_point(ggplot2::aes(x = distance,
                                       y = semivariance)) +
      ggplot2::facet_wrap(~ angle) 
  } else {
    g + 
      ggplot2::geom_point(ggplot2::aes(x = distance,
                                       y = semivariance,
                                       pch = angle,
                                       col = angle))
  }
}
