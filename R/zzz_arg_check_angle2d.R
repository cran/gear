#' Argument check angle2d
#'
#' @param coords1 An \eqn{N \times 2} matrix of spatial
#'   coordinates.
#' @param coords2 An \eqn{N \times 2} matrix of spatial
#'   coordinates.
#' @param radians A logical indicating whether degrees or
#'   radians should be returned.  Default is FALSE, meaning
#'   return angle in degrees.
#' @param invert A logical indiciating whether the x and y
#'   axes should be switched.
#' @noRd
arg_check_angle2d = function(coords1, coords2, radians, invert) {
  # sanity checking of arguments
  if (ncol(coords1) != 2) {
    stop("ncol(coords1) != 2")
  }
  if (ncol(coords2) != 2) {
    stop("ncol(coords2) != 2")
  }
  if (nrow(coords1) != nrow(coords2)) {
    stop("nrow(coords1) != nrow(coords2)")
  }
  if (!is.logical(radians) || length(radians) != 1) {
    stop("radians must be a single logical value")
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("invert must be a single logical value")
  }
}
