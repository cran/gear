#' Check arguments of fitted.geolm
#'
#' @param object geolm object
#' @param x Matrix of covariates or NULL
#' @noRd
arg_check_fitted_geolm = function(object, x){
  if (is.null(object$mu)) {
    if (!is.matrix(x)) {
      stop("x must be a matrix")
    }
    if (nrow(x) == 0) {
      stop("x must have at least one row")
    }
    p = length(object$coeff)
    if (is.matrix(object$coeff)) { p = nrow(object$coeff) }
    if (ncol(x) != p) {
      stop("ncol(x) != number of regression coefficients")
    }
  }
}
