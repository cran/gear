#' Argument check cmod.man
#'
#' @param v The covariance matrix of the observed data, including any errors.  The matrix should be square, symmetric, and positive definite, though that latter two conditions are not checked.
#' @param evar The variance of the errors.  Must be non-negative number.  The default is 0.
#' @return NULL
#' @noRd
arg_check_cmod_man = function(v, evar) {
  if (!is.numeric(v) | nrow(v) != ncol(v) ) stop("v should be a square matrix")
  arg_check_evar(evar)
}
