#' Check additional argument of predict.geolm_cmodMan
#'
#' @param y Vector of observed responses
#' @param vop Cross-covariance matrix
#' @param vp Covariance matrix of responses to be predicted
#' @noRd
arg_check_predict_geolm_cmodMan = function(y, vop, vp) {
  if (!is.matrix(vop)) {
    stop("vop must be a matrix")
  }
  if (length(dim(vop)) != 2) {
    stop("vop must be two-dimensional")
  }
  if (!is.matrix(vp)) {
    stop("vp must be a matrix")
  }
  if (length(dim(vp)) != 2) {
    stop("vp must be two-dimensional")
  }
  if (nrow(vp) != ncol(vp)) {
    stop("vp must be a square matrix")
  }
  if (nrow(vop) != length(y)) {
    stop("nrow(vop) != length(object$y)")
  }
  if (ncol(vop) != nrow(vp)) {
    stop("nrow(vop) != nrow(vp). They must match for compatibility.")
  }
}
