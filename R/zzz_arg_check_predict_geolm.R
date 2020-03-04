#' Check arguments of predict_geolm*
#'
#' @param coordnames 
#' @param newdata 
#' @param nsim 
#' @param return_type 
#' @param dmethod 
#' @param mspe 
#' @noRd
arg_check_predict_geolm = function(coordnames, newdata, nsim, return_type, dmethod, compute_mspe) {
  arg_check_newdata(newdata, coordnames)
  arg_check_nsim(nsim)
  arg_check_return_type(return_type)
  arg_check_dmethod(dmethod)
  arg_check_compute_mspe(compute_mspe)
}
