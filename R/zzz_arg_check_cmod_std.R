#' Argument check cmod_std
#' @param model A standard covariance model type.
#' @param psill The partial sill of the model.  Must be a
#'   positive number.
#' @param r The range parameter r.  Must be a positive
#'   number.
#' @param evar The variance of the errors.  Must be
#'   non-negative number.  The default is 0.
#' @param fvar The finescale variance (microscale error).
#'   Must be a non-negative number.  The default is 0.
#' @param par3 The value of the third parameter for 3
#'   parameter models.  Must be a positive number.  The
#'   default is 0.5.
#' @param longlat A logical value indicating whether great
#'   circle distance should be used. The default is
#'   \code{FALSE}.
#' @noRd
arg_check_cmod_std = function(model, psill, r, evar, 
                              fvar, par3, longlat, angle,
                              ratio, radians, invert) {
  arg_check_cmod_std_model(model)
  arg_check_psill(psill)
  arg_check_r(r)
  arg_check_evar(evar)
  arg_check_fvar(fvar)
  arg_check_par3(par3)
  arg_check_longlat(longlat)
  arg_check_radians(radians)
  arg_check_angle(angle, radians)
  arg_check_ratio(ratio)
  arg_check_invert(invert)
}
