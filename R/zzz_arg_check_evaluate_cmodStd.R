#' Argument check evaluate.cmodStd
#'
#' @param mod A cmodStd
#' @param d distnace matrix or ganisoD object if mod$ganiso != NULL
#' @param e Logical value. Measurement error?
#' @param f Logical value. Finescale error?
#' @noRd
arg_check_evaluate_cmodStd = function(mod, d, e, f) {
  if (mod$ratio < 1 && is.matrix(d)) {
    stop("d must be produced by the ganiso_d function when mod$ganiso is not NULL")
  } else if (is.matrix(d)) {
    arg_check_d(d)
  }
  arg_check_e(e)
  arg_check_f(f)
}
