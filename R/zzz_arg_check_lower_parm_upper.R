#' Check lower, upper bounds for parm for estimate.geolm_cmodStd
#'
#' @param lower A vector of lower bounds for relevant parameters
#' @param parm A vector of initial starting values for relevant parameters
#' @param upper A vector of upper bounds for relevant parameters
#' @noRd
arg_check_lower_parm_upper = function(lower, parm, upper, mod) {
  if (length(lower) != length(parm)) {
    message("The length of lower must must match the number of parameters to be estimated.")
    message("lower:")
    print(lower)
    message("initial values:")
    names(parm) = names(lower)
    print(parm)
    stop("Please specify lower to have the same length as the number of parameters to be estimated.")
  }
  if (length(upper) != length(parm)) {
    message("The length of upper must must match the number of parameters to be estimated.")
    message("upper:")
    print(upper)
    message("initial values:")
    names(parm) = names(upper)
    print(parm)
    stop("Please specify upper to have the same length as the number of parameters to be estimated.")
  }
  for (i in seq_along(lower)) {
    if (lower[i] > parm[i] | parm[i] > upper[i]) {
      message("The initial parameter values are not within the specified bounds.")
      combine = rbind(lower, parm, upper)
      rownames(combine) = c("lower", "initial", "upper")
      colnames(combine) = names(lower)
      print(combine)
      stop("Please specify lower and upper so that the initial starting values are between them or change your initial values.")
    }
  }
}
