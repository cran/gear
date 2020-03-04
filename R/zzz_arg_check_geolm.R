arg_check_geolm = function(formula, data, coordnames, mod,
                           weights, mu) {
  arg_check_geolm_formula(formula)
  if (!is.data.frame(data)) {
    stop("data should be a data frame")
  }
  df_names = names(data)
  if (length(coordnames) != 2) {
    stop("coordnames must refer to only two columns of data")
  }
  if (min(is.element(coordnames, df_names)) == 0) {
    stop("The names in coordnames are not found in the column names of the data dataframe.")
  }
  valid_mod = c("cmodMan", "cmodStd")
  if (!is.element(class(mod), valid_mod)) {
    stop(paste("mod must be of one of the following classes:", paste(valid_mod, collapse = ", ")))
  }
  if (!is.null(weights)) {
    arg_check_weights(weights, nrow(data))
  }
  if (!is.null(mu)) {
    arg_check_mu(mu)
  }
}
