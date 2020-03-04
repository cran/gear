arg_check_evgram = function(formula, data, coordnames, 
                            nbins, maxd, angle, ndir, type,
                            npmin, verbose) {
  arg_check_formula(formula)
  if(!is.element(class(data), c("data.frame", "SpatialPointsDataFrame", "SpatialGridDataFrame", "SpatialPixelsDataFrame")))
  {
    stop("data not of appropriate class.  Should be of class data.frame, SpatialPointsDataFrame, SpatialGridDataFrame, or SpatialPixelsDataFrame.")
  }
  if(min(is.element(all.vars(formula), names(as.data.frame(data)))) == 0) {
    stop("some of the variables in formula are not in data")
  }
  if(is.data.frame(data)) {
    if(is.null(coordnames)) stop("coordnames cannot be NULL when data is a data.frame") 
    df_names = names(as.data.frame(data))
    if(class(coordnames) == "formula"){
      if(min(is.element(all.vars(coordnames), df_names) == 0)){
        stop("some of the variables in coordnames are not in data")
      }
    }else if (length(coordnames) != 2){
      stop("coordnames should have length 2 if not a formula")
    }else if(is.character(coordnames)){
      if(min(is.element(coordnames, df_names)) == 0){
        stop("The names in coordnames are not found in the column names of the data dataframe")
      }
    }else if(is.numeric(coordnames)){
      if(min(coordnames) < 1){
        stop("If coordnames specifies column numbers, then the column numbers must between 1 and ncol(data)")
      }
      if(max(coordnames) > ncol(data)){
        stop("If coordnames specifies column numbers, then the column numbers must between 1 and ncol(data)")
      }
    }else{
      stop("The format of coordnames is not recognized.  Please look at documentation.")
    }
  }
  if (nbins < 1 || length(nbins) != 1 || !is.numeric(nbins))
    stop("nbins should be a single integer >= 1")
  if (!is.null(maxd)) {
    if(maxd <= 0 || length(maxd) != 1 || !is.numeric(maxd))
      stop("maxd should be a single integer >= 1")
  }
  if(length(angle) != 1 || !is.numeric(angle) || angle < 0)
    stop("angle should be an single value >= 0")
  if(ndir < 1 || length(ndir) != 1 || !is.numeric(ndir))
    stop("ndir should be a single integer >= 1")
  if(length(type) != 1) stop("type should be a single name")
  if(!is.element(type, c("standard", "cressie", "cloud")))
    stop(paste(type, "is not a valid type"))
  if(npmin < 1 || length(npmin) != 1 || !is.numeric(npmin))
    stop("npmin should be a single integer >= 1")
  arg_check_verbose(verbose)
}

