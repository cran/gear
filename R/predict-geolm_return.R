#' Return output of predict.geolm* in proper format
#'
#' @param kdtf A data.frame with pred, mspe, rmspe, and possibly sim
#' @param newcoords A matrix with the prediction coordinates
#' @param return_type The type of object to return
#' @noRd
return_predict_geolm = function(kdtf, newcoords, return_type) {
  # return results
  if (return_type == "SpatialPointsDataFrame") {
    sp::coordinates(kdtf) = newcoords
    return(kdtf)
  } else if (return_type == "gearPredict") {
    kdtf$x1 = newcoords[,1]
    kdtf$x2 = newcoords[,2]
    class(ktdf) = c("gearPredict", "data.frame")
    return(kdtf)
  } else if (return_type == "sf") {
    if (requireNamespace("sf", quietly = TRUE)) {
      ktdf$x1 = newcoords[,1]
      ktdf$x2 = newcoords[,2]
      return(sf::st_as_sf(kdtf, coords = c("x1", "x2")))
    } else {
      warning("sf package must be installed to enable this functionality. Returning data.frame")
      return(ktdf)
    }
  }
}
