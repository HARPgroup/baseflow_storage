### This function calculates the slope ###
#'@title get_basin_slope
#'@name get_basin_slope
#' @details Calculates the average watershed slope from nhdplustools of a given
#'   USGS gage
#' @param gage_id Character. The gage ID of a USGS gage e.g. "01631000" for S.F.
#'   Shenandoah River at Front Royal
#' @return The average watershed slope in percent
#'@export get_basin_slope
get_basin_slope <- function(gage_id){

  site <- nhdplusTools::get_nldi_feature(list(featureSource = "nwissite",
                                              featureID = paste0("USGS-", gage_id)))
  gage_comid <- site$comid

  watershed_slope <- nhdplusTools::get_catchment_characteristics(
    varname = "TOT_BASIN_SLOPE",
    ids = gage_comid
  )

  slope <- watershed_slope[,3]
  result <- (paste0("The slope is ", slope, "%"))
  cat(result, "\n\n")
  return(slope)
}
