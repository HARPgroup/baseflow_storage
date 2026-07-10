#' @title get_basin_slope
#' @name
#' get_basin_slope
#' @description
#' gets average percent slope of basin drainage area
#' @details
#' takes a gage R6 object from step 1 of the DEQ workflow,
#' returns the average percent slope of the basin drainage area
#'
#' @param gage_obj an R6 gage object
#'
#' @returns a numeric value representing percent slope of the basin drainage area
#' @importFrom nhdplusTools get_nldi_feature get_catchment_characteristics
#' @export
get_basin_slope <- function(gage_obj){

  site <- get_nldi_feature(list(featureSource = "nwissite",
                                featureID = paste0("USGS-", gageID)))
  gage_comid <- site$comid

  watershed_slope <- get_catchment_characteristics(
    varname = "TOT_BASIN_SLOPE",
    ids = gage_comid
  )

  slope <- watershed_slope[,3]
  result <- (paste0("The slope is ", slope, "%"))
  cat(result, "\n\n")
  return(slope)
}
