### This function calculates the slope ###

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
