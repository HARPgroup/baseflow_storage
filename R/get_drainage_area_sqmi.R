#'@title get_drainage_area_sqmi
#'@name
#'get_drainage_area_sqmi
#'@description
#'Site drainage area
#'@details
#'Find the drainage area of site based on gageID using hydrotools
#'@param gage_id char usgs gage number from commandArgs in function call
#'@return num variable with drainage area in sq mi
#'@importFrom hydrotools WaterGageBase
#'@export
get_drainage_area_sqmi <- function(gage_id) {
  gage_obj <- hydrotools::WaterGageBase$new(gage_id = gage_id)
  gage_obj$load_sf_da()

  if (!(is.na(gage_obj$drainage_area))) {
    stop("WaterGageBase$load_sf_da() did not return drain_area_va for gage_id = ", gage_id)
  }

  area_sqmi <- suppressWarnings(as.numeric(gage_obj$drainage_area))

  if (is.na(area_sqmi) || area_sqmi <= 0) {
    stop("Invalid drainage area returned for gage_id = ", gage_id)
  }

  return(area_sqmi)
}
