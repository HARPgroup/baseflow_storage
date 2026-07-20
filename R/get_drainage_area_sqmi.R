#'@title get_drainage_area_sqmi
#'@name
#'get_drainage_area_sqmi
#'@description
#'Site drainage area
#'@details
#'Find the drainage area of site based on gageID using dataRetrieval and will
#'use the old or new APIs bsaed on user package version. Deprecated in favor of
#'\code{hydrotools::WaterGageBase$load_sf_da()}
#'@param gage_id character usgs gage number from commandArgs in function call
#'@return numeric, drainage area in sq mi
#'@export
get_drainage_area_sqmi <- function(gage_id) {
  #Based on dataRetrieval pacakge version, use either new or deprecated
  #NWIS functions
  if(utils::packageVersion("dataRetrieval") >= "2.7.23") {
    #New functions return an sf already. so just parse out drainage area
    #for separate field
    gage_data_sf <- dataRetrieval::read_waterdata_monitoring_location(paste0("USGS-",gage_id))
    drainage_area <- gage_data_sf$drainage_area
  }else{
    #NWIS functions return a data frame, so convert to SF using
    #appropriate coordinate fields
    site_info <- dataRetrieval::readNWISsite(gage_id)
    drainage_area <- site_info$drain_area_va
  }

  if (!(is.na(drainage_area))) {
    stop("USGS did not return a valid drainage area for gage_id = ", gage_id)
  }

  area_sqmi <- as.numeric(drainage_area)

  if (is.na(area_sqmi) || area_sqmi <= 0) {
    stop("Invalid drainage area returned for gage_id = ", gage_id)
  }

  return(area_sqmi)
}
