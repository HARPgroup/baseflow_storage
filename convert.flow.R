# Function to convert flow from cfs to inches using UsGS gage area
convert.flow <- function(flow_col, gageid){
  require(dataRetrieval)
  cfs <- flow_col
  
  # Get drainage area from USGS
  site <- readNWISsite(paste0(gageid))
  area <- site$drain_area_va
  
  # Create conversion factor
  conversion <- (86400*12)/(5280*5280)
  sp_conv <- conversion/area
  
  flow_in <- cfs * sp_conv
  
  return(flow_in)
}


