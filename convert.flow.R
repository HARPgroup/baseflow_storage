# Function to convert flow from cfs to inches using UsGS gage area
convert.flow <- function(flow_col, area_sqmi){
  cfs <- flow_col
  
  # Create conversion factor
  conversion <- (86400*12)/(5280*5280)
  sp_conv <- conversion/area_sqmi
  
  flow_in <- cfs * sp_conv
  
  return(flow_in)
}


