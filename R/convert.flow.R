#'@title convert.flow
#'@name
#'convert.flow
#'@description
#'Convert flow in cfs to watershed inches
#'@details
#'Convert a vector of flows in cfs to watershed inches using drainage area
#'@param flow_col numeric vector with flow in cfs
#'@param area_sqmi numeric of length one with drainage area in sq mi
#'@return numeric vector flow in watershed inches
#'@export
convert.flow <- function(flow_col, area_sqmi) {
  cfs <- flow_col

  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / area_sqmi

  flow_in <- cfs * sp_conv

  return(flow_in)
}
