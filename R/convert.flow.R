#'@title convert.flow
#'@name
#'convert.flow
#'@description
#'Convert cfs to watershed in.
#'@details
#'Convert cfs to watershed in.
#'@param flow_col num column with flow in cfs
#'@param area_sqmi num variable drainage area in sq mi
#'@return num column with flow in watershed in.
#'@export
convert.flow <- function(flow_col, area_sqmi) {
  cfs <- flow_col

  conversion <- (86400 * 12) / (5280 * 5280)
  sp_conv <- conversion / area_sqmi

  flow_in <- cfs * sp_conv

  return(flow_in)
}
