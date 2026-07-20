#'@title add_flow_in_day
#'@name
#'add_flow_in_day
#'@description
#'Appends a flow timeseries data frame with a field to represent flow in
#'watershed inches
#'@details
#'Calculates conversion of cfs to watershed inches with \code{convert.flow()}
#'Adds column of watershed in. flow to df
#'@param points_df data.frame with a flow field with a name indicated by the
#'  source_flow_col input
#'@param area_sqmi numeric. Drainage area in sq mi, which may be provided by
#'  \code{hydrotools::WaterGageBase$load_sf_da()}
#'@param source_flow_col character, designating column name for Flow, default
#'  "Flow"
#'@param new_col character designating new column for Flow in watershed inches,
#'  default "flow_in_day"
#'@return data.frame with added column of "new_col" with flow in watershed
#'  inches
#'@export
add_flow_in_day <- function(points_df, area_sqmi, source_flow_col = "Flow",
                            new_col = "flow_in_day") {

  if (!(source_flow_col %in% names(points_df))) {
    stop("Missing source_flow_col: ", source_flow_col)
  }

  if (is.na(area_sqmi) || !is.numeric(area_sqmi) || area_sqmi <= 0) {
    stop("area_sqmi must be a single positive numeric value.")
  }

  points_df[[new_col]] <- convert.flow(points_df[[source_flow_col]], area_sqmi)

  return(points_df)
}
