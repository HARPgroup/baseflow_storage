#'@title add_flow_in_day
#'@name
#'add_flow_in_day
#'@description
#'Adds watershed in. column
#'@details
#'Calculates conversion of cfs to watershed in. with convert.flow function
#'Adds column of watershed in. flow to df
#'@param points_df df with columns: Date, GroupID, Flow, AGWR, delta_AGWR, AGWRC, kept, met_alpha
#'@param area_sqmi num variable with drainage area in sq mi
#'@param source_flow_col char string designating column name for Flow, default "Flow"
#'@param new_col char string designating new column for Flow in watershed in., default "flow_in_day"
#'@return point_df df with added column of "flow_in_day"
#'@author
#'@export
add_flow_in_day <- function(points_df, area_sqmi, source_flow_col = "Flow", new_col = "flow_in_day") {

  if (!(source_flow_col %in% names(points_df))) {
    stop("Missing source_flow_col: ", source_flow_col)
  }

  if (is.na(area_sqmi) || !is.numeric(area_sqmi) || area_sqmi <= 0) {
    stop("area_sqmi must be a single positive numeric value.")
  }

  points_df[[new_col]] <- convert.flow(points_df[[source_flow_col]], area_sqmi)

  return(points_df)
}
