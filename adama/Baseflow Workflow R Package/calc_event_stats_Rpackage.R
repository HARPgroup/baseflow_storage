#'@title calc_event_stats
#'@name
#'calc_event_stats
#'@description
#'Creates new df with AGWRC and group R.squared
#'@details
#'Creates a df based on 3 conditions in SQL, being AGWR, dAGWRmax, dAGWRmin. 
#'New df is combined with AGWRC and R.squared from bf_event_stats
#'
#'@param data df with Date, Flow, AGWR, delta_AGWR as minimum present columns. 
#'From locationBF.csv (analysis_df)
#'@param event_num i in 1:max(data$GroupID) (summarize_event), can be set to one specific GroupID if desired
#'@param dAGWRmax num 1 + dAGWR_range, default dAGWR_range is 0.03 from function call script (summarize_event)
#'@param dAGWRmin num 1 - dAGWR_range, default dAGWR_range is 0.03 from function call script (summarize_event)
#'@return new df with data df, AGWRC, and R.squared
#'@author
#'@importFrom sqldf sqldf
#'@export
calc_event_stats <- function (data, event_num, dAGWRmax, dAGWRmin){
require(sqldf)

# source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_event_stats.R")

sqldf_query <- paste0(
  "select * from data 
    where GroupID = ", event_num, " and
    AGWR < 1 and
    delta_AGWR < ", dAGWRmax," and
    delta_AGWR > ", dAGWRmin,"
    ")

event_data <- sqldf(sqldf_query)

values <- bf_event_stats(event_data)

new_df <- data.frame(event_data, calc_AGWRC = values$AGWRC, R_squared = values$R_squared)

return(new_df)

}
