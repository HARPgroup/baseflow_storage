#'@title calc_event_stats
#'@name
#'calc_event_stats
#'@description
#'Remove out-of-range data and calculates log-linear regression of a recession
#'event
#'@details
#'Removes out-of-range data by filtering the flow values in data for days in
#'which the AGWR and dAGWR are within user provided range. Then, this filtered
#'data is passed to \code{bf_event_stats()} to calculate the log-linear
#'regression
#'@param data df with Date, Flow, AGWR, delta_AGWR as minimum present columns.
#'@param event_num Numeric. The groupID of the recession event
#'@param dAGWRmax numeric. The upper limit for the change in AGWR (which is the
#'  ratio of today's and yesterday's flow). Defaults to 1.03
#'@param dAGWRmin numeric. The lower limit for the change in AGWR (which is the
#'  ratio of today's and yesterday's flow). Defaults to 0.97
#'@return data.frame of fitlered data with the recession regression slope and r
#'  squared
#'@export
calc_event_stats <- function (data, event_num, dAGWRmax, dAGWRmin){

sqldf_query <- paste0(
  "select * from data
    where GroupID = ", event_num, " and
    AGWR < 1 and
    delta_AGWR < ", dAGWRmax," and
    delta_AGWR > ", dAGWRmin,"
    ")

event_data <- sqldf::sqldf(sqldf_query)

values <- bf_event_stats(event_data)

new_df <- data.frame(event_data, calc_AGWRC = values$AGWRC, R_squared = values$R_squared)

return(new_df)

}
