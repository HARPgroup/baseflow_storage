calc_event_stats <- function (data, event_num, dAGWRmax, dAGWRmin){
require(sqldf)
  
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_event_stats.R")
  
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
