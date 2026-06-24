#'@title attach_event_stats
#'@name
#'attach_event_stats
#'@description
#'Creates new binded df between input df and function df
#'@details
#'Joins analysis_df (locationBF.csv) with function df (summarize_event), uses SQL query
#'to filter for equal GroupID and Date, then filters out R.squared values less than 0
#'
#'@param analysis_data df with Date, Flow, AGWR, delta_AGWR as minimum present columns.
#'From locationBF.csv (analysis_df)
#'@param r_lim num variable setting R.squared limit, default 0
#'@return df with site_no, Date, Flow, AGWR, delta_AGWR, Year, Month, 
#'Day, season, GroupID, calc_AGWRC, R_squared (locationstats.csv)
#'@author 
#'@importFrom sqldf sqldf
#'@export
attach_event_stats <- function(analysis_data, r_lim =0){
  require(sqldf)
  # source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/summarize_event.R")
  analysis_data <- analysis_data
  event_stats <- summarize_event(analysis_data)
  
  combined_data <- sqldf(sprintf(
    "select a.*, b.AGWR as calc_AGWR, b.R_squared
    from analysis_data as a
    left outer join event_stats as b
    on a.GroupID = b.GroupID
    AND a.Date = b.Date
    where b.R_squared > '%f'
    ", r_lim)
  )
  
  return(combined_data)
}
