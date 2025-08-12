attach_event_stats <- function(analysis_data, r_lim =0){
  require(sqldf)
  source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/summarize_event.R")
  analysis_data <- analysis_data
  event_stats <- summarize_event(analysis_data)
  
  combined_data <- sqldf(sprintf(
    "select a.*, b.AGWR as calc_AGWR, b.R_squared from analysis_data as a
    left outer join event_stats as b
    on a.GroupID = b.i
    where R_squared > '%f'
    ", r_lim)
  )
  
  return(combined_data)
}