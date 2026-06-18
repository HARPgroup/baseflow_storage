### This function determines the gage length ###

event_df <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")

gage_length <- function(event_df){
  paste0(
    "This gage has data from ", 
    min(event_df$start_date),
    " to " ,
    max(event_df$end_date)
  )
}
gage_length(event_df)
