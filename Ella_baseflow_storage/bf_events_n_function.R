### This function determines the total number of events ###

event_df <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")

bf_events_n <- function(event_df){
  paste0("This gage has ", n_distinct(event_df$GroupID), " recorded events")
}
bf_events_n(event_df)