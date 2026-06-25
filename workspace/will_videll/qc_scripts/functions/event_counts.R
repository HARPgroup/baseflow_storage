### This function determines the monthly total number of events ###

bf_monthly_events_n <- function(event_df, gage_ID){
  gage_total <- n_distinct(event_df$GroupID)
  monthly_event_count <- event_df |>
    mutate(month = month(as.Date(start_date, format = "%y-%m-%d"))) |>
    group_by(month) |>
    summarise(event_cnt = n_distinct(GroupID)) |>
    mutate(gage_total = gage_total)
  
  
  cat(paste0("Monthly event totals saved to 'step08_", gageID, "_eventCount.csv'"), "\n\n")
  return(monthly_event_count)
}