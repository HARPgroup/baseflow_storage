### This function determines the monthly total number of events ### 
library(tidyverse)

event_df <- read_csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")

bf_monthly_events_n <- function(event_df){
  event_df |>
  mutate(
    month = month(as.Date(start_date, format = "%y-%m-%d"))
  ) |>
    group_by(month) |>
    summarise(event_cnt = n_distinct(GroupID))
}
bf_monthly_events_n(event_df)
