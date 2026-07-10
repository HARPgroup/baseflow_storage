#' @title bf_monthly_events_n
#' @name
#' bf_monthly_events_n
#' @description
#' counts total and monthly number of baseflow events at a given gage
#' @details
#' creates a df with total number of bf events,
#' df also includes total bf events by month
#'
#' @param event_df df from step 06 of DEQ bf workflow
#' @param gage_ID str variable associated with a USGS gage location
#'
#' @returns df with columns for month, event_cnt, and gage_total
#'
#' importFrom dplyr n_distinct mutate group_by summarise
#' @export
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
