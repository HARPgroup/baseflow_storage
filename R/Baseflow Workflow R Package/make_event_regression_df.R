#'@title make_event_regression_df
#'@name
#'make_event_regression_df
#'@description
#'Consolidates df for baseflow regression
#'@details
#'Converts previous df into more organized df with limited columns. Groups data by GroupID, finds start_date
#'and end_date for all events, calculates median flow for each event and event AGWRC
#'@param point_df df with columns, Date, GroupID, Flow, AGWR, delta_AGWR, AGWRC, kept, met_alpha
#'@param regression_flow_col char string designating column name for regression_flow_col, default "Flow"
#'@return df with GroupID, start_date, end_date, n_days, median_flow, event_AGWRC
#'@author 
#'@export
make_event_regression_df <- function(points_df, regression_flow_col = "Flow") {
  
  required <- c("GroupID", "Date", regression_flow_col, "AGWRC", "kept", "met_alpha")
  missing <- setdiff(required, names(points_df))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  points_df %>%
    mutate(
      Date = as.Date(Date),
      regression_flow = as.numeric(.data[[regression_flow_col]])
    ) %>%
    filter(
      kept == TRUE,
      met_alpha == TRUE,
      !is.na(GroupID),
      !is.na(regression_flow),
      !is.na(AGWRC),
      regression_flow > 0
    ) %>%
    group_by(GroupID) %>%
    summarise(
      start_date = min(Date, na.rm = TRUE),
      end_date = max(Date, na.rm = TRUE),
      n_days = dplyr::n(),
      median_flow = median(regression_flow, na.rm = TRUE),
      event_AGWRC = dplyr::first(AGWRC),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(median_flow),
      !is.na(event_AGWRC),
      median_flow > 0
    ) %>%
    arrange(start_date)
}
