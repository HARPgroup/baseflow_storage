#'@title make_event_regression_df
#'@name
#'make_event_regression_df
#'@description
#'Consolidates df for baseflow regression
#'@details
#'Converts previous df into more organized df with limited columns. Groups data by GroupID, finds start_date
#'and end_date for all events, calculates median flow for each event and event AGWRC
#'@param points_df df with columns, Date, GroupID, Flow, AGWR, delta_AGWR, AGWRC, kept, met_alpha
#'@param regression_flow_col char string designating column name for regression_flow_col, default "Flow"
#'@return df with GroupID, start_date, end_date, n_days, median_flow, event_AGWRC
#'@importFrom rlang .data
#'@importFrom stats median
#'@export
make_event_regression_df <- function(points_df, regression_flow_col = "Flow") {

  required <- c("GroupID", "Date", regression_flow_col, "AGWRC", "kept", "met_alpha")
  missing <- setdiff(required, names(points_df))

  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  out <- points_df |>
    dplyr::mutate(
      Date = as.Date(.data$Date),
      regression_flow = as.numeric(.data[[regression_flow_col]])
    ) |>
    dplyr::filter(
      .data$kept == TRUE,
      .data$met_alpha == TRUE,
      !is.na(.data$GroupID),
      !is.na(.data$regression_flow),
      !is.na(.data$AGWRC),
      .data$regression_flow > 0
    ) |>
    dplyr::group_by(.data$GroupID) |>
    dplyr::summarise(
      start_date = min(.data$Date, na.rm = TRUE),
      end_date = max(.data$Date, na.rm = TRUE),
      n_days = dplyr::n(),
      median_flow = stats::median(.data$regression_flow, na.rm = TRUE),
      event_AGWRC = dplyr::first(.data$AGWRC),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !is.na(.data$median_flow),
      !is.na(.data$event_AGWRC),
      .data$median_flow > 0
    ) |>
    dplyr::arrange(.data$start_date)
  return(out)
}
