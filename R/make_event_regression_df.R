#'@title make_event_regression_df
#'@name
#'make_event_regression_df
#'@description
#'Summarized a data.frame to get median flow of recession events with corresponding AGWRC
#'@details
#'Summarizes a data.frame of recession events to calculate median flow during
#'the event and the corresponding recession coefficient. Only keeps non-flagged
#'data, with flags inidcated by logical values in the fields kept and met_alpha.
#'The input for this function is often provided by \code{agws::trim_event_mk()}
#'or \code{agws::attach_event_stats()}. The output from this function may be
#'passed to \code{agws::fit_agwrc_regression()} for further analysis
#'@param points_df data.frame with fields: Date, GroupID, Flow, AGWR,
#'  AGWRC, kept, met_alpha
#'@param regression_flow_col Character, designating field name for
#'  regression_flow_col, default "Flow"
#'@return data.frame one row per recession event with fields GroupID,
#'  start_date, end_date, n_days, median_flow, event_AGWRC
#'@importFrom rlang .data
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
