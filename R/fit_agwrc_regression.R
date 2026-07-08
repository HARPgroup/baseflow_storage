#'@title fit_agwrc_regression
#'@name
#'fit_agwrc_regression
#'@description
#'Runs linear regression of logFlow and eventAGWRC
#'@details
#'Runs linear regression of logFlow and eventAGWRC
#'@param event_df df with GroupID, start_date, end_date, n_days, median_flow, event_AGWRC
#'@return model variable with m, b, m-value, p-value, and R.squared
#'@importFrom rlang .data
#'@export
fit_agwrc_regression <- function(event_df) {

  required <- c("GroupID", "median_flow", "event_AGWRC")
  missing <- setdiff(required, names(event_df))

  if (length(missing) > 0) {
    stop("event_df is missing required columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(event_df) < 2) {
    stop("Need at least 2 valid events to fit regression.")
  }

  reg_df <- event_df |>
    dplyr::mutate(logQ = log(.data$median_flow))

  model <- stats::lm(event_AGWRC ~ logQ, data = reg_df)
  model_summary <- summary(model)

  return(model)
}
