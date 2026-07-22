#'@title fit_agwrc_regression
#'@name
#'fit_agwrc_regression
#'@description
#'Runs linear regression of log(Flow) and eventAGWRC
#'@details
#'Runs linear regression of log(Flow) and eventAGWRC from output of
#'\code{agws::make_event_regression_df()} to provide insight if recession decay
#'coefficients are related to flow levels (e.g. groundwater storage levels)
#'@param event_df data.frame with GroupID, median_flow, event_AGWRC
#'@return lm object with m, b, m-value, p-value, and R.squared
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

  return(model)
}
