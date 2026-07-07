#'@title bf_event_stats
#'@name
#'bf_event_stats
#'@description
#'Linear model of flow data
#'@details
#'Creates a linear model between flow and date columns in df.
#'Pulls AGWRC and R.squared values
#'
#'@param data df with Flow, Date columns
#'@param flow_col Set column name for flow_col, default "Flow"
#'@param date_col Set column name for date_col, default "Date"
#'@return List with AGWRC and R.squared for all elements
#'@export
bf_event_stats <- function(data, flow_col="Flow", date_col="Date"){

  data[[date_col]]<-as.Date(data[[date_col]])

  # Create lm of data
  logFlow_lm <- stats::lm(log(data[[flow_col]]) ~ data[[date_col]])
  event_sum <- summary(logFlow_lm)

  # Assign AWGRC and R-squared
  AGWRC <- exp(event_sum$coefficients[[2,1]])
  R_squared <- event_sum$r.squared

  return(list(AGWRC = AGWRC, R_squared = R_squared))

}
