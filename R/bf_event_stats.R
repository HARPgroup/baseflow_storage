#'@title bf_event_stats
#'@name
#'bf_event_stats
#'@description
#'Calculate a log-linear regression
#'@details
#'Creates a linear model between flow and date columns in data such as
#'log(flow_col) = f(date_col) Returns the slope coefficient converted to linear
#'space and the corresponding coefficient of determination (R squared)
#'@param data data.frame with two fields identified by flow_col and date_col
#'@param flow_col Character. Field name for y variable in the regression log(y)
#'  ~ x. default "Flow"
#'@param date_col Character. Field name for x variable in the regression log(y)
#'  ~ x. Default "Date"
#'@return List with AGWRC (slope) and R.squared for regression
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
