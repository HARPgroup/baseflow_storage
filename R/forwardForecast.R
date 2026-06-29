#'@title RegressionAGWRC
#'@name RegressionAGWRC
#' @details Calculate AGWRC based on a log linear relationship of flow, m,
#' and b
#' @param Flow numeric of length 1. Flow to calculate AGWRC from in log-linear
#'   relationship
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @return A list containing the forcast flows and AGWRCs used in the forecast
#'@export
RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
  return(AGWRC)
}

#'@title single_forecast
#'@name single_forecast
#' @details Calculate flow prediction based on initial flow, AGWRC, and days
#' @param Q0 numeric of length 1 for initial flow
#' @param AGWRC numeric scalar or vector for AGWRC
#' @param days numeric scalar or vector running calculation up to maximum value in vector
#' @return numeric column containing forecast flows
#' @export
single_forecast <- function(Q0, AGWRC, days){
  Qout <- Q0*AGWRC^days
  return(Qout)
}

#'@title forwardForecast
#'@name forwardForecast
#' @details A function to calculate a forcast days in the future based on Q0. Allows for
#'vector inputs of days and AGWRC. Alternatively, allows for calculation of AGWRC
#'via AGWRC = "lm_constant" or AGWRC = "lm_variable"
#' @param Q0 numeric of length 1. initial flow on day 0
#' @param days numeric vector. Days where forcast should be calculated and will
#'   use corresponding AGWRC if provided vector. Will use a variable calculated
#'   AGWRC if AGWRC = "lm_variable"
#' @param AGWRC numeric or character. The decay coefficient to regress Q0 for
#'   forecast. May be a single numeric to allow for a constant forecast or a
#'   vector of numeric values to allow for a variable forecast but must be of
#'   length days. Otherwise, may be "lm_constant" to claculate a constant value
#'   from m and b or "lm_variable" to have a variable value
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @return A data frame containing the days, forcast flows and AGWRCs used in the forecast
#' @export
forwardForecast <- function(Q0, days = 0:90, AGWRC, m, b) {

  # Assignments for future indexing, example 1:91 num vector
  n <- max(days)
  full_data <- 0:n

  # Column size for length n+1, example 1:91 num vector of zeros
  AGWRCi <- numeric(n+1)
  Qi <- numeric(n+1)

  # AGWRC argument must fit if length > 1, must be equal to maximum values in days vector
  if(length(AGWRC) > 1 & length(AGWRC) != max(days)){
    warning("AGWRC must be of length 1 or of same length as maximum days")
  }

  # Check for "lm_constant" argument, run RegressionAGWRC
  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- RegressionAGWRC(Q0, m, b)
  }

  if(is.numeric(AGWRC)){
    if(length(AGWRC) == 1){
      # Assignments, AGWRCi is copy of AGWRC, Qi[1] is day 0 initial flow
      AGWRCi <- rep(AGWRC, length(AGWRCi))
      # Calculate flow forecast for i, example days 0:90 with 91 flow values
      Qi <- single_forecast(Q0 = Q0, AGWRC = AGWRC, days = 0:n)
    }else if(length(AGWRC) > 1){
      # AGWRC numeric and length > 1, run loop
      # AGWRCI is copy of AGWRC, Qi[1] is day 0 initial flow
      AGWRCi <- AGWRC
      Qi[1] <- Q0

      # Calculate flow forecast for i by i, Qi and AGWRCi change each i
      for (i in 1:n){
        Qi[i+1] <- single_forecast(Q0 = Qi[i], AGWRC = AGWRCi[i], days = 1)
      }
    }
  }

  # AGWRC = "lm_variable" argument
  if(is.character(AGWRC) && AGWRC == "lm_variable") {

    # Initial values for AGWRC and Qi in index 1
    AGWRCi[1] <- RegressionAGWRC(Q0, m, b)
    Qi[1] <- Q0

    # Iterative loop for Qi and AGWRCi
    for (i in 1:n) {
      Qi[i+1] <- single_forecast(Q0 = Qi[i], AGWRC = AGWRCi[i], days = 1)
      AGWRCi[i+1] <- RegressionAGWRC(Qi[i+1], m, b)
    }
  }

  # Indexes to match selected non-consecutive days, example days = c(0,7,15) grabs index 1, 8, 16
  idx <- full_data %in% days
  # df of days, forecast flows and AGWRC
  return(data.frame(Day = full_data[idx], Forecast = Qi[idx], AGWRC = AGWRCi[idx]))
}

# Local Testing
#GageID = "01672500"
#reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
#m <- reg_lm$m
#b <- reg_lm$b
#Q0 <- 100
#days = c(0:90)
#AGWRC = rnorm(90, 0.97, 0.001)
#
#Test <- forwardForecast(Q0, days, AGWRC = "lm_variable", m, b)
