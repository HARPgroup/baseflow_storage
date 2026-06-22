#' @details Calculate AGWRC based on a log linear relationship of flow, m,
#' and b
#' @param Flow numeric of length 1. Flow to calculate AGWRC from in log-linear
#'   relationship
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @return A list containing the forcast flows and AGWRCs used in the forecast
RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
  return(AGWRC)
}

#' @details A function to calculate a forcast days in the future based on Q0. Allows for
#'vector inputs of days and AGWRC. Alternatively, allows for calculation of AGWRC
#'via AGWRC = "lm_constant" or AGWRC = "lm_variable"
#' @param Q0 numeric of length 1. initial flow on day 1
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
#' @return A list containing the forcast flows and AGWRCs used in the forecast
forwardForecast <- function(Q0, days = 0:90, AGWRC, m, b) {

  # Import flow data, get Q0, m and b from gage
  n <- max(days)
  full_data <- 0:n

  # Column size for length n
  AGWRCi <- numeric(n+1)
  Qi <- numeric(n+1)

  ## AGWRC argument must fit
  #if(length(AGWRC) > 1 & length(AGWRC) != length(days)){
  #  warning("AGWRC must be of length 1 or of same length as days")
  #}

  # Check for "lm_constant" argument
  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- RegressionAGWRC(Q0, m, b)
  }

  # AGWRC numeric and length 1, run loop
  if(is.numeric(AGWRC)){
    if(length(AGWRC) == 1){

      # Assignments
      AGWRCi[] <- AGWRC
      Qi[1] <- Q0

    # Calculate flow forecast for i
    for (i in 1:n){
      Qi[i+1] <- Q0 * AGWRC^i
      }
    }
  }

  if(is.numeric(AGWRC)) {
    if(length(AGWRC) > 1){

      AGWRCi <- AGWRC
      Qi[1] <- Q0

      for (i in 1:n){
        Qi[i+1] <- Qi[i] * AGWRCi[i]
      }
    }
  }

  # AGWRC = "lm_variable" argument
  if(is.character(AGWRC) && AGWRC == "lm_variable") {

    # Initial values for AGWRC and Qi
    AGWRCi[1] <- RegressionAGWRC(Q0, m, b) #InitialValues
    Qi[1] <- Q0

    # Iterative loop for Qi and AGWRCi
    for (i in 1:n) {
      Qi[i+1] <- Qi[i] * AGWRCi[i]
      AGWRCi[i+1] <- RegressionAGWRC(Qi[i+1], m, b)
    }
  }

  idx <- full_data %in% days
  return(data.frame(Day = full_data[idx], Forecast = Qi[idx], AGWRC = AGWRCi[idx]))
}

## Local Testing
#GageID = "01672500"
#reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
#m <- reg_lm$m
#b <- reg_lm$b
#Q0 <- 100
#days = c(0,7,15,30,45,60,75,90)
#AGWRC = rnorm(max(days), 0.97, 0.001)
#
#Test <- forwardForecast(Q0, days, AGWRC, m, b)
