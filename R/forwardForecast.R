#'@title RegressionAGWRC
#'@name RegressionAGWRC
#' @details Calculate AGWRC based on a log linear relationship of flow, m,
#' and b e.g. AGWRC = m * log(Flow) + b
#' @param Flow numeric of length 1. Flow to calculate AGWRC from in log-linear
#'   relationship
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @return An AGWR coefficient calculated from the log-linear regression for Flow
#'@export
RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
  return(AGWRC)
}


#'@title regressionLimitAGWRC
#'@name regressionLimitAGWRC
#' @details Calculate AGWRC based on a log linear relationship of flow, m,
#' and b e.g. AGWRC = m * log(Flow) + b
#' @param Flow numeric of length 1. Flow to calculate AGWRC from in log-linear
#'   relationship
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @param low_flow_limit A flow value below which the regression is not
#'   applicable. At this limit, the low_agwrc_limit will be returned. If NULL,
#'   this is not considered and instead the regression is used.
#' @param low_agwrc_limit If the input flow is below the low_flow_limit, this
#'   function will return this value
#' @param high_flow_limit A flow value above which the regression is not
#'   applicable. At this limit, the high_flow_limit will be returned. If NULL,
#'   this is not considered and instead the regression is used.
#' @param high_agwrc_limit If the input flow is above the high_flow_limit, this
#'   function will return this value
#' @return An AGWR coefficient calculated from the log-linear regression for Flow
#'@export regressionLimitAGWRC
regressionLimitAGWRC <- function(Flow, m, b,
                            low_flow_limit = NULL, low_agwrc_limit = NULL,
                            high_flow_limit = NULL, high_agwrc_limit = NULL) {
  #If a lower limit is provided and flow is below that limit, return the lower
  #default agwrc value or warn user if not provided
  if(!is.null(low_flow_limit) && Flow < low_flow_limit){
    if(!is.null(low_agwrc_limit)){
      return(low_agwrc_limit)
    }else{
      warning("No low_agwrc_limit provided but flow of ",Flow,
              "is below low_flow_limit ", low_flow_limit)
    }
  }

  #If a higher limit is provided and flow is above that limit, return the higher
  #default agwrc value or warn user if not provided
  if(!is.null(high_flow_limit) && Flow > high_flow_limit){
    if(!is.null(high_agwrc_limit)){
      return(high_agwrc_limit)
    }else{
      warning("No high_agwrc_limit provided but flow of ",Flow,
              "is above low_flow_limit ", high_flow_limit)
    }
  }

  #If limits don't apply, just calculate the regression as is:
  AGWRC <- RegressionAGWRC(Flow, m, b)
  return(AGWRC)
}

#'@title single_forecast
#'@name single_forecast
#' @details Calculate flow prediction based on initial flow, AGWRC, and days
#'   using the exponential decay function of Q = Q0 * (AGWRC^days)
#' @param Q0 numeric of length 1 for initial flow
#' @param AGWRC numeric of length 1 for AGWRC
#' @param days numeric scalar or vector running calculation up to maximum value in vector
#' @return numeric vector containing forecasted flows
#' @examples
#' single_forecast(Q0 = 100, AGWRC = 0.97, days = 0:10)
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
#'   length days. Otherwise, may be "lm_constant" (calculate a constant AGWRC
#'   based on Q0 from m and b); "lm_variable" (calculate a variable AGWRC from m
#'   and b based on previous day flow). low_flow_limit and high_flow_limit may
#'   be used to limit the initial AGWRC for lm_constant or the entire
#'   lm_variable analysis such that AGWRC is bounded when flow is above or below
#'   the provided limits
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @param low_flow_limit A flow value below which the regression is not
#'   applicable. At this limit, the low_agwrc_limit will be returned. Only
#'   used in the "lm_constant_limit" and "lm_variable_limit" method and will not be
#'   used if NULL.
#' @param low_agwrc_limit If the input flow is below the low_flow_limit, this
#'   function will return this value
#' @param high_flow_limit A flow value above which the regression is not
#'   applicable. At this limit, the high_flow_limit will be returned. If NULL,
#'   this is not considered and instead the regression is used. Only used in the
#'   "lm_constant_limit" and "lm_variable_limit" method and will not be used if
#'   NULL.
#' @param high_agwrc_limit If the input flow is above the high_flow_limit, this
#'   function will return this value
#' @return A data frame containing the days, forecast flows and AGWRCs used in
#'   the forecast
#' @examples
#' GageID <- "02040000"
#' reg_lm <- read.csv(
#'   paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_",
#'   GageID, ".csv")
#' )
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = 0.97)
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = rnorm(90, 0.97, 0.001))
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_constant", m = reg_lm$m, b = reg_lm$b)
#' #Repeat now with limit
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_constant",
#'                 m = reg_lm$m, b = reg_lm$b,
#'                 low_flow_limit = reg_lm$low_Q, low_agwrc_limit = reg_lm$low_Q_agwrc,
#'                 high_flow_limit = reg_lm$high_Q, high_agwrc_limit = reg_lm$high_Q_agwrc)
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_variable", m = reg_lm$m, b = reg_lm$b)
#' #Repeat now with limit
#' forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_variable",
#'                 m = reg_lm$m, b = reg_lm$b,
#'                 low_flow_limit = reg_lm$low_Q, low_agwrc_limit = reg_lm$low_Q_agwrc,
#'                 high_flow_limit = reg_lm$high_Q, high_agwrc_limit = reg_lm$high_Q_agwrc)
#' @export
forwardForecast <- function(Q0, days = 0:90, AGWRC, m, b,
                            low_flow_limit = NULL, low_agwrc_limit = NULL,
                            high_flow_limit = NULL, high_agwrc_limit = NULL){
  # Assignments for future indexing, example 1:91 num vector
  n <- max(days)
  full_data <- 0:n

  # Column size for length n+1, example 1:91 num vector of zeros
  AGWRCi <- numeric(n+1)
  Qi <- numeric(n+1)

  # AGWRC argument must fit if length > 1, must be equal to maximum values in days vector
  if(length(AGWRC) > 1 & length(AGWRC) != length(full_data)){
    warning("AGWRC must be of length 1 or of same length as the full forecast days",
            min(full_data),"to",max(full_data))
  }

  # Check for "lm_constant" argument, run RegressionAGWRC
  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- regressionLimitAGWRC(Flow = Q0, m = m, b = b,
                                  low_flow_limit = low_flow_limit,
                                  low_agwrc_limit = low_agwrc_limit,
                                  high_flow_limit = high_flow_limit,
                                  high_agwrc_limit = high_agwrc_limit)
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
  }else if(is.character(AGWRC) && AGWRC == "lm_variable") {

    # Initial values for AGWRC and Qi in index 1
    AGWRCi[1] <- regressionLimitAGWRC(Flow = Q0, m = m, b = b,
                         low_flow_limit = low_flow_limit,
                         low_agwrc_limit = low_agwrc_limit,
                         high_flow_limit = high_flow_limit,
                         high_agwrc_limit = high_agwrc_limit)

    Qi[1] <- Q0

    # Iterative loop for Qi and AGWRCi
    for (i in 1:n) {
      Qi[i+1] <- single_forecast(Q0 = Qi[i], AGWRC = AGWRCi[i], days = 1)
      AGWRCi[i+1] <- regressionLimitAGWRC(Flow = Qi[i+1], m = m, b = b,
                                          low_flow_limit = low_flow_limit,
                                          low_agwrc_limit = low_agwrc_limit,
                                          high_flow_limit = high_flow_limit,
                                          high_agwrc_limit = high_agwrc_limit)
    }
  }

  # Indexes to match selected non-consecutive days, example days = c(0,7,15) grabs index 1, 8, 16
  idx <- full_data %in% days
  # df of days, forecast flows and AGWRC
  return(data.frame(Day = full_data[idx], Forecast = Qi[idx], AGWRC = AGWRCi[idx]))
}

