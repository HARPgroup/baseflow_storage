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
forwardForecast <- function(Q0, days = 1:90, AGWRC, m, b) {
  #Check to see if AGWRC is of the appropriate length (must be length 1 or the
  #same as days)
  if(length(AGWRC) > 1 & length(AGWRC) != length(days)){
    warning("AGWRC must be of length 1 or of same length as days")
  }
  #If user requests a calculated constant AGWRC, use the regression coefficients
  #to caluclate
  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- RegressionAGWRC(Q0, m, b)
  }
  #If AGWRC is now numeric (either calculated or input by user), forecast flows
  if(is.numeric(AGWRC)){
    if(length(AGWRC) == 1){
      #If a constant AGWRC, the forecast is simple:
      flowForecast <- Q0*AGWRC^days
    }else{
      #If a variable AGWRC, run a loop for each value in days that iterates
      #based on the time elapsed from on entry in days to the next (in case user
      #enters days = c(7,15, 90)). Use the forecast calculated in the previous
      #loop with the next AGWRC value
      #Initialize a vector for forecasted flows and set a flow iterator Qi to Q0
      flowForecast <- numeric(length(days))
      Qi <- Q0
      for(i in 1:length(days)){
        #Use the first days for the iniital loop, otherwise use time elapsed
        if(i == 1){
          dayi <- days[i]
        }else{
          dayi <- days[i] - days[i-1]
        }
        #Calculate the forecasted flow for this instance using the corresponding
        #AGWRC
        flowForecast[i] <- forwardForecast(Q0 = Qi, days = dayi,
                                           AGWRC = AGWRC[i])$forecast
        #Set flow iterator for next loop to serve as the initial flow for
        #forwardForecast
        Qi <- flowForecast[i]
      }
    }
    
  }else if(AGWRC == "lm_variable"){
    #If a variable calculated AGWRC, run a loop for each value in days that iterates
    #based on the time elapsed from on entry in days to the next (in case user
    #enters days = c(7,15, 90)). Use the calculated AGWRC based on the previous
    #forecasted flow value
    #Initialize a vector for forecasted flows and set a flow iterator Qi to Q0
    #Initialized AGWRC with initial flow
    AGWRCi <- RegressionAGWRC(Q0, m, b)
    Qi <- Q0
    flowForecast <- numeric(length(days))
    AGWRC <- numeric(length(days))
    for(i in 1:length(days)){
      #Use the first days for the iniital loop, otherwise use time elapsed
      if(i == 1){
        dayi <- days[i]
      }else{
        dayi <- days[i] - days[i-1]
      }
      #Store AGWRC iterator
      AGWRC[i] <- AGWRCi
      #Calculate the forecasted flow for this instance using the corresponding
      #AGWRC
      flowForecast[i] <- forwardForecast(Q0 = Qi, days = dayi,
                                         AGWRC = AGWRCi)$forecast
      #Set iterators
      Qi <- flowForecast[i]
      AGWRCi <- RegressionAGWRC(Qi, m, b)
    }
  }
  #Return a list that contains the forecast for each day and the corresponding
  #AGWRC
  return(
    data.frame(
      days = days,
      forecast = flowForecast,
      AGWRC = AGWRC
    )
  )
}  

GageID <- "01672500"
reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
forwardForecast(Q0 = 100, days = 1:90, AGWRC = 0.97)
forwardForecast(Q0 = 100, days = 1:90, AGWRC = rnorm(90, 0.97, 0.001))
forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_constant", m = reg_lm$m, b = reg_lm$b)
forwardForecast(Q0 = 100, days = 1:90, AGWRC = "lm_variable", m = reg_lm$m, b = reg_lm$b)
