RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
  return(AGWRC)
}

forwardForecast <- function(GageID, start_date, days = 1:90, AGWRC) {
  
  # Import flow data, get Q0, m and b from gage
  reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
  flow_csv <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
  Q0 <- flow_csv$obs_flow[match(start_date, flow_csv$obs_date)]
  n <- length(days)
  m <- reg_lm$m
  b <- reg_lm$b
  
  # Column size for length n
  AGWRCi <- numeric(n + 1)
  Qi <- numeric(n + 1)
  
  # AGWRC argument must fit
  if(length(AGWRC) > 1 & length(AGWRC) != length(days)){
    warning("AGWRC must be of length 1 or of same length as days")
  }
  
  # Check for "lm_constant" argument
  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- RegressionAGWRC(Q0, m, b)[1]
  }

  # AGWRC numeric and length 1, run loop
  if(is.numeric(AGWRC)){
    if(length(AGWRC) == 1){
      
      # Assignments
      AGWRCi <- AGWRC
      Qi[1] <- Q0
      
    # Calculate flow forecast for i
    for (i in 1:n){
      Qi[i+1] <- Q0 * AGWRC^i
    }
      
      return(data.frame(Day = c(0, days), Forecast = Qi, AGWRC = AGWRCi))
    }
  }
  
  # AGWRC = "lm_variable" argument  
  if(AGWRC == "lm_variable") {
    
    # Initial values for AGWRC and Qi  
    AGWRCi[1] <- RegressionAGWRC(Q0, m, b) #InitialValues
    Qi[1] <- Q0
      
    # Iterative loop for Qi and AGWRCi
    for (i in 1:n) {
      Qi[i+1] <- Qi[i] * AGWRCi[i]
      AGWRCi[i+1] <- RegressionAGWRC(Qi[i+1], m, b)
    }
      
    return(data.frame(Day = c(0, days), Forecast = Qi, AGWRC = AGWRCi))
  }
}
  
## Local Testing
#Test <- forwardForecast("01672500", "1990-05-08", days = 1:90, AGWRC = "lm_variable")
