RegressionAGWRC <- function(Flow, m, b) {
  AGWRC <- m * log(Flow) + b
  return(AGWRC)
}

forwardForecast <- function(GageID, start_date, days = 1:90, AGWRC) {
  
  reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
  flow_csv <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
  Q0 <- flow_csv$obs_flow[match(start_date, flow_csv$obs_date)]
  m <- reg_lm$m
  b <- reg_lm$b

  if(length(AGWRC) > 1 & length(AGWRC) != length(days)){
    warning("AGWRC must be of length 1 or of same length as days")
  }

  if(is.character(AGWRC) && AGWRC == "lm_constant"){
    AGWRC <- RegressionAGWRC(Q0, m, b)
  }

  if(is.numeric(AGWRC)){
    if(length(AGWRC) == 1){
      flowForecast <- Q0*AGWRC^days
      
      return(
        data.frame(
          days = days,
          forecast = flowForecast,
          AGWRC = AGWRC
        )
      )
      
    } else if(AGWRC == "lm_variable") {
      
      n <- length(days)
      AGWRCi <- numeric(n + 1)
      Qi <- numeric(n + 1)
      
      AGWRCi[1] <- RegressionAGWRC(Q0, m, b) #InitialValues
      Qi[1] <- Q0
      
      for (i in 1:length(days)) {

        Qi[i+1] <- Qi[i] * AGWRCi[i]
        AGWRCi[i+1] <- RegressionAGWRC(Qi[i+1], m, b)
  
      }
      
      flowForecast <- data.frame(Day = c(0, days), Flow = Qi, AGWRC = AGWRCi)
      return(flowForecast)
    }
  }
}
  
# Local Testing

GageID = "01672500"
start_date = "1990-05-08"
days = 1:90

reg_lm <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
flow_csv <- read.csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
Q0 <- flow_csv$obs_flow[match(start_date, flow_csv$obs_date)]
m <- reg_lm$m
b <- reg_lm$b

Test <- forwardForecast("01672500", "1990-05-08", days = 1:90, AGWRC = "lm_variable")

n <- length(days)
AGWRCi <- numeric(n + 1)
Qi <- numeric(n + 1)

AGWRCi[1] <- RegressionAGWRC(Q0, m, b) #InitialValues
Qi[1] <- Q0

for (i in 1:length(days)) {
  
  Qi[i+1] <- Qi[i] * AGWRCi[i]
  AGWRCi[i+1] <- RegressionAGWRC(Qi[i+1], m, b)
  
}

flowForecast <- data.frame(Day = c(0, days), Flow = Qi, AGWRC = AGWRCi)
return(flowForecast)

