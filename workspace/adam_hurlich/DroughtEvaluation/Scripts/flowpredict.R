library(tidyverse)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/FinalRegression.R")

## Local Testing
#flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/02056000-flow.csv")
#baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_summary_df_02056000.csv")

flowpredict <- function(baseflow_csv, flow_csv, start_date) {
  
  flow_csv <- flow_csv |>
    select(obs_date, obs_flow) |>
    rename(Date = obs_date, Flow = obs_flow)
  
  flow_initial <- flow_csv |>
    filter(Date == as.Date(start_date)) |>
    pull(Flow)
  
  model <- fit_agwrc_regression(baseflow_csv)
  
  predictionlog <- data.frame(logQ = log(flow_initial)) 
  flowAGWRC <- predict(model, predictionlog)[[1]]
  
  days = c(15, 30, 45, 90)
  FlowResults <- data.frame(StartDate = start_date, FlowInitial = flow_initial, AGWRC = flowAGWRC)
  
  for (d in days) {
    FlowResults[[paste0("proj_", d,"day")]] <- flow_initial * flowAGWRC^d
    FlowResults[[paste0("observ_", d, "day")]] <- flow_csv$Flow[match(as.Date(start_date) + d, flow_csv$Date)]
  }
  
  return(FlowResults)
}

## Local Testing
#FlowResults <- flowpredict(baseflow_csv, flow_csv, "2026-04-10")
