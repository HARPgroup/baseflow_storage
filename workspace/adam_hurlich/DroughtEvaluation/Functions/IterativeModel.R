library(tidyverse)
library(patchwork)
library(gt)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/FinalRegression.R")

flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/02056000-flow.csv")
baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_02056000.csv")
baseflow_final <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_summary_df_02056000.csv")

model <- fit_agwrc_regression(baseflow_final)

flow_csv <- flow_csv |>
  select(obs_date, obs_flow) |>
  rename(Date = obs_date, Flow = obs_flow)

baseflow_csv <- baseflow_csv |>
  select(GroupID, Date, Flow, AGWRC)

FlowResults <- baseflow_csv |>
  group_by(GroupID) |>
  mutate(Flow = last(Flow)) |>
  mutate(logQ = log(Flow)) |>
  select(GroupID, Date, Flow, logQ)

FlowResults <- FlowResults[!duplicated(FlowResults$GroupID, fromLast = TRUE), ]

accuracy_table <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2")
)

iterate_model <- function(model, FlowResults, days = 7) {
  
  for (d in 1:days) {
    
    FlowResults[[paste0("pred_", d)]] <- NA_real_
    
    for (i in 1:nrow(FlowResults)) {
      
      if (d == 1) {
        Flow_i <- FlowResults$Flow[i]
      } else {
        Flow_i <- FlowResults[[paste0("pred_", d-1)]][i]
      }
      
      AGWRC_i <- predict(model, FlowResults)
      
      FlowResults[[output_col]][i] <- AGWRC_i * flow_i
      
    }
  }
  
  return(FlowResults)
}

FlowResults <- iterate_model(model, FlowResults, days = 7)

