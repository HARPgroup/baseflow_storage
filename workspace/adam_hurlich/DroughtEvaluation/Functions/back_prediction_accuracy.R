library(tidyverse)

## Compile flow data - Local Test
#flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/02056000-flow.csv")
#baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_02056000.csv")

# Create function, 4 args, baseflow_csv, flow_csv, prediction days, minimum baseflow days
back_prediction_accuracy <- function(baseflow_csv, flow_csv, days, filter_min) {
  
  # Clean data arguments
  flow_csv <- flow_csv |>
    select(obs_date, obs_flow) |>
    rename(Date = obs_date, Flow = obs_flow)
  
  baseflow_csv <- baseflow_csv |>
    select(GroupID, Date, Flow, AGWRC)
  
  # Calculate duration, define flow_final, filter organized df
  FlowResults <- baseflow_csv |>
    group_by(GroupID) |>
    mutate(duration = max(Date) - min(Date)) |>
    mutate(flow_final = last(Flow)) |>
    select(GroupID, Date, flow_final, AGWRC, duration)
  
  # Baseflow duration must be >= than filter_min in args
  FlowResults <- FlowResults[!duplicated(FlowResults$GroupID, fromLast = TRUE), ] |>
    filter(duration >= filter_min)
  
  accuracy_table <- data.frame(
    Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2")
  )
  
  # Find matching flow results for d in days periods from flow_csv, calculate stats
  for (d in days) {
    FlowResults[[paste0("pred_", d,"day")]] <- FlowResults$flow_final*FlowResults$AGWRC^-d
    FlowResults[[paste0("observ_", d, "day")]] <- flow_csv$Flow[match(FlowResults$Date - d, flow_csv$Date)]
    FlowResults[[paste0("MAE_", d, "day")]] <- mean(abs(FlowResults[[paste0("pred_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]))
    FlowResults[[paste0("RMSE_", d, "day")]] <- sqrt(mean((FlowResults[[paste0("pred_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]])^2))
    FlowResults[[paste0("MAPE_", d, "day")]] <- mean(abs((FlowResults[[paste0("pred_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]) / FlowResults[[paste0("observ_", d, "day")]])) * 100
    FlowResults[[paste0("Bias_", d, "day")]] <- mean(FlowResults[[paste0("pred_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]])
    FlowResults[[paste0("r.squared_", d, "day")]] <- summary(lm(as.formula(paste0("pred_", d, "day", "~", "observ_", d, "day")), data = FlowResults))$r.squared
    
    # Accuracy table with x columns for all day ranges predicted
    accuracy_table[[paste0("FlowStats_", d, "day")]] <- c(FlowResults[[paste0("MAE_", d, "day")]][1], FlowResults[[paste0("RMSE_", d, "day")]][1], FlowResults[[paste0("MAPE_", d, "day")]][1], FlowResults[[paste0("Bias_", d, "day")]][1],  FlowResults[[paste0("r.squared_", d, "day")]][1])
  }
  
  return(list(accuracy_table = accuracy_table, FlowResults = FlowResults))
  
}

## Function call - Local Test w/ Sample Inputs
#Predict <- prediction_accuracy(baseflow_csv, flow_csv, days = c(7,15), filter_min = 15)