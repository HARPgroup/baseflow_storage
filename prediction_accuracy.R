library(tidyverse)
library(dplyr)
library(gt)

## Compile flow data - Local Test
flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/02056000-flow.csv")
baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_02056000.csv")

# Create function, 4 args, baseflow_csv, flow_csv, prediction days, minimum baseflow days
prediction_accuracy <- function(baseflow_csv, flow_csv, days, filter_min) {
  
  # Clean data arguments
  flow_csv <- flow_csv |>
    select(obs_date, obs_flow) |>
    rename(Date = obs_date, Flow = obs_flow)
  
  baseflow_csv <- baseflow_csv |>
    select(GroupID, Date, Flow, AGWRC)
  
  # Calculate duration, define flow_initial, filter organized df
  FlowResults <- baseflow_csv |>
    group_by(GroupID) |>
    mutate(duration = max(Date) - min(Date)) |>
    mutate(flow_initial = first(Flow)) |>
    select(GroupID, Date, flow_initial, AGWRC, duration)
  
  # Baseflow duration must be >= than filter_min in args
  FlowResults <- FlowResults[!duplicated(FlowResults$GroupID), ] |>
    filter(duration >= filter_min)
  
  accuracy_table <- data.frame(
    Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2")
  )
  
  # Find matching flow results for d in days periods from flow_csv, calculate stats
  for (d in days) {
    FlowResults[[paste0("pred_", d,"day")]] <- FlowResults$flow_initial*FlowResults$AGWRC^d
    FlowResults[[paste0("observ_", d, "day")]] <- flow_csv$Flow[match(FlowResults$Date + d, flow_csv$Date)]
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
Predict <- prediction_accuracy(baseflow_csv, flow_csv, days = c(7,15), filter_min = 15)
FlowResults <- Predict$FlowResults
accuracy_table <- Predict$accuracy_table
#calculate the residuals and add as a column to the flowResults
FlowResults <- FlowResults %>%
  mutate(
    Residual_7day = pred_7day - observ_7day,
    Residual_15day = pred_15day - observ_15day)

# Plot 1, scatter plot of Actual vs. Predicted
ggplot(FlowResults, aes(x = Date)) +
  geom_point(aes(y = observ_7day, color = "Observed"), size = 1.3) +
  geom_point(aes(y = pred_7day, color = "Predicted"), size = 1.3) +
  labs(title = "Predicted vs. Actual Flow in Baseflow Events (7-Day Period)", y = "Flow (cfs)", x = "Date", color = "Flow")+
  theme_classic()

# Plot 1.2, scatter plot of Actual vs. Predicted
ggplot(FlowResults, aes(x = Date)) +
  geom_point(aes(y = observ_15day, color = "Observed"), size = 1.3) +
  geom_point(aes(y = pred_15day, color = "Predicted"), size = 1.3) +
  labs(title = "Predicted vs. Actual Flow in Baseflow Events (15-Day Period)", y = "Flow (cfs)", x = "Date", color = "Flow")+
  theme_classic()

# Plot 2, scatter plot of Residuals and baseline y = 0
 ggplot(FlowResults, aes(x = Date, Residual_7day)) +
  geom_hline(yintercept = 0, color ="black") +
  geom_point(color = "purple", size = 1.3) +
  labs(title = "Predicted vs. Actual Residuals (7-Day Period)", x = "Date", y = "Predicted vs. Actual (cfs)") +
  theme_classic()
 
# Plot 2.2, scatter plot of Residuals and baseline y = 0
 ggplot(FlowResults, aes(x = Date, Residual_15day)) +
   geom_hline(yintercept = 0, color ="black") +
   geom_point(color = "purple", size = 1.3) +
   labs(title = "Predicted vs. Actual Residuals (15-Day Period)", x = "Date", y = "Predicted vs. Actual (cfs)") +
   theme_classic()
 
 accuracy_table |>
   gt() |>
   gtsave("accuracy_table.png")
 

# Plot 1 and Plot 2
p1/p2