## Drought Prediction Evaluation -- Backwards Analysis

library(tidyverse)
library(patchwork)
library(gt)

# Compile flow data
flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/01634000-flow.csv")
baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_01634000.csv")

back_prediction_accuracy <- function(baseflow_csv, flow_csv, days, filter_min) {
  
flow_csv <- flow_csv |>
  select(obs_date, obs_flow) |>
  rename(Date = obs_date, Flow = obs_flow)

baseflow_csv <- baseflow_csv |>
  select(GroupID, Date, Flow, AGWRC)

FlowResults <- baseflow_csv |>
  group_by(GroupID) |>
  mutate(duration = max(Date) - min(Date)) |>
  mutate(flow_final = last(Flow)) |>
  select(GroupID, Date, flow_final, AGWRC, duration)

FlowResults <- FlowResults[!duplicated(FlowResults$GroupID, fromLast = TRUE), ] |>
  filter(duration >= filter_min)

accuracy_table <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2")
)

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

p1 <- ggplot(FlowResults, aes(x = Date)) +
  geom_point(aes(y = observ_7day, color = "Observed"), size = 1.3) +
  geom_point(aes(y = pred_7day, color = "Predicted"), size = 1.3) +
  labs(title = "Predicted vs. Observed Flow in Baseflow Events (7-Day Period)", y = "Flow (cfs)", x = "Date", color = "Flow")+
  theme_classic()

# Calculate Residuals
Residual_7day = FlowResults$pred_7day - FlowResults$observ_7day

# Plot 2, scatter plot of Residuals and baseline y = 0
p2 <- ggplot(FlowResults, aes(Date, Residual_7day)) +
  geom_hline(yintercept = 0, color ="black") +
  geom_point(color = "purple", size = 1.3) +
  labs(title = "Predicted vs. Observed Residuals (7-Day Period)", x = "Date", y = "Predicted vs. Observed (cfs)") +
  theme_classic()

# Plot 1 and Plot 2
p1/p2

# Export Summary Table
accuracy_table |>
  gt() |>
  gtsave("BackwardsPredictionStrasburg.png")
