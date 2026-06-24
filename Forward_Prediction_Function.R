#Forward calculation Prediction, with an unknown AGWRC value 
library(tidyverse)
library(patchwork)
library(gt)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/FinalRegression.R")

flow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/01634000-flow.csv")
baseflow_csv <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_01634000.csv")
baseflow_final <- read_csv("https://deq1.bse.vt.edu/usgs/agws/baseflow_summary_df_01634000.csv")

model <- fit_agwrc_regression(baseflow_final)

flow_csv <- flow_csv |>
  select(obs_date, obs_flow) |>
  rename(Date = obs_date, Flow = obs_flow)

FlowResults <- baseflow_csv |>
  group_by(GroupID) |>
  mutate(duration = max(Date) - min(Date)) |>
  mutate(Flow = first(Flow)) |>
  select(GroupID, Date, Flow, duration)

FlowResults <- FlowResults[!duplicated(FlowResults$GroupID), ] |>
  filter(duration >= 15)

accuracy_table <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2")
)

predictionlog <- data.frame(logQ = log(FlowResults$Flow))
FlowResults$lmAGWRC <- predict(model, predictionlog)

days = c(7, 15)

for (d in days) {
  FlowResults[[paste0("proj_", d,"day")]] <- FlowResults$Flow * FlowResults$lmAGWRC^d
  FlowResults[[paste0("observ_", d, "day")]] <- flow_csv$Flow[match(FlowResults$Date + d, flow_csv$Date)]
  FlowResults <- FlowResults[!is.na(FlowResults[[paste0("observ_", d, "day")]]), ]
  FlowResults[[paste0("MAE_", d, "day")]] <- mean(abs(FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]))
  FlowResults[[paste0("RMSE_", d, "day")]] <- sqrt(mean((FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]])^2))
  FlowResults[[paste0("MAPE_", d, "day")]] <- mean(abs((FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]) / FlowResults[[paste0("observ_", d, "day")]])) * 100
  FlowResults[[paste0("Bias_", d, "day")]] <- mean(FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]])
  FlowResults[[paste0("r.squared_", d, "day")]] <- summary(lm(as.formula(paste0("proj_", d, "day", "~", "observ_", d, "day")), data = FlowResults))$r.squared
  FlowResults[[paste0("residuals_", d, "day")]] <- FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]
  
  threshold <- quantile(abs(FlowResults[[paste0("residuals_", d, "day")]]),probs = 0.90,na.rm = TRUE)
  FlowResults[[paste0("flag_", d, "day")]] <-abs(FlowResults[[paste0("residuals_", d, "day")]]) > threshold
  
  cat("\n", d, "day threshold =", threshold, "\n")
  FlowResults %>%
    filter(abs(.data[[paste0("residuals_", d, "day")]]) > threshold) %>%
    select(
      Date,
      all_of(paste0("residuals_", d, "day")),
      all_of(paste0("flag_", d, "day"))
    ) %>%
    print()
  
  # Accuracy table with x columns for all day ranges predicted
  accuracy_table[[paste0("FlowStats_", d, "day")]] <- c(FlowResults[[paste0("MAE_", d, "day")]][1], FlowResults[[paste0("RMSE_", d, "day")]][1], FlowResults[[paste0("MAPE_", d, "day")]][1], FlowResults[[paste0("Bias_", d, "day")]][1],  FlowResults[[paste0("r.squared_", d, "day")]][1])
}
###############################################################################
# Plot 1, scatter plot of Residuals and baseline y = 0
  ggplot(FlowResults, aes(Date, residuals_7day)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(color = "purple", size = 1.3) +
    geom_point(
      data = filter(FlowResults, flag_7day),
      color = "red",
      size = 1.3) +
    labs(title = "Projected vs. Observed Residuals (7- Day Period)")+
    theme_classic()
  
  # Plot 2, scatter plot of Residuals and baseline y = 0
  ggplot(FlowResults, aes(Date, residuals_15day)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(color = "purple", size = 1.3) +
    geom_point(
      data = filter(FlowResults, flag_15day),
      color = "red",
      size = 1.3) +
    labs(title = "Projected vs. Observed Residuals (15- Day Period)")+
    theme_classic()
  
  
######################################################################## 
  # Plot 1, scatter plot of Actual vs. Predicted
  ggplot(FlowResults, aes(x = Date)) +
    geom_point(aes(y = observ_7day, color = "Observed"), size = 1.3) +
    geom_point(aes(y = proj_7day, color = "Projected"), size = 1.3) +
    labs(title = "Projected vs. Observed Flow in Baseflow Events (7-Day Period)", y = "Flow (cfs)", x = "Date", color = "Flow")+
    theme_classic()

  ggplot(FlowResults, aes(x = proj_7day, y = observ_7day)) +
    geom_point(color = "blue", size = 1.3) +
    geom_smooth(method = "lm", se = F, color = "red") +
    labs(title = "Projected vs. Observed Flow in Baseflow Events (7- day Period)", y = "Observed Flow (cfs)", x = "Projected Flow (cfs)") +
    theme_classic()
  
  accuracy_table |>
    gt() |>
    gtsave("accuracy_table.png")
  
# Plot 1 and Plot 2
#p1/p2
#########################################################  
  # threshold <- quantile(
  #   abs(FlowResults$residuals_7day),
  #   probs = 0.95,
  #   na.rm = TRUE
  # )
  # 
  # FlowResults <- FlowResults %>%
  #   mutate(
  #     flag_7day = abs(residuals_7day) > threshold
  #   )
  
############################################################  
  # threshold <- quantile(
  #   abs(FlowResults$residuals_15day),
  #   probs = 0.95,
  #   na.rm = TRUE
  # )
  # 
  # FlowResults <- FlowResults %>%
  #   mutate(
  #     flag_15day = abs(residuals_15day) > threshold
  #   ) 
  
#####################################################
  # FlowResults <- FlowResults %>%
  #   mutate(
  #     pct_error_15day =
  #       100 * abs(proj_15day - observ_15day) / observ_15day
  #   )
  # 
  # threshold <- quantile(
  #   FlowResults$pct_error_15day,
  #   0.95,
  #   na.rm = TRUE
  # )
  # 
  # FlowResults <- FlowResults %>%
  #   mutate(
  #     flag_15day = pct_error_15day > threshold
  #   )
  