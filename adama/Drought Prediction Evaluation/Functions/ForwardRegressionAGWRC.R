library(tidyverse)

GageID = "02056000"

source("C:/HARP/HARP - GitHub/baseflow_storage/adama/Drought Prediction Evaluation/Functions/RegressionAGWRC.R")

ForwardRegressionAGWRC <- function(GageID, days = c(7,15)) {

  flow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
  baseflow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_", GageID, ".csv"))
  regression_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))

  flow_csv <- flow_csv |>
    select(obs_date, obs_flow) |>
    rename(Date = obs_date, Flow = obs_flow)

  FlowResults <- baseflow_csv |>
    group_by(GroupID) |>
    mutate(duration = as.numeric(max(Date) - min(Date))) |>
    slice(1) |>
    ungroup() |>
    filter(duration >= 15) |>
    select(GroupID, Date, Flow, AGWRC, duration)

  accuracy_table <- data.frame(
    Metric = c("MAE", "RMSE", "MAPE", "Bias", "R^2"))

  FlowResults$lmAGWRC <- RegressionAGWRC(FlowResults$Flow, regression_csv$m, regression_csv$b)

  for (d in days) {
    FlowResults[[paste0("proj_", d,"day")]] <- FlowResults$Flow*FlowResults$lmAGWRC^d
    FlowResults[[paste0("observ_", d, "day")]] <- flow_csv$Flow[match(FlowResults$Date + d, flow_csv$Date)]
    FlowResults <- FlowResults[!is.na(FlowResults[[paste0("observ_", d, "day")]]), ]

    Difference <- FlowResults[[paste0("proj_", d, "day")]] - FlowResults[[paste0("observ_", d, "day")]]

    MAE <- mean(abs(Difference))
    RMSE <- sqrt(mean(Difference^2))
    MAPE <- mean(abs(Difference / FlowResults[[paste0("observ_", d, "day")]])) * 100
    Bias <- mean(Difference)
    R.squared <- summary(lm(as.formula(paste0("proj_", d, "day", "~", "observ_", d, "day")), data = FlowResults))$r.squared
    FlowResults[[paste0("residuals_", d, "day")]] <- Difference

    accuracy_table[[paste0("FlowStats_", d, "day")]] <- c(MAE, RMSE, MAPE, Bias, R.squared)
  }

  return(list(FlowResults = FlowResults, accuracy_table = accuracy_table))
}

Flow <- ForwardRegressionAGWRC("02056000")
