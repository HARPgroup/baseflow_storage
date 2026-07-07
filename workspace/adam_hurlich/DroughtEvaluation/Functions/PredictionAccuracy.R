## Local Testing
#library(tidyverse)
#
#GageID = "01634000" #Strasburg
#flow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/", GageID, "-flow.csv"))
#baseflow_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_trimmed_stats_", GageID, ".csv"))
#regression_csv <- read_csv(paste0("https://deq1.bse.vt.edu/usgs/agws/baseflow_regression_df_", GageID, ".csv"))
#m <- regression_csv$m
#b <- regression_csv$b
#
#flow_csv <- flow_csv |>
#  select(obs_date, obs_flow) |>
#  rename(Date = obs_date, Observed = obs_flow)
#
#baseflow_csv <- baseflow_csv |>
#  group_by(GroupID) |>
#  mutate(duration = as.numeric(max(Date) - min(Date))) |>
#  slice(1) |>
#  ungroup() |>
#  filter(duration >= 15) |>
#  select(GroupID, Date, Flow, AGWRC, duration)

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/adam_bf_workflow/workspace/adam_hurlich/DroughtEvaluation/Functions/forwardForecast.R")

#' @title PredictionAccuracy
#' @name
#'PredictionAccuracy
#'  @description
#'Baseflow workflow event accuracy
#' @details A function to evaluate all identified baseflow events in the baseflow workflow.
#' All events are evaluated using forecasted vs observed flow based on a constant or variable AGWRC.
#' MAE, RMSE, MAPE, Bias and r2 for each event and each day is provided.
#' @param flow_csv df of all daily flow data for USGS gage. Minimum columns: Date, Observed column
#' @param baseflow_csv df of all baseflow events daily flow data, filtered for minimum event duration.
#' Minimum required columns: GroupID, Date, and Flow
#' @param days num vector of desired projected day data
#' @param AGWRC numeric or character. The decay coefficient to regress Q0 for
#'   forecast. May be a single numeric to allow for a constant forecast or a
#'   vector of numeric values to allow for a variable forecast but must be of
#'   length days. Otherwise, may be "lm_constant" to claculate a constant value
#'   from m and b or "lm_variable" to have a variable value
#' @param m numeric of length 1 that is the slope of a log-linear relationship
#'   of Q and AGWRC
#' @param b numeric of length 1 that is the intercept of a log-linear
#'   relationship of Q and AGWRC
#' @return A list with 3 df,
#' event_stats: columns of GroupID, Date, forecast and observed flow, AGWRC, and residuals
#' group_stats: grouped by GroupID, provides MAE, RMSE, MAPE, Bias, R2 for all baseflow events
#' daily_stats: grouped by Day, provides MAE, RMSE, MAPE, Bias, R2 for all Days
#' @importFrom dplyr filter mutate summarise group_by group_modify
#' @importFrom tibble tibble
#' @export
PredictionAccuracy <- function(flow_csv, baseflow_csv, days = c(0:15), AGWRC, m, b) {

  # Remove scientific notation
  options(scipen = 999)

  # Run forwardForecast for all baseflow events
  ResultsFinal <- lapply(1:nrow(baseflow_csv), function(i) {
    # df for forwardForecast function
    out <- forwardForecast(
      Q0 = baseflow_csv$Flow[i],
      days = days,
      AGWRC = AGWRC,
      m = m,
      b = b
    )
    # add Date to forwardForecast df
    out$Date <- baseflow_csv$Date[i] + days
    # add GroupID to forwardForecast df
    out$GroupID <- baseflow_csv$GroupID[i]
    out
  })

  # Merge observed flow by matching Date
  ResultsFinal <- lapply(ResultsFinal, function(df_i) {
    merge(df_i, flow_csv, by = "Date", all.x = T)
  })

  # Create df from list object
  big <- do.call(rbind, Map(cbind, ResultsFinal)) |>
    filter(Day != 0)

  # Calculate residuals
  big$Residuals <- big$Forecast - big$Observed

  # df for overall event_stats, large df with all info
  event_stats <- big %>%
    group_by(GroupID, Day) %>%
    summarise(Date, Forecast, Observed, AGWRC, Residuals, .groups = "drop")

  # df for group_stats by GroupID grouping, getting statistical metrics
  group_stats <- big %>%
    group_by(GroupID) %>%
    group_modify(~ {
      model <- lm(Forecast ~ Observed, data = .x)
      tibble(
        MAE = mean(abs(.x$Residuals), na.rm = TRUE),
        RMSE = sqrt(mean(.x$Residuals^2, na.rm = TRUE)),
        MAPE = mean(abs(.x$Residuals / .x$Observed)) * 100,
        Bias = mean(.x$Residuals, na.rm = TRUE),
        r2 = summary(model)$r.squared
      )
    })

  # df for daily_stats by Day grouping, getting statistical metrics
  daily_stats <- big %>%
    group_by(Day) %>%
    group_modify(~ {
      model <- lm(Forecast ~ Observed, data = .x)
      tibble(
        MAE = mean(abs(.x$Residuals), na.rm = TRUE),
        RMSE = sqrt(mean(.x$Residuals^2, na.rm = TRUE)),
        MAPE = mean(abs(.x$Residuals / .x$Observed)) * 100,
        Bias = mean(.x$Residuals, na.rm = TRUE),
        r2 = summary(model)$r.squared
      )
    })

  return(list(event_stats = event_stats, group_stats = group_stats, daily_stats = daily_stats))
}

## Local Testing
#Predict <- PredictionAccuracy(flow_csv, baseflow_csv, days = c(0:15), AGWRC = "lm_variable", m, b)
#
#df <- Predict$event_stats %>%
#  filter(GroupID == 4, Day %in% 1:15)
#
#df_long <- df %>%
#  select(Day, Observed, Forecast) %>%
#  pivot_longer(
#    cols = c(Observed, Forecast),
#    names_to = "Type",
#    values_to = "Value"
#  )
#
#ggplot(df_long, aes(x = Day, y = Value, color = Type)) +
#  geom_point(size = 1.5) +
#  scale_x_continuous(breaks = 1:15) +
#  labs(
#    title = paste("Observed vs Predicted by Day for ID", 4),
#    x = "Day",
#    y = "Flow"
#  ) +
#  theme_minimal()
