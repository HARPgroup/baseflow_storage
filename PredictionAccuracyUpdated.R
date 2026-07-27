## Local Testing
basepath <- "/var/www/R"
source("/var/www/R/config.R")
library(tidyverse)
library(flextable)
library(hydrotools)

GageID = "02016000" #Cowpasture
gage_obj <- hydrotools::WaterGageDaily$new(gage_id = GageID)

flow_csv <- gage_obj$gage_data |>
 select(time, value) |>
 rename(Date = time, Observed = value)

var <- gage_obj$baseflow_workflow_data(omsite)

baseflow_csv <- var$trimmed_events_df |>
  mutate(Date = as.Date(Date)) |>
  group_by(GroupID) |>
  mutate(duration = as.numeric(max(Date) - min(Date))) |>
  slice(1) |>
  ungroup() |>
  filter(duration >= 7) |>
  select(GroupID, Date, Flow, AGWRC, duration)

regression_csv <- var$lm_df

m <- regression_csv$m
b <- regression_csv$b

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
#' @importFrom agws forwardForecast
#' @export
PredictionAccuracy <- function(flow_csv, baseflow_csv, days = c(0:15), AGWRC, m, b) {

  # Remove scientific notation
  options(scipen = 999)

  # Run forwardForecast for all baseflow events
  ResultsFinal <- lapply(1:nrow(baseflow_csv), function(i) {
    # df for forwardForecast function
    out <- agws::forwardForecast(
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
  event_stats <- big |>
    group_by(GroupID, Day) |>
    summarise(Date, Forecast, Observed, AGWRC, Residuals, .groups = "drop")

  # df for group_stats by GroupID grouping, getting statistical metrics
  group_stats <- big |>
    group_by(GroupID) |>
    group_modify(~ {
      model <- lm(Forecast ~ Observed, data = .x)
      tibble::tibble(
        MAE = mean(abs(.x$Residuals), na.rm = TRUE),
        RMSE = sqrt(mean(.x$Residuals^2, na.rm = TRUE)),
        MAPE = mean(abs(.x$Residuals / .x$Observed)) * 100,
        Bias = mean(.x$Residuals, na.rm = TRUE),
        r2 = summary(model)$r.squared
      )
    })

  # df for daily_stats by Day grouping, getting statistical metrics
  daily_stats <- big |>
    group_by(Day) |>
    group_modify(~ {
      model <- lm(Forecast ~ Observed, data = .x)
      tibble::tibble(
        MAE = mean(abs(.x$Residuals), na.rm = TRUE),
        RMSE = sqrt(mean(.x$Residuals^2, na.rm = TRUE)),
        MAPE = mean(abs(.x$Residuals / .x$Observed)) * 100,
        Bias = mean(.x$Residuals, na.rm = TRUE),
        r2 = summary(model)$r.squared
      )
    })
  summary_table <- summary_stats(group_stats)

  return(list(event_stats = event_stats, group_stats = group_stats, daily_stats = daily_stats, summary_table = summary_table))
}

#' @title summary_stats
#' @name
#'summary_stats
#'  @description
#'statistical significane of metrics
#' @details creates a flextable object for mean, median, min, max, and std dev of MAE, RMSE, MAPE, Bias and r2
#' @param df df with columns of MAE, RMSE, MAPE, Bias, r2
#' @return flextable object
#' @importFrom flextable flextable
#' @export
summary_stats <- function(df) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required. Please install it using install.packages('flextable')")
  }

  metrics <- c("MAE", "RMSE", "MAPE", "Bias", "r2")

  summary_table <- data.frame(
    Statistic = c("Mean", "Median", "Min", "Max", "Std Dev"),
    sapply(metrics, function(x) {
      c(
        mean(df[[x]], na.rm = T),
        median(df[[x]], na.rm = T),
        min(df[[x]], na.rm = T),
        max(df[[x]], na.rm = T),
        sd(df[[x]], na.rm = T)
      )
    }),
    check.names = F
  )

  ft <- flextable::flextable(summary_table) |>
    flextable::colformat_num(digits = 2) |>
    flextable::theme_vanilla() |>
    flextable::autofit()

  return(ft)
}


## Local Testing
Predict <- PredictionAccuracy(flow_csv, baseflow_csv, days = c(0:15), AGWRC = "lm_constant", m, b)
summary_table <- Predict$summary_table
