#'@title multi_regression_curve
#'@name
#'multi_regression_curve
#'@description
#'Multiple gage regression curve
#'@details
#'Develops a multi-gage regression curve based on inputs of
#'USGS gage objects via Hydrotools. Provides m and b values of best fit
#'@param df input of USGS gage environments with drainage area, daily flow data, and baseflow workflow df
#'  \code{hydrotools::WaterGageDaily$new()}
#'@return data.frame of m and b values of regression curve and printed ggplot scatter plot
#'@examples
#' \dontrun{
#'library(hydrotools)
#'library(tidyverse)
#'basepath <- "/var/www/R"
#'source("/var/www/R/config.R")
#'
#'FrontRoyal <- hydrotools::WaterGageDaily$new(gage_id = '01631000', ds_in = ds)
#'Strasburg <- hydrotools::WaterGageDaily$new(gage_id = '01634000', ds_in = ds)
#'
#'df <- multi_regression_curve(df = list(Strasburg, FrontRoyal))
#'#' }
#'@importFrom dplyr mutate
#'@importFrom ggplot2 ggplot geom_point geom_ribbon geom_line labs theme_minimal
#'@export
multi_regression_curve <- function(
  df) {

  df_list <- lapply(seq_along(df), function(i) {
    obj <- df[[i]]

    data <- obj$baseflow_workflow_data(omsite)$event_summary_df

    data <- data |>
      dplyr::mutate(norm_flow = median_flow / obj$drainage_area) |>
      dplyr::mutate(id = obj$gage_data_sf$monitoring_location_name)

    data
  })

  data <- do.call(rbind, df_list)

  model_data <- lm(event_AGWRC ~ log(norm_flow), data = data)
  summary(model_data)

  b_data <- model_data$coefficients[1]
  m_data <- model_data$coefficients[2]

  data_new <- data.frame(norm_flow = seq(min(data$norm_flow), max(data$norm_flow), length.out = length(data)))
  data_new <- cbind(data_new, predict(model_data, newdata = data_new, interval = "confidence"))

  plot <- ggplot2::ggplot(data, aes(x = norm_flow, y = event_AGWRC)) +
    ggplot2::geom_point(aes(color = factor(id)), size = 1.5) +
    ggplot2::geom_ribbon(data = data_new, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.2, fill = "black") +
    ggplot2::geom_line(data = data_new, aes(y = fit), color = "black") +
    ggplot2::labs(title = "Multi-Regression Curve",
         color = "ID",
         x = "Normalized Flow (cfs)",
         y = "Event AGWRC") +
    ggplot2::theme_minimal()

  print(plot)
  data.frame(m = m_data, b = b_data)
}

## Local Testing
#library(hydrotools)
#library(tidyverse)
#basepath <- "/var/www/R"
#source("/var/www/R/config.R")
#
#FrontRoyal <- hydrotools::WaterGageDaily$new(gage_id = '01631000', ds_in = ds)
#Strasburg <- hydrotools::WaterGageDaily$new(gage_id = '01634000', ds_in = ds)
#
#df <- multi_regression_curve(df = list(Strasburg, FrontRoyal))
