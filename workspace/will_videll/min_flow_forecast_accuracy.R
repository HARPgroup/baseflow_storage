basepath <- "/var/www/R"
source("/var/www/R/config.R")
library(hydrotools)
library(agws)
gage_obj <- WaterGageDaily$new(gage_id = "03524000", ds_in = ds)

#Plot forecasts as plotly or ggplot
forecast_data <- gage_obj$baseflow_forecast(start_date = "2023-07-01", AGWRC = "lm_variable",
                                            use_limits = TRUE)
