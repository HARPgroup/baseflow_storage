### AGWR at local minima ###
basepath <- "/var/www/R"
source("/var/www/R/config.R")
library(hydrotools)
library(agws)
library(tidyverse)

gage_obj <- WaterGageDaily$new(gage_id = "01634000", ds_in = ds)
#var <- gage_obj$baseflow_workflow_data(omsite) 
nep <- gage_obj$nep_table() |> 
  mutate(month_num = match(Month, month.abb))

# Read in daily flows and join with NEP table
daily_flow <- gage_obj$gage_data |> 
  mutate(month_num = month(time)) |> 
  left_join(nep, by = "month_num", multiple = "all") |> 
  mutate(value = value + rnorm(n = nrow(daily_flow), sd = 0.1))

# Create trough column and calculate AGWR
daily_flow$trough <- zoo::rollapply(daily_flow$value, width = 7, function(x) x[4] <= min(x[-4]), fill = FALSE)
daily_flow <- daily_flow |> 
  mutate(AGWR = value / lag(value))

# Plot
plotly::ggplotly(daily_flow |> 
  filter(trough & month(time) %in% c(1:12) & value < `25%`) |> 
  ggplot(aes(value, AGWR, text = paste("Date:", time), col = Month))+
    geom_point()+
  scale_x_log10()+
  theme_minimal()+
  labs(x = "Flow (CFS)", title = "Local Minima (25% Monthly Flow)"))

# Sample troughs plot
daily_flow1 <- daily_flow |> 
  filter(time < time[90])

  plotly::ggplotly(ggplot(daily_flow1, aes(text = paste("Date:", time)))+
  geom_line(aes(time, value))+
  geom_point(data = subset(daily_flow1, trough == TRUE), aes(time, value), col = "red"))

# Update github issues
# document in rmd - illustrate as best as possible
# show time series of specific points on a line
# Work with will and ella - think back to other papers