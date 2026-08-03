gage_obj <- WaterGageDaily$new(gage_id = "02056900", ds_in = ds)
daily_flow <- gage_obj$gage_data 

summer_min_flows <- daily_flow |> 
  mutate(Month = month(time),
         Year = year(time)) |> 
  filter((month(time) == 6 & mday(time) >= 1) | 
           (month(time) == 7 & mday(time) <= 15)) |> 
  group_by(Year) |> 
  filter(value == min(value)) |> 
  slice_tail(n = 1)

summary <- summer_min_flows |> 
  select(-monitoring_location_id, -parameter_code, -approval_status) |> 
  mutate(min_90d_flow = NA,
         min_forecasted = NA)

for(i in 1:nrow(summary)){
  
  forecast <- gage_obj$baseflow_forecast(start_date = summary$time[i], AGWRC = "lm_variable",
                                         use_limits = TRUE)
  
  obs_min_data <- forecast |> 
    dplyr::slice_min(obs_flow)
  
  summary$min_90d_flow[i] <- obs_min_data$obs_flow[1]
  summary$min_forecasted[i] <- obs_min_data$Forecast[1]
}

# Scatter plot  
ggplot(summary, aes(min_90d_flow, min_forecasted))+
  geom_point()+
  geom_abline()+
  labs(x = "Min Observed Flow", y = "Forecasted Flow", title = "Yearly Summer Lowflow Projections (Blackwater River)")+
  theme_minimal()

plot_event_minima(gage_obj, "2003-07-15")

nrow(summary |> 
  filter(min_90d_flow >= min_forecasted))
