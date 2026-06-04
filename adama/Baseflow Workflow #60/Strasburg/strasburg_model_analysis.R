library(tidyverse)

# read model and USGS data
model_events <- read_csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/Baseflow Workflow #60/Strasburg/bf_model_events_01634000.csv") |>
  mutate(Date = as.Date(Date), Year = year(Date), Month = month(Date))
usgs_events <- read_csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/Baseflow Workflow #60/Strasburg/strasburg_usgs_flow.csv") |>
  mutate(Date = as.Date(Date), Year = year(Date), Month = month(Date))

# join model and USGS data, auto removing all non-matching dates
join_events <- inner_join(model_events, usgs_events, by = "Date") |>
  select(Date = Date, Flow_model = Flow.x, Flow_usgs = Flow.y) |>
  mutate(Month = month(Date), Year = year(Date))

# plot initial hydrograph of model flow vs usgs flow
ggplot(join_events, aes(x = Date, y = Flow_model, col = "model")) +
  geom_line() + 
  geom_line(aes(y = Flow_usgs, col = "usgs")) +
  theme_classic() +
  labs(x = element_blank(), y = "Q (cfs)")

# find mean monthly 10% from usgs data
usgs_events_10p <- usgs_events |>
  group_by(Month) |>
  summarize(usgs_10p = quantile(Flow, 0.10))

# group_by(Year, Month) --> all months for all years

# join usgs_10p and remove all usgs data with a higher value
low_flow_events <- inner_join(join_events, usgs_events_10p, by = "Month", multiple = "all") |>
  filter(Flow_usgs <= usgs_10p)

# plot hydrograph
ggplot(low_flow_events, aes(x = Date, y = Flow_model, col = "model")) +
  geom_line() + 
  geom_line(aes(y = Flow_usgs, col = "usgs")) +
  theme_classic() +
  labs(x = element_blank(), y = "Q (cfs)")

# plot boxplot
low_flow_events_plot <- low_flow_events |>
  pivot_longer(cols = c(Flow_model, Flow_usgs), names_to = "variable", values_to = "value")

low_flow_events_plot$Month <- factor(
  low_flow_events_plot$Month,
  levels = 1:12,
  labels = month.abb)
  
ggplot(low_flow_events_plot, aes(x = Month, y = value, fill = variable)) +
  geom_boxplot(position = "dodge") +
  labs(x = "Month", y = "Q (cfs)", title = "Low Flow Evaluation of Model and USGS Gage Data") +
  theme_classic()
  
# Plot model base flow events with dots represnting the usgs_10p
# usgs gage data plot --> points usgs_10p, add 05p and 25p
