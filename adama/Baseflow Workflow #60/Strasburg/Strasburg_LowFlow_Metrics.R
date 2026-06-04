### Strasburg Baseflow Metric Analysis
library(tidyverse)

usgs_low_flow <- read_csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/Baseflow Workflow #60/Strasburg/strasSummaryStats.csv") |>
  mutate(mo = month(start_date))
usgs_gage <- read_csv("C:/HARP/HARP - GitHub/baseflow_storage/adama/Baseflow Workflow #60/Strasburg/strasGage.csv")

usgs_gage <- usgs_gage |>
  rename(start_date = obs_date)

## Seasonal 
#usgs_gage <- usgs_gage %>%
#  mutate(season = case_when(
#    mo %in% c(12, 1, 2) ~ "Winter",
#    mo %in% c(3, 4, 5)  ~ "Spring",
#    mo %in% c(6, 7, 8)  ~ "Summer",
#    mo %in% c(9, 10, 11) ~ "Fall"
#  ))
#
#usgs_low_flow <- usgs_low_flow %>%
#  mutate(season = case_when(
#    mo %in% c(12, 1, 2) ~ "Winter",
#    mo %in% c(3, 4, 5)  ~ "Spring",
#    mo %in% c(6, 7, 8)  ~ "Summer",
#    mo %in% c(9, 10, 11) ~ "Fall"
#  ))

# Calculate quantile flows by month for general gage data
usgs_gage_stats <- usgs_gage |>
  group_by(mo) |>
  summarize(
    usgs_05p = quantile(obs_flow, 0.05, na.rm = TRUE),
    usgs_10p = quantile(obs_flow, 0.10, na.rm = TRUE),
    usgs_25p = quantile(obs_flow, 0.25, na.rm = TRUE)
    )

## calculate mean of median flows
#usgs_median_mean <- usgs_low_flow|>
#  group_by(mo) |>
#  summarize(
#    mean_median = mean(median_flow))

# inner_join usgs_low_flows and usgs_gage_stats by month
low_flow_analysis <- inner_join(
  usgs_low_flow,
  usgs_gage_stats,
  by = "mo"
) |>
  select(GroupID, start_date, mo, median_flow, event_AGWRC,
         usgs_05p, usgs_10p, usgs_25p)

# month number to abv
low_flow_analysis$mo <- factor(
  low_flow_analysis$mo,
  levels = 1:12,
  labels = month.abb)

# box plot and point overlap
ggplot(low_flow_analysis) +
  geom_boxplot(aes(x = mo, y = median_flow), size = 0.33) + 
  geom_point(aes(x = mo, y = usgs_05p), color = "darkred", size = 1.5) +
  geom_point(aes(x = mo, y = usgs_10p), color = "red", size = 1.5) +
  geom_point(aes(x = mo, y = usgs_25p), color = "orange", size = 1.5) +
  labs(x = "Month", y = "Flow (cfs)", title = "USGS Low Flow vs. Seasonal Quantile Flow (5%, 10%, 25%)") +
  theme_minimal()

## scatter plot
#ggplot(low_flow_analysis) +
#  geom_point(aes(x = mo, y = median_flow), size = 1.5) + 
#  geom_point(aes(x = mo, y = usgs_05p), color = "darkred", size = 0.75) +
#  geom_point(aes(x = mo, y = usgs_10p), color = "red", size = 0.75) +
#  geom_point(aes(x = mo, y = usgs_25p), color = "orange", size = 0.75) +
#  labs(x = "Month", y = "Flow (cfs)", title = "USGS Low Flow vs. Seasonal Quantile Flow (5%, 10%, 25%)") +
#  theme_minimal()
