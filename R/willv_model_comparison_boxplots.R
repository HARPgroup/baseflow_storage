
library(tidyverse)
library(plotly)

baseflow_events <- read_csv("bf_events_01632000.csv") |>
  select(Date, Flow, Month)
usgs_events <- read_csv("cootes_store_usgs_flow.csv") |>
  select(Date, Flow)

# Calculate Xth percentile flows by month
usgs_lf_percentiles <- usgs_events |>
  mutate(month = month(Date)) |>
  group_by(month) |>
  summarize(usgs_05p = quantile(Flow, probs = 0.05, na.rm = TRUE),
            usgs_10p = quantile(Flow, probs = 0.1, na.rm = TRUE),
            usgs_25p = quantile(Flow, probs = .25, na.rm = TRUE))

# joined overall percentiles to base flow event data
baseflow_events_joined <- baseflow_events |>
  left_join(usgs_lf_percentiles, by = c("Month" = "month"), multiple = "all")

############
# Box plot #
############

# % points represent percentiles derived from overall flow, boxes represent baseflow events
ggplot(baseflow_events_joined, aes(x = as.factor(Month), y = Flow))+
  geom_boxplot()+
  geom_point(aes(y = usgs_05p, color = "5% Flow"))+
  geom_point(aes(y = usgs_10p, color = "10% Flow"))+
  geom_point(aes(y = usgs_25p, color = "25% Flow"))+
  scale_y_log10()+
  labs(x = "",
       y = "Flow (CFS)",
       title = "Base Flow by Month")+
  theme_classic()

##############################

slope <- out$m
intercept <- out$b

ggplot(event_df, aes(median_flow, event_AGWRC))+
  geom_point()+
  geom_abline(intercept = intercept, slope = slope, color = "red")+
  labs(x = "Q",
       y = "AGWRC")+
  scale_x_log10()+
  theme_classic()
