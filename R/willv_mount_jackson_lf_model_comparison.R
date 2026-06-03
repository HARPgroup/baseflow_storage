
library(tidyverse)
library(plotly)

model_events <- read_csv("bf_model_events_01633000.csv")
usgs_events <- read_csv("mount_jackson_usgs_flow.csv")

#Join data and select Date, Flow_model, Flow_usgs, and GroupID columns
joined_events <- model_events |>
  inner_join(usgs_events, by = "Date") |>
  select(Date = Date,
         Flow_model = Flow.x,
         Flow_usgs = Flow.y,
         GroupID) |>
  mutate(month = month(Date))

# Hydrograph of model and usgs flow
ggplot(joined_events, aes(Date, Flow_model, col = "Model"))+
  geom_line()+
  geom_line(aes(y = Flow_usgs, col = "USGS"))+
  theme_classic()+
  labs(x = element_blank(),
       y = "Q (cfs)")

#Calculate 10th percentile flows by month
joined_events_10p <- joined_events |>
  mutate(month = month(Date)) |>
  group_by(month) |>
  summarize(usgs_10p = quantile(Flow_usgs, probs = 0.1),
            model_10p = quantile(Flow_model, probs = 0.1))

# joined percentiles to flow data and filtered to only low flow events
joined_events <- joined_events |>
  left_join(joined_events_10p, by = "month", multiple = "all") |>
  filter(Flow_usgs < usgs_10p)

#plot of Model BF vs USGS 10th percentile
plot <- joined_events |>
  ggplot(aes(Date, Flow_model, group = GroupID, color = "Model BF"))+
  geom_line()+
  geom_point(aes(y = usgs_10p, col = "USGS 10%"))+
  scale_y_log10()+
  theme_classic()+
  labs(x = "",
       y = "Flow (CFS)",
       title = "Model Base Flow vs. USGS Monthly 10th Percentile")

ggplotly(plot)
