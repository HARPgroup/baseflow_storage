
library(tidyverse)
library(zoo)
library(data.table)

# Data prep
baseflow_events <- read_csv("bf_events_01634000.csv") |>
  select(Date, Flow, Month, GroupID) |>
  group_by(Month) |>
  summarise(p10 = quantile(Flow, probs = .1),
            p25 = quantile(Flow, probs = .25),
            median = median(Flow))

usgs_daily <- read_csv("strasburg_usgs_flow.csv") |>
  select(Date, Flow) |>
  mutate(Month = month(Date),
         xdaymin = frollmin(Flow,
                            7,
                            fill = NA,
                            na.rm = T)) |>
  group_by(Month) |>
  summarise(median = median(xdaymin, na.rm = T),
            min = min(xdaymin, na.rm = T),
            p10 = quantile(xdaymin, probs = .1, na.rm = T),
            p10_flow = quantile(Flow, probs = .1, na.rm = T))



# Plot
ggplot()+
  geom_line(data = baseflow_events, aes(x = Month, y = p10, color = "10th Percentile"), linewidth = 1, linetype = "dashed") +
  geom_line(data = baseflow_events, aes(x = Month, y = p25, color = "25th Percentile"), linewidth = 1, linetype = "dotdash") +
  geom_line(data = baseflow_events, aes(x = Month, y = median, color = "Median"), linewidth = 1, linetype = "solid", alpha = .3) +
  geom_point(data = usgs_daily, aes(Month, median, fill = "Median of 7 day min"),
             size = 3, shape = 21, color = "black", stroke = 1)+
  geom_point(data = usgs_daily, aes(Month, p10_flow, fill = "p10 Monthly Flow"),
             size = 2, shape = 21, color = "black", stroke = 1)+
  geom_point(data = usgs_daily, aes(Month, min, fill = "Min of 7 day min"),
             size = 3, shape = 21, color = "black", stroke = 1)+
  geom_point(data = usgs_daily, aes(Month, p10, fill = "p10 of 7 day min"),
             size = 3, shape = 21, color = "black", stroke = 1)+
  scale_fill_manual(name = "Stats", values = c("Median of 7 day min" = "blue", "Min of 7 day min" = "green",
                                               "p10 of 7 day min" = "yellow", "p10 Monthly Flow" = "black")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(name = "Event Flow Percentiles",
                     values = c("10th Percentile" = "orange", "25th Percentile" = "red", "Median" = "darkred"))+
  scale_y_log10()+


  labs(
    title = "7 Day Min Flow vs Median Event Flow (Strasburg)",
    x = "Month",
    y = "Flow (cfs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )



