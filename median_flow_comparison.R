
library(tidyverse)
library(zoo)
library(data.table)

# Data prep
baseflow_events <- read_csv("bf_events_01632000.csv") |>
<<<<<<< HEAD
  select(Date, Flow, Month, GroupID) |>
  group_by(Month) |>
  summarise(ymax = quantile(Flow, probs = .6),
            ymin = quantile(Flow, probs = .4),
            median = mean(Flow))

usgs_daily <- read_csv("cootes_store_usgs_flow.csv") |>
  select(Date, Flow) |>
  mutate(Month = month(Date),
         xdaymin = frollmin(Flow,
                            7,
                            fill = NA,
                            na.rm = T)) |>
  group_by(Month) |>
  summarise(mean = median(xdaymin),
            p25 = quantile(Flow, probs = .3, na.rm = T))
=======
  select(Date, Flow, Month, GroupID) |> 
  group_by(Month) |> 
  summarise(ymax = quantile(Flow, probs = .6),
            ymin = quantile(Flow, probs = .4),
            median = median(Flow))

usgs_daily <- read_csv("cootes_store_usgs_flow.csv") |>
  select(Date, Flow) |> 
  mutate(Month = month(Date),
         xdaymin = frollmin(Flow, 
                            14, 
                            fill = NA, 
                            na.rm = T)) |> 
  group_by(Month) |> 
  summarise(mean = median(xdaymin))
>>>>>>> 29db14b6452b0c7716c0ae4820990f27fd57baae



# Plot
ggplot()+
<<<<<<< HEAD
  geom_ribbon(data = baseflow_events, aes(x = Month, ymax = ymax, ymin = ymin),
=======
  geom_ribbon(data = baseflow_events, aes(x = Month, ymax = ymax, ymin = ymin), 
>>>>>>> 29db14b6452b0c7716c0ae4820990f27fd57baae
              fill = "red", alpha = 0.1) +
  geom_line(data = baseflow_events, aes(x = Month, y = ymax, color = "60th Percentile"), linewidth = 1, linetype = "dashed") +
  geom_line(data = baseflow_events, aes(x = Month, y = ymin, color = "40th Percentile"), linewidth = 1, linetype = "dotdash") +
  geom_line(data = baseflow_events, aes(x = Month, y = median, color = "Median"), linewidth = 1, linetype = "solid", alpha = .3) +
<<<<<<< HEAD
  geom_point(data = usgs_daily, aes(Month, mean, fill = "Average 7 day min"),
             size = 2, shape = 21, color = "black", stroke = 1)+
  geom_point(data = usgs_daily, aes(Month, p25, fill = "Monthly 30th Percentile"),
             size = 2, shape = 21, color = "black", stroke = 1)+
  scale_fill_manual(name = "", values = c("Average 7 day min" = "blue", "Monthly 30th Percentile" = "green")) +
=======
  geom_point(data = usgs_daily, aes(Month, mean, fill = "Average 14 day min"))+
  scale_fill_manual(name = "", values = c("Average 14 day min" = "blue")) +
>>>>>>> 29db14b6452b0c7716c0ae4820990f27fd57baae
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(name = "Threshold Bounds",
                     values = c("60th Percentile" = "orange", "40th Percentile" = "red", "Median" = "darkred"))+
  scale_y_log10()+
<<<<<<< HEAD


  labs(
    title = "Median 7 Day Min Flow vs Median Event Flow (Mount Jackson)",
=======
  
  
  labs(
    title = "Median 14 Day Min Flow vs Median Event Flow (SF Lynnwood)",
>>>>>>> 29db14b6452b0c7716c0ae4820990f27fd57baae
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



