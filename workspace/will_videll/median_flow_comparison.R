
library(tidyverse)
library(zoo)
library(data.table)
library(dataRetrieval)

# Data prep
baseflow_events <- read_csv("bf_events_01633000.csv") |>
  select(Date, Flow, Month, GroupID) |>
  group_by(Month) |>
  summarise(p10 = quantile(Flow, probs = .1),
            p25 = quantile(Flow, probs = .25),
            median = median(Flow))

usgs_daily <- read_csv("mount_jackson_usgs_flow.csv") |>
  select(Date, Flow) |>
  mutate(Month = month(Date),
         xdaymin = frollmin(Flow,
                            7,
                            fill = NA,
                            na.rm = T)) |>
  group_by(Month) |>
  summarise(median = median(xdaymin, na.rm = T),
            #min = min(xdaymin, na.rm = T),
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
  #geom_point(data = usgs_daily, aes(Month, min, fill = "Min of 7 day min"),
            # size = 3, shape = 21, color = "black", stroke = 1)+
  geom_point(data = usgs_daily, aes(Month, p10, fill = "p10 of 7 day min"),
             size = 3, shape = 21, color = "black", stroke = 1)+
  scale_fill_manual(name = "Stats", values = c("Median of 7 day min" = "blue", #"Min of 7 day min" = "green",
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


### Find the closest line (method 1, not ideal)

<<<<<<< HEAD
<<<<<<< HEAD
# combined <- baseflow_events |>
#   left_join(usgs_daily, by = "Month") |>
#   select(Month,
#          bf_p25 = p25,
#          p10_month = p10_flow,
#          median_7day = median.y) |>
#   mutate(p10_dist = abs(bf_p25 - p10_month),
#          median_dist = abs(median_7day - bf_p25),
#          difference = abs(p10_dist - median_dist))
#
# combined$best_metric <- "Median"
# combined$best_metric[combined$p10_dist < combined$median_dist] <- "Monthly 10th percentile"
=======
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c
combined <- baseflow_events |>
  left_join(usgs_daily, by = "Month") |>
  select(Month,
         bf_p25 = p25,
         p10_month = p10_flow,
         median_7day = median.y) |>
  mutate(p10_dist = abs(bf_p25 - p10_month),
         median_dist = abs(median_7day - bf_p25),
         difference = abs(p10_dist - median_dist))

combined$best_metric <- "Median"
combined$best_metric[combined$p10_dist < combined$median_dist] <- "Monthly 10th percentile"
<<<<<<< HEAD
>>>>>>> dd476f1b1e9e2730541a9a8d20ae8a97a96cef4e
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c


####

bf_long <- baseflow_events |>
  select(Month,
         bf_p25 = p25,
         bf_p10 = p10,
         bf_median = median) |>
  pivot_longer(cols = bf_p25:bf_median, names_to = "type", values_to = "value")

usgs_long <- usgs_daily |>
  select(Month,
         median,
         p10_7day = p10,
         p10_monthly = p10_flow) |>
  pivot_longer(cols = median:p10_monthly, names_to = "type", values_to = "value")

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c
merged3 <- bf_long |>
  filter(type == "bf_p10") |>
  left_join(usgs_long, by = "Month", relationship = "many-to-many") |>
  mutate(vert_dist = abs(value.x - value.y),
         Abs_pct_err = (abs(value.x - value.y) / value.x) * 100) |>
<<<<<<< HEAD
=======
merged <- bf_long |>
  filter(type == "bf_median") |>
  left_join(usgs_long, by = "Month", relationship = "many-to-many") |>
  mutate(vert_dist = abs(value.x - value.y)) |>
>>>>>>> dd476f1b1e9e2730541a9a8d20ae8a97a96cef4e
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c
  group_by(type.x, Month) |>
  slice_min(vert_dist, n = 1, with_ties = FALSE) |>
  select(Month,
         bf_stat = type.x,
         bf_value = value.x,
         Matched_Point = type.y,
         Point_Value = value.y,
<<<<<<< HEAD
<<<<<<< HEAD
         Distance = vert_dist,
         Abs_pct_err) %>%
  arrange(Month, bf_stat)

### Do we want this as part of the workflow or a stand alone script
#all_VA_sites <- read_waterdata_monitoring_location(
#  state_name = "Virginia",
#  site_type = "Stream",
#  properties = c("monitoring_location_id", "agency_code"))
=======
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c
         Distance = vert_dist) %>%
  arrange(Month, bf_stat)

### Do we want this as part of the workflow or a stand alone script
all_VA_sites <- read_waterdata_monitoring_location(
  state_name = "Virginia",
  site_type = "Stream",
  properties = c("monitoring_location_id", "agency_code"))
<<<<<<< HEAD
>>>>>>> dd476f1b1e9e2730541a9a8d20ae8a97a96cef4e
=======
>>>>>>> a532287d1e418e85b37cbf59a0f2885ffa31616c
