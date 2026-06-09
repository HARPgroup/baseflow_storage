
library(tidyverse)
library(plotly)

baseflow_events <- read_csv("bf_events_01632000.csv") |>
  select(Date, Flow, Month, GroupID)
usgs_events <- read_csv("cootes_store_usgs_flow.csv") |>
  select(Date, Flow)

# Calculate Xth percentile flows by month
usgs_lf_percentiles <- usgs_events |>
  mutate(month = month(as.Date(Date))) |>
  group_by(month) |>
  summarize(usgs_05p = quantile(Flow, probs = 0.05, na.rm = TRUE),
            usgs_10p = quantile(Flow, probs = 0.1, na.rm = TRUE),
            usgs_25p = quantile(Flow, probs = .25, na.rm = TRUE),
            usgs_30p = quantile(Flow, probs = .30, na.rm = TRUE),
            usgs_50p = quantile(Flow, probs = .50, na.rm = TRUE))

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
       title = "Base Flow by Month (Lynnwood)")+
  theme_classic()

# Group usgs data by month and year
bf_median <- baseflow_events |>
  group_by(GroupID) |>
  summarize(median_flow = median(Flow),
            month = month(Date))

##########################
# Baseflow event scatter #
##########################

# Plot of Event Median flows vs historical baselines
ggplot() +
  #baseline ribbon
  geom_ribbon(data = usgs_lf_percentiles,
              aes(x = month, ymin = usgs_05p, ymax = usgs_25p),
              fill = "red", alpha = 0.1) +

  #creates lines for percentiles
  geom_line(data = usgs_lf_percentiles, aes(x = month, y = usgs_50p, color = "50th Percentile"), linewidth = 1, linetype = "dashed") +
  geom_line(data = usgs_lf_percentiles, aes(x = month, y = usgs_30p, color = "30th Percentile"), linewidth = 1, linetype = "dotdash") +
  geom_line(data = usgs_lf_percentiles, aes(x = month, y = usgs_25p, color = "25th Percentile"), linewidth = 1, linetype = "dashed") +
  geom_line(data = usgs_lf_percentiles, aes(x = month, y = usgs_10p, color = "10th Percentile"), linewidth = 1, linetype = "dotdash") +
  geom_line(data = usgs_lf_percentiles, aes(x = month, y = usgs_05p,  color = "5th Percentile"),  linewidth = 1, linetype = "solid") +

  #plots baseflow event medians, formatting
  geom_point(data = bf_median, aes(x = month, y = median_flow, fill = "Specific Baseflow Events"),
             size = 2, shape = 21, color = "black", stroke = 1) +

  #month abb. on axis
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  #line legend
  scale_color_manual(name = "Historical Baselines",
                     values = c("50th Percentile" = "darkgreen", "30th Percentile" = "yellow", "25th Percentile" = "orange", "10th Percentile" = "red", "5th Percentile" = "darkred")) +
  #point legend
  scale_fill_manual(name = "", values = c("Specific Baseflow Events" = "blue")) +
  #general formatting
  labs(
    title = "Specific Baseflow Events vs. Historical Monthly Lowflow Percentiles (Cootes Store)",
    x = "Month",
    y = "Baseflow (cfs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

