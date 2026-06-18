# Explore low flows with Luray USGS data from Baseflow Workflow

library(tidyverse)

# load in the csv files
Luray_usgs_events <- read_csv("LuraySummaryStats.csv")

# Plot the hydrograph of the Model and the gage data
ggplot(Luray_usgs_events, aes(x = start_date, y = median_flow, col)) +
  geom_line() +
  theme_classic() +
  labs(x = element_blank(),
       y = "Flow (cfs)",
       title = "Median Flow over Time for S F Shenandoah River near Luray, VA")

# create a new column for the 10th percentile flow
# calculate the 10th percentile flow
# month() pulls the month from the Date
events_10p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  group_by(month) %>%
  summarize(usgs_10p = quantile(median_flow, probs = 0.1))

# keep all USGS values that are below the 10th percentile
Luray_events_10p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  left_join(events_10p, by = "month", multiple = "all") %>%
  filter(median_flow < usgs_10p)

# create a new column for the 15th percentile flow
# calculate the 15th percentile flow
events_15p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  group_by(month) %>%
  summarize(usgs_15p = quantile(median_flow, probs = 0.15))

# keep all USGS values that are below the 15th percentile
Luray_events_15p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  left_join(events_15p, by = "month", multiple = "all") %>%
  filter(median_flow < usgs_15p)

# Calculate the 25th percentile flow
events_25p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  group_by(month) %>%
  summarize(usgs_25p = quantile(median_flow, probs = 0.25))

# keep all USGS values that are below the 15th percentile
Luray_events_25p <- Luray_usgs_events %>%
  mutate(month = month(start_date)) %>%
  left_join(events_25p, by = "month", multiple = "all") %>%
  filter(median_flow < usgs_25p)

# add month column to joined events
Luray_usgs_events <- Luray_usgs_events %>%
  mutate(month = month(start_date))

# create a boxplot of the 10th, 15th, and 25th percentile flows per month
ggplot(Luray_usgs_events, aes(x = as.factor(month), y = median_flow)) +
  geom_boxplot() + 
  geom_point(data = Luray_events_10p, aes(x = as.factor(month), y = usgs_10p, 
                                    color = "10th Percentile")) + 
  geom_point(data = Luray_events_15p, aes(x = as.factor(month), y = usgs_15p,
                                    color = "15th Percentile"))+ 
  geom_point(data = Luray_events_25p, aes(x = as.factor(month), y = usgs_25p,
                                    color = "25th Percentile")) +
  labs(x = "Month",
       y = "Gage Flow (cfs)",
       title = "USGS Gage Flow per Month") +
  scale_color_manual("", 
                     breaks = c("10th Percentile", "15th Percentile", "25th Percentile"),
                     values = c("red", "blue", "green"))

# hydrograph showing median flow and percentile flows
ggplot(Luray_usgs_events, aes(x = start_date, y = median_flow, col)) +
  geom_line() +
  geom_line(data = Luray_events_10p, aes(x = start_date, 
                                         y = usgs_10p, 
                                         color = "10th Percentile")) +
  geom_line(data = Luray_events_15p, aes(x = start_date, 
                                         y = usgs_15p, 
                                         color = "15th Percentile")) +
  geom_line(data = Luray_events_25p, aes(x = start_date, 
                                         y = usgs_25p, 
                                         color = "25th Percentile")) +
  theme_classic() +
  labs(x = element_blank(),
       y = "Flow (cfs)",
       title = "Flow over Time for S F Shenandoah River near Luray, VA")

# boxplot of AGWRC per month
ggplot(Luray_usgs_events, aes(x = as.factor(month), y = event_AGWRC)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Month",
       y = "AGWRC",
       title = "AGWRC per Month at S F Shenandoah River near Luray, VA")

# left join min monthly flow to trim stats to have AGWRC for low flows
Luray_min_monthly_stats <- Luray_min_monthly %>%
  left_join(Luray_Trim_Stats, by = "Year", multiple = "first")

# boxplot of AGWRC per month for low flows
ggplot(Luray_min_monthly_stats, aes(x = as.factor(Month.x), y = AGWRC)) +
  geom_boxplot() +
  geom_boxplot(data = Luray_usgs_events, aes(x = as.factor(month), y =  )) +
  labs(x = "Month",
       y = "AGWRC",
       title = "Low Flow AGWRC per Month at S F Shenandoah River near Luray, VA")

# boxplot of AGWRC per event duration
ggplot(Luray_usgs_events, aes(x = as.factor(n_days), y = event_AGWRC)) +
  geom_boxplot() + 
  labs(x = "Observed duration",
       y = "AGWRC",
       title = "AGWRC per Month at S F Shenandoah River near Luray, VA")

# load in the csv files
Luray_Trim_Stats <- read_csv("LurayTrimStats.csv")

# Calculate monthly minimum flows
Luray_min_monthly <- Luray_Trim_Stats %>%
  group_by(Year, Month) %>%
  summarize(min_flow = min(Flow))

# boxplot of minimum monthly flows per year
ggplot(Luray_min_monthly, aes(x = as.factor(Month), y = min_flow)) +
  geom_boxplot() + 
  geom_point(data = Luray_events_10p, aes(x = as.factor(month), y = usgs_10p, 
                                          color = "10th Percentile")) + 
  geom_point(data = Luray_events_15p, aes(x = as.factor(month), y = usgs_15p,
                                          color = "15th Percentile"))+ 
  geom_point(data = Luray_events_25p, aes(x = as.factor(month), y = usgs_25p,
                                          color = "25th Percentile")) +
  labs(x = "Month",
       y = "Minimum Monthly Flow (cfs)",
       title = "Minimum Monthly Flows at S F Shenandoah River near Luray, VA") +
  scale_color_manual("", 
                     breaks = c("10th Percentile", "15th Percentile", "25th Percentile"),
                     values = c("red", "blue", "green"))

# barchart of monthly minimum vs percentile flows
ggplot(Luray_min_monthly, aes(x = as.factor(Month), y = min_flow)) +
  geom_col(position = "dodge") +
  geom_col(data = events_10p, aes(x = as.factor(month), y = usgs_10p, color = "10th Percentile")) + 
  geom_col(data = events_15p, aes(x = as.factor(month), y = usgs_15p,
                                          color = "15th Percentile")) + 
  geom_col(data = events_25p, aes(x = as.factor(month), y = usgs_25p,
                                         color = "25th Percentile")) +
  labs(x = "Month",
       y = "Minimum Monthly Flow (cfs)",
       title = "Minimum Monthly Flows at S F Shenandoah River near Luray, VA") +
  scale_color_manual("", 
                     breaks = c("10th Percentile", "15th Percentile", "25th Percentile"),
                     values = c("red", "blue", "green"))







