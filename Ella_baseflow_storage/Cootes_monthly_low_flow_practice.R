# Explore low flows with Cootes model data and USGS data

library(tidyverse)

# load in the csv files
Cootes_model_events <- read_csv("bf_model_events_01632000.csv")
Cootes_usgs_events <- read_csv("cootes_store_usgs_flow.csv")

# combine the two datasets by data so both start after 1984
joined_events <- Cootes_model_events %>%
  inner_join(Cootes_usgs_events, by = "Date") %>%
  select (Date = Date,
          Flow_model = Flow.x,
          Flow_usgs = Flow.y)

# Plot the hydrograph of the Model and the gage data
ggplot(joined_events, aes(x = Date, y = Flow_model, col = "Model")) +
  geom_line() +
  geom_line(aes(y = Flow_usgs, col = "USGS")) +
  theme_classic() +
  labs(x = element_blank(),
       y = "Flow (cfs)")

# create a new column for the 10th percentile flow
# calculate the 10th percentile flow
# month() pulls the month from the Date
events_10p <- joined_events %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarize(usgs_10p = quantile(Flow_usgs, probs = 0.1),
            model_10p = quantile(Flow_model, probs = 0.1))

# keep all USGS values that are below the 10th percentile
joined_events_10p <- joined_events %>%
  mutate(month = month(Date)) %>%
  left_join(events_10p, by = "month", multiple = "all") %>%
  filter(Flow_usgs < usgs_10p)

# Calculate the mean difference between model and usgs data
joined_events_10p <- joined_events_10p %>%
  mutate(Difference = Flow_usgs - Flow_model) 

# Calculate the mean difference between model and usgs data
joined_events_diff <- joined_events_10p %>%
  mutate(Difference = Flow_model - Flow_usgs) %>%
  group_by(month) %>%
  summarize(mean_difference = mean(Difference))

# plot of difference between model and usgs data per month
ggplot(joined_events_10p, aes(x = as.factor(month), y = Difference)) +
  geom_col() +
  labs(x = "Month",
       y = "Difference between USGS Flow and Model Flow",
       title = "Difference between USGS Flow and Model Flow per Month")

# column chart showing mean difference
ggplot(joined_events_diff, aes(x = as.factor(month), y = mean_difference)) +
  geom_col() +
  labs(x = "Month",
       y = "Mean Difference between Model Flow and USGS Flow",
       title = "Mean Difference between Model Flow and USGS Flow per Month")
# The model produced values that were considerably closer to the USGS values in 
# June through October than the other months 

# create a new column for the 15th percentile flow
# calculate the 15th percentile flow
events_15p <- joined_events %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarize(usgs_15p = quantile(Flow_usgs, probs = 0.15),
            model_15p = quantile(Flow_model, probs = 0.15))

# keep all USGS values that are below the 15th percentile
joined_events_15p <- joined_events %>%
  mutate(month = month(Date)) %>%
  left_join(events_15p, by = "month", multiple = "all") %>%
  filter(Flow_usgs < usgs_15p)

# Calculate the 25th percentile flow
events_25p <- joined_events %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarize(usgs_25p = quantile(Flow_usgs, probs = 0.25),
            model_25p = quantile(Flow_model, probs = 0.25))

# keep all USGS values that are below the 25th percentile
joined_events_25p <- joined_events %>%
  mutate(month = month(Date)) %>%
  left_join(events_25p, by = "month", multiple = "all") %>%
  filter(Flow_usgs < usgs_25p)

# add month column to joined events
joined_events <- joined_events %>%
  mutate(month = month(Date))

# create a boxplot of the 10th, 15th, and 25th percentile flows per month
ggplot(joined_events, aes(x = as.factor(month), y = Flow_usgs)) +
  geom_boxplot() + 
  geom_point(data = events_10p, aes(x = as.factor(month), y = usgs_10p, 
                                    color = "10th Percentile")) + 
  geom_point(data = events_15p, aes(x = as.factor(month), y = usgs_15p,
                                    color = "15th Percentile"))+ 
  geom_point(data = events_25p, aes(x = as.factor(month), y = usgs_25p,
                                    color = "25th Percentile")) +
  labs(x = "Month",
       y = "Gage Flow (cfs)",
       title = "USGS Gage Flow per Month") +
  scale_color_manual("", 
                     breaks = c("10th Percentile", "15th Percentile", "25th Percentile"),
                     values = c("red", "blue", "green"))

# create a boxplot of the 10th, 15th, and 25th percentile flows for June
joined_events_June <- joined_events %>%
  filter(month == "6")

events_10p_June <- events_10p %>%
  filter(month == "6")

events_15p_June <- events_15p %>%
  filter(month == "6")

events_25p_June <- events_25p %>%
  filter(month == "6")

### remove the outlier
joined_events_June_no_outliers <- joined_events_June[-c(113,114), ]

ggplot(joined_events_June_no_outliers, aes(x = as.factor(month), y = Flow_usgs)) +
  geom_boxplot() + 
  geom_point(data = events_10p_June, aes(x = as.factor(month), y = usgs_10p, 
                                    color = "10th Percentile")) + 
  geom_point(data = events_15p_June, aes(x = as.factor(month), y = usgs_15p,
                                    color = "15th Percentile"))+ 
  geom_point(data = events_25p_June, aes(x = as.factor(month), y = usgs_25p,
                                    color = "25th Percentile")) +
  labs(x = "Month",
       y = "Gage Flow (cfs)",
       title = "USGS Gage Flow per Month") +
  scale_color_manual("", 
                     breaks = c("10th Percentile", "15th Percentile", "25th Percentile"),
                     values = c("red", "blue", "green"))

# create a histogram showing flow trends during the month of June
joined_events_10p_June <- joined_events_10p %>%
  filter(month == "6")

joined_events_15p_June <- joined_events_15p %>%
  filter(month == "6")

joined_events_25p_June <- joined_events_25p %>%
  filter(month == "6")

ggplot(joined_events_June_no_outliers, aes(x = Date, 
                               y = Flow_usgs, 
                               col = "USGS")) +
  geom_point() +
  geom_point(data = joined_events_10p_June, aes(x = Date, 
                                               y = Flow_usgs, 
                                               col = "10th Percentile")) +
  geom_point(data = joined_events_15p_June, aes(x = Date, 
                                               y = Flow_usgs, 
                                               col = "15th Percentile")) +
  geom_point(data = joined_events_25p_June, aes(x = Date, 
                                               y = Flow_usgs, 
                                               col = "25th Percentile")) +
  theme_classic() +
  labs(x = "Date",
       y = "Flow (cfs)")



