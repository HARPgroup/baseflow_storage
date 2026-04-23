library(ggplot2)
library(dplyr)
library(dplyr)
library(lubridate)

events_CS <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01632000.csv")
events_MJ <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01633000.csv")
events_S <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01634000.csv")

events_S <- events_S %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date)
  )

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")
events_CS <- add_model_data(events_CS, land_type_code = "forN51165", "AGWS", scenario = "subsheds2", site = "http://deq1.bse.vt.edu:81")
events_MJ <- add_model_data(events_MJ, land_type_code = "forN51171", "AGWS", scenario = "subsheds2", site = "http://deq1.bse.vt.edu:81")
events_S <- add_model_data(events_S, land_type_code = "forN51187", "AGWS", scenario = "subsheds2", site = "http://deq1.bse.vt.edu:81")



events_S <- events_S %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date)
  )

#CS forN51165 MJ S

# Compute average AGWS per GroupID, then join back
events_CS_avg <- events_CS %>%
  group_by(GroupID) %>%
  mutate(avg_AGWS = mean(AGWS, na.rm = TRUE)) %>%
  ungroup()


events_MJ_avg <- events_MJ %>%
  group_by(GroupID) %>%
  mutate(avg_AGWS = mean(AGWS, na.rm = TRUE)) %>%
  ungroup()

events_S_avg <- events_S %>%
  group_by(GroupID) %>%
  mutate(avg_AGWS = mean(AGWS, na.rm = TRUE)) %>%
  ungroup()


ggplot(
  events_CS_avg,
  aes(x = avg_AGWS, y = calc_AGWR)
) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Average AGWS",
    y = "AGWRC",
    title = "AGWRC vs Average AGWS Cootes Store"
  ) +
  coord_cartesian(ylim = c(0.75, 1))

ggplot(
  events_MJ_avg,
  aes(x = avg_AGWS, y = calc_AGWR)
) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Average AGWS",
    y = "AGWRC",
    title = "AGWRC vs Average AGWS Mount Jackson"
  ) +
  coord_cartesian(ylim = c(0.75, 1))

ggplot(
  events_CS_avg,
  aes(x = avg_AGWS, y = calc_AGWR)
) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Average AGWS",
    y = "AGWRC",
    title = "AGWRC vs Average AGWS Strasburg"
  ) +
  coord_cartesian(ylim = c(0.75, 1))
