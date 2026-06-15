#Import your gages csv
#on line 28 change the gage ID to your gage ID
#Change title names on plots to reflect your gage
library(hydrotools)
library(dataRetrieval)
library(tidyverse)
library(dplyr)

#import the csv
baseflow_trimmed_stats <- read_csv("C:\\HARP Folder\\baseflow_storage\\ForecastingApp\\Cootes_combined_baseflow_stats.csv")
baseflow_trimmed_stats <- baseflow_trimmed_stats

#Create function to find drainage area (sq.mi) and Flow per drainage area (cfs/sq. mi)  
add_drainage_area <- function(baseflow_trimmed_stats, gageID) {
  da <- read_waterdata_monitoring_location(
    monitoring_location_id = paste0("USGS-", gageID)
  ) %>%
    pull(drainage_area)
  
  baseflow_trimmed_stats %>%
    mutate(
      drainage_area = da,
      Flow_per_DA = Flow / da
    )
}
baseflow_trimmed_stats <- add_drainage_area(
  baseflow_trimmed_stats,
  "01634000"
)
#Function to convert flow from cfs to inches using the USGS gage area
source("C:\\HARP Folder\\baseflow_storage\\convert.flow.R")

baseflow_trimmed_stats <- baseflow_trimmed_stats %>% 
  mutate(
    flow_in = convert.flow(Flow, drainage_area)
  )
baseflow_trimmed_stats <- baseflow_trimmed_stats %>% 
  mutate(
    median_flow_in = convert.flow(median_flow, drainage_area)
  )


#adding AGWS (Active Groundwater Storage) column 
baseflow_trimmed_stats <- baseflow_trimmed_stats %>% 
  mutate(AGWS = median_flow_in / (1 - AGWRC))

#Plot Flow vs. AGWS
ggplot(baseflow_trimmed_stats, aes(x = median_flow_in, y = AGWS)) +
  geom_point(size = 1.5) +
  labs(
    title = "Median Flow vs. Median AGWS For Cootes Store (01634000)",
    x = "Median Q (in/day)",
    y = "Median AGWS (WS-in.)"
  ) +
  theme_classic()

#Plot AGWS vs. AGWRC
ggplot(baseflow_trimmed_stats, aes(x = AGWS, y = AGWRC)) +
  geom_point(size = 1.5) +
  labs(
    title = "Median AGWS vs. AGWRC For Cootes Store (01634000)",
    x = "AGWS (WS-in.)",
    y = "AGWRC"
  ) +
  theme_classic()