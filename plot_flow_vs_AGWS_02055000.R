# We need to be able to plot storage versus AGWRC to make it easier to frame
# our results in the context of HSPF equations
#Storage versus flow plots for Roanoke

#didnt work cauase flow units and AGWS units are not comaprable 
# Mutate RoanokeTrimStats.csv to add a column for AGWS (active groundwater storage)
#baseflow_trimmed_stats_02055000 <- read_csv("RoanokeTrimStats.csv")
#baseflow_trimmed_stats_02055000 <- baseflow_trimmed_stats_02055000 %>% 
#  mutate(AGWS = Flow / (1 - AGWRC))

library(dataRetrieval)
library(hydrotools)
library(tidyverse)
library(dplyr)
library(ggplot2)
#baseflow_trimmed_stats_Roanoke <- read_csv("RoanokeTrimStats.csv")

#Roanoke_gage <- readNWISdv(siteNumbers = "02055000", 
                        #parameterCd = "00060")


#import the csv
baseflow_trimmed_stats_02055000 <- read_csv("RoanokeTrimStats.csv")
baseflow_trimmed_stats_02055000 <- baseflow_trimmed_stats_02055000

#Create function to find drainage area (sq.mi) and Flow per drainage area (cfs/sq. mi)  

add_drainage_area <- function(df, gageID) {
  da <- readNWISsite(gageID) %>%
    pull(drain_area_va)
  df %>%
    mutate(
      DrainageArea = da,
      Flow_per_DA = Flow / da
    )
}

baseflow_trimmed_stats_02055000 <- add_drainage_area(
  baseflow_trimmed_stats_02055000,
  "02055000"
)
#Function to convert flow from cfs to inches using the USGS gage area
#flow_to_inches <- function(flow_cfs, drainage_area_sqmi) {
#  flow_cfs * 1.008 / drainage_area_sqmi
#}

#baseflow_trimmed_stats_02055000 <- baseflow_trimmed_stats_02055000 %>%
#  mutate(
#    Flow_inches_day = flow_to_inches(Flow, DrainageArea)
#  )

convert.flow <- function(flow_col, area_sqmi){
  cfs <- flow_col
  # Create conversion factor
  conversion <- (86400*12)/(5280*5280)
  sp_conv <- conversion/area_sqmi
  
  flow_in <- cfs * sp_conv
  
  return(flow_in)
}
baseflow_trimmed_stats_02055000 <- baseflow_trimmed_stats_02055000 %>% 
  mutate(
    flow_in = convert.flow(Flow, DrainageArea)
  )


#adding AGWS (Active Groundwater Stoarge) column 
baseflow_trimmed_stats_02055000 <- baseflow_trimmed_stats_02055000 %>% 
  mutate(AGWS = flow_in / (1 - AGWRC))

#Plot 
ggplot(baseflow_trimmed_stats_02055000, aes(x = flow_in, y = AGWS)) +
  geom_point(size = 1.5) +
  labs(
    title = "Flow vs. AGWS For Roanoke",
    x = "Q (in/day)",
    y = "AGWS"
  ) +
  theme_classic()
