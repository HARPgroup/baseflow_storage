#import the csv
baseflow_trimmed_stats <- read_csv("MauryTrimStats.csv")
baseflow_trimmed_stats <- baseflow_trimmed_stats

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

baseflow_trimmed_stats <- add_drainage_area(
  baseflow_trimmed_stats,
  "02024000"
)

#Function to convert flow from cfs to icnhes using the USGS gage area
convert.flow <- function(Flow, DrainageArea){
  cfs <- Flow
  #  Create conversion factor
  conversion <- (86400*12)/(5280*5280)
  sp_conv <- conversion/DrainageArea
  
  flow_in <- cfs * sp_conv
  
  return(flow_in)
}
baseflow_trimmed_stats <- baseflow_trimmed_stats %>% 
  mutate(
    flow_in = convert.flow(Flow, DrainageArea)
  )


#adding AGWS (Active Groundwater Storage) column 
baseflow_trimmed_stats <- baseflow_trimmed_stats %>% 
  mutate(AGWS = flow_in / (1 - AGWRC))

#Plot flow vs. AGWS For Maury
ggplot(baseflow_trimmed_stats, aes(x = flow_in, y = AGWS)) +
  geom_point(size = 1.5) +
  labs(
    title = "Flow vs. AGWS For Maury (02024000)",
    x = "Q (in/day)",
    y = "AGWS"
  ) +
  theme_classic()

#Plot AGWS vs. AGWRC
ggplot(baseflow_trimmed_stats, aes(x = AGWS, y = AGWRC)) +
  geom_point(size = 1.5) +
  labs(
    title = "AGWS vs. AGWRC For Maury (02024000)",
    x = "Storage (WS-in.)",
    y = "Recession Coefficient"
  ) +
  theme_classic()