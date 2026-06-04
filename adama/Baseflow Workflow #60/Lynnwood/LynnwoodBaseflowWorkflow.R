#Lynnwood
gageID <- "01628500"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "LynnwoodGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("LynnwoodGage.csv", "obs_date", "obs_flow", FALSE, "LynnwoodEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("LynnwoodEvent.csv", "obs_date", "obs_flow", "Lynnwood", "LynnwoodBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("LynnwoodBF.csv", "for", "Lynnwoodstats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")

#Step 04_trim_events
commandArgs <- function(...){
  c("Lynnwoodstats.csv", "LynnwoodTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("LynnwoodTrim.csv", "LynnwoodTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")

#Step 06_regression_df
commandArgs <- function(...){
  c("LynnwoodTrimStats.csv", gageID, "LynnwoodSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("LynnwoodSummaryStats.csv", gageID, "LynnwoodAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c(gageID, "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "LynnwoodAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")


flow_data <- read.csv("LynnwoodGage.csv")
event_identification <- read.csv("LynnwoodEvent.csv")
baseflow_events_events <- read.csv("LynnwoodBF.csv")
baseflow_stats <- read.csv("Lynnwoodstats.csv")
trim_events <- read.csv("LynnwoodTrim.csv")
trim_stats <- read.csv("LynnwoodTrimStats.csv")
regression_df <- read.csv("LynnwoodSummaryStats.csv")
AGWRC_Q_lm <- read.csv("LynnwoodAGWRCRegression.csv")

ggplot(event_df, aes(x = median_flow, y = event_AGWRC)) +
  geom_point(size = 1.5) + 
  labs(x = "Q (cfs)", y = "AGWRC", title = "Median Flow vs. AGWRC - Gage(01628500)") +
  theme_classic()
