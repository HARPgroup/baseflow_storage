#Lynnwood
gageID <- "01628500"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "lynnGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("lynnGage.csv", "obs_date", "obs_flow", FALSE, "lynnEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("lynnEvent.csv", "obs_date", "obs_flow", "lynnwood", "lynnBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("lynnBF.csv", "for", "lynnstats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("lynnstats.csv", "lynnTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("lynnTrim.csv", "lynnTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("lynnTrimStats.csv", "01634000", "lynnSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("lynnSummaryStats.csv", "01634000", "lynnAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c("01634000", "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "lynnAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("lynnGage.csv")
event_identification <- read.csv("lynnEvent.csv")
baseflow_events_events <- read.csv("lynnBF.csv")
baseflow_stats <- read.csv("lynnstats.csv")
trim_events <- read.csv("lynnTrim.csv")
trim_stats <- read.csv("lynnTrimStats.csv")
regression_df <- read.csv("lynnSummaryStats.csv")
AGWRC_Q_lm <- read.csv("lynnAGWRCRegression.csv")