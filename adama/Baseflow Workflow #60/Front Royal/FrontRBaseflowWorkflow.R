#Front Royal
gageID <- "01631000"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "FrontRGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("FrontRGage.csv", "obs_date", "obs_flow", FALSE, "FrontREvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("FrontREvent.csv", "obs_date", "obs_flow", "FrontR", "FrontRBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("FrontRBF.csv", "for", "FrontRstats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("FrontRstats.csv", "FrontRTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("FrontRTrim.csv", "FrontRTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("FrontRTrimStats.csv", gageID, "FrontRSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("FrontRSummaryStats.csv", gageID, "FrontRAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c(gageID, "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "FrontRAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("FrontRGage.csv")
event_identification <- read.csv("FrontREvent.csv")
baseflow_events_events <- read.csv("FrontRBF.csv")
baseflow_stats <- read.csv("FrontRstats.csv")
trim_events <- read.csv("FrontRTrim.csv")
trim_stats <- read.csv("FrontRTrimStats.csv")
regression_df <- read.csv("FrontRSummaryStats.csv")
AGWRC_Q_lm <- read.csv("FrontRAGWRCRegression.csv")