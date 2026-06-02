devtools::install_github("HARPgroup/hydro-tools")


#Millville
gageID <- "01636500"

#Step 01_obtain_flow
commandArgs <- function(...){
  c(gageID, "millGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("millGage.csv", "obs_date", "obs_flow", FALSE, "millEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("millEvent.csv", "obs_date", "obs_flow", "millville", "millBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("millBF.csv", "for", "millstats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("millstats.csv", "millTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("millTrim.csv", "millTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("millTrimStats.csv", "01636500", "millSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("millSummaryStats.csv", "01636500", "millAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c("01636500", "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "millAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("millGage.csv")
event_identification <- read.csv("millEvent.csv")
baseflow_events_events <- read.csv("millBF.csv")
baseflow_stats <- read.csv("millstats.csv")
trim_events <- read.csv("millTrim.csv")
trim_stats <- read.csv("millTrimStats.csv")
regression_df <- read.csv("millSummaryStats.csv")
AGWRC_Q_lm <- read.csv("millAGWRCRegression.csv")
