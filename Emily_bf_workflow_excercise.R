library("devtools")
#Make sure it hasn't been called, but if it has we can unload it
unloadNamespace('hydrotools')
#Get the master branch deployment of the package
devtools::install_github("HARPgroup/hydro-tools")

#Roanoke 
gageID <- "02055000"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "RoanokeGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("RoanokeGage.csv", "obs_date", "obs_flow", FALSE, "RoanokeEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("RoanokeEvent.csv", "obs_date", "obs_flow", "Roanoke", "RoanokeBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("RoanokeBF.csv", "for", "Roanokestats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("Roanokestats.csv", "RoanokeTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("RoanokeTrim.csv", "RoanokeTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("RoanokeTrimStats.csv", "02055000", "RoanokeSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("RoanokeSummaryStats.csv", "02055000", "RoanokeAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c("02055000", "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "RoanokeAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("RoanokeGage.csv")
event_identification <- read.csv("RoanokeEvent.csv")
baseflow_events_events <- read.csv("RoanokeBF.csv")
baseflow_stats <- read.csv("Roanokestats.csv")
trim_events <- read.csv("RoanokeTrim.csv")
trim_stats <- read.csv("RoanokeTrimStats.csv")
regression_df <- read.csv("RoanokeSummaryStats.csv")
AGWRC_Q_lm <- read.csv("RoanokeAGWRCRegression.csv")

