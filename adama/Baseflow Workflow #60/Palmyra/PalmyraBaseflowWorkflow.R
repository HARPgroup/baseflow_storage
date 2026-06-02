#Palmyra
gageID <- "02034000"

library("devtools")

#Make sure it hasn't been called, but if it has we can unload it

unloadNamespace('hydrotools')

#Get the master branch deployment of the package

devtools::install_github("HARPgroup/hydro-tools")

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "PalmyraGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("PalmyraGage.csv", "obs_date", "obs_flow", FALSE, "PalmyraEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("PalmyraEvent.csv", "obs_date", "obs_flow", "Palmyra", "PalmyraBF.csv", "monitoring_location_id")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("PalmyraBF.csv", "for", "Palmyrastats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("Palmyrastats.csv", "PalmyraTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("PalmyraTrim.csv", "PalmyraTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("PalmyraTrimStats.csv", gageID, "PalmyraSummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("PalmyraSummaryStats.csv", gageID, "PalmyraAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c(gageID, "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "PalmyraAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("PalmyraGage.csv")
event_identification <- read.csv("PalmyraEvent.csv")
baseflow_events_events <- read.csv("PalmyraBF.csv")
baseflow_stats <- read.csv("Palmyrastats.csv")
trim_events <- read.csv("PalmyraTrim.csv")
trim_stats <- read.csv("PalmyraTrimStats.csv")
regression_df <- read.csv("PalmyraSummaryStats.csv")
AGWRC_Q_lm <- read.csv("PalmyraAGWRCRegression.csv")
