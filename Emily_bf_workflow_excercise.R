#Roanoke River At Roanoke
gageID <- "01634000"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "Roanoke.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("RoanokeGage.csv", "obs_date", "obs_flow", FALSE, "RoanokeEvent.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("strasEvent.csv", "obs_date", "obs_flow", "Strasburg", "strasBF.csv", "monitoring_location_id")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("strasBF.csv", "for", "strasstats.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("strasstats.csv", "strasTrim.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("strasTrim.csv", "strasTrimStats.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("strasTrimStats.csv", "01634000", "strasSummaryStats.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("strasSummaryStats.csv", "01634000", "strasAGWRCRegression.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c("01634000", "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "strasAGWRCRegression.csv")
}
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/meta_model/scripts/usgs/usgs_post_regression.R")



flow_data <- read.csv("strasGage.csv")
event_identification <- read.csv("strasEvent.csv")
baseflow_events_events <- read.csv("strasBF.csv")
baseflow_stats <- read.csv("strasstats.csv")
trim_events <- read.csv("strasTrim.csv")
trim_stats <- read.csv("strasTrimStats.csv")
regression_df <- read.csv("strasSummaryStats.csv")
AGWRC_Q_lm <- read.csv("strasAGWRCRegression.csv")

