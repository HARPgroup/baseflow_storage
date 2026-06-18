library("devtools")
#Make sure it hasn't been called, but if it has we can unload it
unloadNamespace('hydrotools')
#Get the master branch deployment of the package
devtools::install_github("HARPgroup/hydro-tools")

#Luray, VA
gageID <- "01629500"

#Step 01_obtain_flow 
commandArgs <- function(...){
  c(gageID, "LurayGage.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")

#Step 01_event_identification
commandArgs <- function(...){
  c("LurayGage.csv", "obs_date", "obs_flow", FALSE, "LurayEvent.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")

#Step 02_baseflow_events
commandArgs <- function(...){
  c("LurayEvent.csv", "obs_date", "obs_flow", "Luray", "LurayBF.csv", "monitoring_location_id", 14)
}

source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")

#Step 03_baseflow_stats
commandArgs <- function(...){
  c("LurayBF.csv", "for", "Luraystats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")


#Step 04_trim_events
commandArgs <- function(...){
  c("Luraystats.csv", "LurayTrim.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")

#Step 05_trim_stats
commandArgs <- function(...){
  c("LurayTrim.csv", "LurayTrimStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")


#Step 06_regression_df
commandArgs <- function(...){
  c("LurayTrimStats.csv", "01629500", "LuraySummaryStats.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")

#Step 07_AGWRC_Q_lm
commandArgs <- function(...){
  c("LuraySummaryStats.csv", "01629500", "LurayAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")

#Step import -> 01_regression_coeff
commandArgs <- function(...){
  c("01629500", "watershed", "usgs_full_drainage", "agwrc-1.0", 'simple_lm', "LurayAGWRCRegression.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")

#Step 08_qc
commandArgs <- function(...){
  c(gage_obj, "LuraySummaryStats.csv", "LurayQC.csv")
}
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/R/08_qc_tests.R")

#Step 09_iterative_lm
commandArgs <- function(...){
  c("LuraySummaryStats.csv", "LurayIterativeLm.csv", "LurayHighestRSquared")
}
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/Ella_bf_workflow/Ella_baseflow_storage/iterative_lm.R")

flow_data <- read.csv("LurayGage.csv")
event_identification <- read.csv("LurayEvent.csv")
baseflow_events_events <- read.csv("LurayBF.csv")
baseflow_stats <- read.csv("Luraystats.csv")
trim_events <- read.csv("LurayTrim.csv")
trim_stats <- read.csv("LurayTrimStats.csv")
regression_df <- read.csv("LuraySummaryStats.csv")
AGWRC_Q_lm <- read.csv("LurayAGWRCRegression.csv")

#Create plot showing regression coefficient on y and logQ on x
#Create new column that has the log of flow
event_df <- event_df %>%
  mutate(LogQ = log(median_flow))

ggplot(event_df, aes(x = LogQ, y = event_AGWRC)) +
  geom_abline(slope = -0.01573969, intercept = 1.078166, col = "red") + 
  geom_point() + 
  labs(x = "Log Median Flow (cfs)",
       y = "AGWRC",
       title = "AGWRC vs Log Median Flow for South Fork Shenandoah River near Luray, VA")






