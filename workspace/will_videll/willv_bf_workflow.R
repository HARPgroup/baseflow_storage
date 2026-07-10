#Strasburg
gageID <- "0"

stepd01_args <- function(...){
  c(gageID, paste0("flow_",gageID, ".csv"))
}
stepa01_args <- function(...){
  c(paste0("flow_",gageID, ".csv"), "obs_date", "obs_flow", FALSE, paste0("step1_",gageID, ".csv"))
}
stepa02_args <- function(...){
  c(paste0("step1_",gageID, ".csv"), "obs_date", "obs_flow", "Strasburg", paste0("step2_",gageID, ".csv"), "monitoring_location_id", 14)
}
stepa03_args <- function(...){
  c(paste0("step2_",gageID, ".csv"), "for", paste0("step3_",gageID, ".csv"))
}
stepa04_args <-function(...){
  c(paste0("step3_",gageID, ".csv"), paste0("step4_",gageID, ".csv"))
}
stepa05_args <- function(...){
  c(paste0("step4_",gageID, ".csv"), paste0("step5_",gageID, ".csv"))
}
stepa06_args <- function(...){
  c(paste0("step5_",gageID, ".csv"), gageID, paste0("step6_",gageID, ".csv"))
}
stepa07_args <- function(...){
  c(paste0("step6_",gageID, ".csv"), gageID, paste0("step7_",gageID, ".csv"))
}
stepa08_args <- function(...){
  c(gage_obj, paste0("step6_", gageID, ".csv"), paste0("step1_", gageID, ".csv"), paste0("step8_",gageID, ".csv"), gageID)
}
stepi01_args <- function(...){
  c(gageID, "watershed", "usgs_full_drainage", "AGWRC-1.0", 'simple_lm', paste0("step7_",gageID, ".csv"))
}

commandArgs <- stepd01_args
#Step 01_obtain_flow
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/river/usgsdata.R")
commandArgs <- stepa01_args
#Step 01_event_identification
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/event_identification.R")
commandArgs <- stepa02_args
#Step 02_baseflow_events
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_events.R")
commandArgs <- stepa03_args
#Step 03_baseflow_stats
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_stats.R")
commandArgs <- stepa04_args
#Step 04_trim_events
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming.R")
commandArgs <- stepa05_args
#Step 05_trim_stats
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/bf_trimming_analysis.R")
commandArgs <- stepa06_args
#Step 06_regression_df
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression_df.R")
commandArgs <- stepa07_args
#Step 07_AGWRC_Q_lm
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/baseflow_regression.R")
commandArgs <- stepa08_args
#Step 08_QC_tests
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/willv_bf_workflow/workspace/will_videll/08_qc_tests.R")
commandArgs <- stepi01_args
#Step import -> 01_regression_coeff
source("https://raw.githubusercontent.com/HARPgroup/meta_model/refs/heads/main/scripts/usgs/usgs_post_regression.R")



stepd01_flow <- read.csv(paste0("flow_",gageID, ".csv"))
step01a_eventid <- read.csv(paste0("step1_",gageID, ".csv"))
step02a_BF <- read.csv(paste0("step2_",gageID, ".csv"))
step03a_stats <- read.csv(paste0("step3_",gageID, ".csv"))
step04a_trim <- read.csv(paste0("step4_",gageID, ".csv"))
step05a_trmstats <- read.csv(paste0("step5_",gageID, ".csv"))
step06a_regdf <- read.csv(paste0("step6_",gageID, ".csv"))
step06a_lm <- read.csv(paste0("step7_",gageID, ".csv"))


