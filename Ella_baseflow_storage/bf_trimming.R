#Trim a series of baseflow events
#Local Testing:
# commandArgs <- function(...){
#   c("strasstats.csv", "strasTrim.csv")
# }
# dependencies 
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
library(Kendall)

#set up command Args
argst <- commandArgs(trailingOnly = T)
if (length(argst) < 2) {
  message("This script will take a time series of identified drought events and trim them to remove storm flow events/not baseflow events .")
  message("Use: bf_trimming_analysis.R Drought_event_time_series_data_to_trim output_file ")
  q()
}

csv1_path <- argst[1]
output_file <- argst[2]

#Load data to trim
csv1 <- read.csv(csv1_path)

# load MK trimming function
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/will_mk_trim.R")

#1. Trim the Data with trim_event_mk
csv1_trimmed <- csv1 %>%
  dplyr::group_by(GroupID) %>%
  dplyr::group_modify(~ trim_event_mk(.x, alpha = 0.3)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(kept == TRUE, met_alpha == TRUE)


#2. Export as .csv files
#Save as .csv files
write.csv(csv1_trimmed, file = output_file,
          row.names = FALSE
)




