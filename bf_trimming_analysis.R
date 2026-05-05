# dependencies 
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(ggplot2))


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
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/will_mk_trim.R")

#load bf_event_stats
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_function_cleanup/bf_event_stats.R")

#1. Trim the Data with trim_event_mk
csv1_trimmed <- csv1 %>%
  group_by(GroupID) %>%
  group_modify(~ trim_event_mk(.x, alpha = 0.3)) %>%
  ungroup() %>%
  filter(kept == TRUE, met_alpha == TRUE)


#2. Apply bf_event_stats to determine post trimming values of AGWRC and R Squared
csv1_event_stats <- csv1_trimmed %>%
  group_by(GroupID) %>%
  group_split() %>%
  map_df(~ {
    res <- bf_event_stats(.x)
    .x %>% mutate(
      AGWRC = res$AGWRC,
      R_squared = res$R_squared
    )
  }) %>%
  ungroup()


#3. Filter for AGWRC values < 1 introduced by trimming
tol <- 1e-8

csv1_bf_events <- csv1_event_stats %>%
  filter(AGWRC < 1 - tol)


#4. Export as .csv files
#Save as .csv files
write.csv(csv1_bf_events, file = output_file,
          row.names = FALSE
)




