# --- dependencies ---
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

#load original data
CS_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/CS_original_analysis_df.csv"
)

S_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/S_original_analysis_df.csv"
)

MJ_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/MJ_original_analysis_df.csv"
)

# load MK trimming function
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/will_mk_trim.R")

#load bf_event_stats
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_function_cleanup/bf_event_stats.R")

#1. Trim the Data with trim_event_mk
CS_trimmed <- CS_original_analysis_df %>%
  group_by(GroupID) %>%
  group_modify(~ trim_event_mk(.x, alpha = 0.3)) %>%
  ungroup() %>%
  filter(kept == TRUE, met_alpha == TRUE)

MJ_trimmed <- MJ_original_analysis_df %>%
  group_by(GroupID) %>%
  group_modify(~ trim_event_mk(.x, alpha = 0.3)) %>%
  ungroup() %>%
  filter(kept == TRUE, met_alpha == TRUE)

S_trimmed <- S_original_analysis_df %>%
  group_by(GroupID) %>%
  group_modify(~ trim_event_mk(.x, alpha = 0.3)) %>%
  ungroup() %>%
  filter(kept == TRUE, met_alpha == TRUE)
                
#2. Apply bf_event_stats to determine post trimming values of AGWRC and Rsquared
CS_event_stats <- CS_trimmed %>%
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

MJ_event_stats <- MJ_trimmed %>%
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

S_event_stats <- S_trimmed %>%
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

bf_events_01632000 <- CS_event_stats %>%
  filter(AGWRC < 1 - tol)

bf_events_01633000 <- MJ_event_stats %>%
  filter(AGWRC < 1 - tol)

bf_events_01634000 <- S_event_stats %>%
  filter(AGWRC < 1 - tol)

#4. Export as .csv files

write.csv(bf_events_01632000, file ="bf_events_01632000.csv", row.names = FALSE )
write.csv(bf_events_01633000, file ="bf_events_01633000.csv", row.names = FALSE )
write.csv(bf_events_01634000, file ="bf_events_01634000.csv", row.names = FALSE )



