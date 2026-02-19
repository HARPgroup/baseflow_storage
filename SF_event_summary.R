# Storage And Flow Summaries for input event timeseries data
args <- commandArgs(trailingOnly = T)

suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(sqldf))

# Do not use until SSL is fixed
# source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/calc_storage.R")

# args for example
args[1] <- paste0("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_trimming/bf_events_01632000.csv")
args[2] <- paste0("forN51165")
args[3] <- paste0("01632000")

# Convert args
event_data <- read.csv(args[1])
land_type_code <- args[2]
site_num <- args[3]

# Filter only valid data from event data and fix date
event_data <- sqldf(
  "Select * from event_data where kept = TRUE and met_alpha = TRUE"
)

event_data$Date <- as.Date(event_data$Date)

# Add ET data from model
# event_data <- add_model_data(event_data, land_type_code, "AGWET")

# Checking: ADD AGWO, SURO, and IFWO to calc total_model_flow
# event_data <- add_model_data(event_data, land_type_code, "AGWO")
# event_data <- add_model_data(event_data, land_type_code, "SURO")
# event_data <- add_model_data(event_data, land_type_code, "IFWO")

# event_data$tot_model_flow <- event_data$AGWO + event_data$SURO + event_data$IFWO

# Convert flow to watershed in/day instead of cfs to match model units
# Get site-specific drainage area
site <- readNWISsite(site_num)
da_sqmi <- site$drain_area_va

conversion <- (86400*12)/(5280*5280)
sp_conv <- conversion/da_sqmi

# Multiply flows by conversion factor
event_data$Flow_in <- event_data$Flow * sp_conv

# Calculate AGWS equivalent using agwo/1-agwrc
event_data <- calc_storage(event_data, "Flow_in", "AGWRC")

# Summarize into groups, if needed
event_sums <- sqldf("
  with date_range as(
    select GroupID, min(Date) as start_date, max(Date) as end_date
    from event_data
    group by GroupID
  )
  select a.GroupID, a.start_date, a.end_date,
    s_start.Storage_in as Storage_0,
    s_end.Storage_in as Storage_f,
    sum(s.Flow_in) as Flow_tot,
    sum(s.AGWET) as AGWET_tot
  from date_range as a
  join event_data s on s.GroupID=a.GroupID
  left join event_data s_start on( 
    s_start.GroupID = a.GroupID and s_start.Date = a.start_date)
  left join event_data s_end on( 
    s_end.GroupID = a.GroupID and s_end.Date = a.end_date)
  group by a.GroupID
")

# Fix dates and calculate remainder
event_sums$remainder <- (event_sums$Storage_0 - event_sums$Flow_tot - event_sums$Storage_f)
event_sums$start_date <- as.Date(event_sums$start_date)
event_sums$end_date <- as.Date(event_sums$end_date)

