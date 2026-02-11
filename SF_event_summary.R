# Storage And Flow Summaries for input event timeseries data
args <- commandArgs(trailingOnly = T)

suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(sqldf))

# Arg setup for command line
# if (length(args) != 4){
#   message("Missing or extra arguments. Usage: ...")
#   q()
# }

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
event_data <- add_model_data(event_data, land_type_code, "AGWET")

# Checking: ADD AGWO, SURO, and IFWO to calc total_model_flow
event_data <- add_model_data(event_data, land_type_code, "AGWO")
event_data <- add_model_data(event_data, land_type_code, "SURO")
event_data <- add_model_data(event_data, land_type_code, "IFWO")

event_data$tot_model_flow <- event_data$AGWO + event_data$SURO + event_data$IFWO

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

# Summariz3 into groups
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


# Plot Results
ggplot(data = event_sums, mapping = aes(x=100* remainder/Storage_f, y = 100* (AGWET_tot)/Storage_f))+
  geom_point()+
  theme_bw()+
  #coord_cartesian(xlim = c(-1,1), ylim = c(0,0.2))+
  xlab("Remainder (% of Final Storage)")+
  ylab("Total AGWET (% of Final Storage)")+
  ggtitle(paste0("Remainder and Event total ET - ", site_num))+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = event_sums, aes(Storage_f, AGWET_tot)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw()


ggplot() +
  geom_boxplot(data = event_data, aes(x = "USGS", y = Flow_in), color="dodgerblue3") +
  geom_boxplot(data = event_data, aes(x = "Model", y = tot_model_flow), color="firebrick3") +
  xlab("Data Source") +
  ylab("Flow (in/day)") +
  ggtitle(paste0("Flow Value Ranges by Data Source - ", site_num))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

ggplot() +
  geom_boxplot(data = event_data, aes(x = "Model", y = AGWET), color="firebrick3") +
  xlab("Data Source") +
  ylab("AGWET (in/day)") +
  ggtitle(paste0("AGWET Value Range by Data Source - ", site_num))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

check2 <- sqldf("select * from event_data where GroupID = 81")

ggplot(data = check2, mapping = aes(Date, Storage_in))+
  geom_point(color="mediumpurple3")+
  theme_bw()
