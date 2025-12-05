# Data Import
mj_trimmed_old <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_trimming/MJ_trimmed_analysis.csv")

mj_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/mount_jackson_event_dataset.csv")

# Do not use until SSL is fixed
# source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/add_model_data.R")

land_type_code <- "forN51171"

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWET")

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWO")

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWS")

# Create event summary data frame
mj_trimmed$Date <- as.Date(mj_trimmed$Date)

event_sums <- sqldf("
  with date_range as(
    select GroupID, min(Date) as start_date, max(Date) as end_date
    from mj_trimmed
    group by GroupID
  )
  select a.GroupID, a.start_date, a.end_date,
    s_start.AGWS as AGWS_0,
    s_end.AGWS as AGWS_f,
    sum(s.AGWO) as AGWO_tot,
    sum(s.AGWET) as AGWET_tot
  from date_range as a
  join mj_trimmed s on s.GroupID=a.GroupID
  left join mj_trimmed s_start on( 
    s_start.GroupID = a.GroupID and s_start.Date = a.start_date)
  left join mj_trimmed s_end on( 
    s_end.GroupID = a.GroupID and s_end.Date = a.end_date)
  group by a.GroupID
")

# Calculate Error
event_sums$error <- event_sums$AGWS_0 - event_sums$AGWO_tot - event_sums$AGWS_f
event_sums$start_date <- as.Date(event_sums$start_date)
event_sums$end_date <- as.Date(event_sums$end_date)


# Plotting sme results
ggplot(data = event_sums, mapping = aes(x=error))+
  geom_boxplot()+
  theme_bw()+
  xlab("Error (in)")+
  ggtitle("Change in Storage - Total Outflow (Model)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = event_sums, mapping = aes(x=error, y = AGWET_tot))+
  geom_point()+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.15), ylim = c(0,0.15))+
  xlab("Error (in)")+
  ylab("AGWET (in)")+
  ggtitle("Error and Event total ET")+
  theme(plot.title = element_text(hjust = 0.5))

# Done with USGS Data ----
# Download trimmed data
mj_trimmed <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_trimming/MJ_trimmed_events_full.csv")

mj_trimmed <- sqldf(
  "Select * from mj_trimmed where kept = TRUE and alpha = 0.3"
  )

mj_trimmed$Date <- as.Date(mj_trimmed$Date)

land_type_code <- "forN51171"

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWET")

# Convert flow to watershed in/day instead of cfs to match model units
# Get site-specific drainage area
site <- readNWISsite("01633000")
da_sqmi <- site$drain_area_va

conversion <- (86400*12)/(5280*5280)
sp_conv <- conversion/da_sqmi

 # Multiply flows by conversion factor
mj_trimmed$Flow_in <- mj_trimmed$Flow * sp_conv

# Calculate AGWS equivalent using agwo/1-agwrc
mj_trimmed$Storage_in <- mj_trimmed$Flow_in/(1-mj_trimmed$trimmed_AGWRC)

# 
event_sums <- sqldf("
  with date_range as(
    select GroupID, min(Date) as start_date, max(Date) as end_date
    from mj_trimmed
    group by GroupID
  )
  select a.GroupID, a.start_date, a.end_date,
    s_start.Storage_in as Storage_0,
    s_end.Storage_in as Storage_f,
    sum(s.Flow_in) as Flow_tot,
    sum(s.AGWET) as AGWET_tot
  from date_range as a
  join mj_trimmed s on s.GroupID=a.GroupID
  left join mj_trimmed s_start on( 
    s_start.GroupID = a.GroupID and s_start.Date = a.start_date)
  left join mj_trimmed s_end on( 
    s_end.GroupID = a.GroupID and s_end.Date = a.end_date)
  group by a.GroupID
")

event_sums$error <- (event_sums$Storage_0 - event_sums$Flow_tot - event_sums$Storage_f)
event_sums$start_date <- as.Date(event_sums$start_date)
event_sums$end_date <- as.Date(event_sums$end_date)


# Plotting sme results
ggplot(data = event_sums, mapping = aes(x=error))+
  geom_histogram(bins = 30, fill="firebrick2", color = "firebrick")+
  theme_bw()+
   #coord_cartesian(xlim = c(-0.25,.25))+
  xlab("Error (in)")+
  ggtitle("Change in Storage - Total Outflow (USGS)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = event_sums, mapping = aes(x=error, y = (AGWET_tot)))+
  geom_point()+
  theme_bw()+
  coord_cartesian(xlim = c(-20,1), ylim = c(0,20))+
  xlab("Remainder (% of Final Storage)")+
  ylab("Total AGWET (% of Final Storage)")+
  ggtitle("Remainder and Event total ET - Mount Jackson")+
  theme(plot.title = element_text(hjust = 0.5))

check <- sqldf(
  "select * from mj_trimmed where GroupID = 73
  "
)

