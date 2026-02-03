#Puts model data into generalized form for event_identification.R
Cootes_Store <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5550_5560_flows_11.csv")
Mount_Jackson <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5560_5100_flows_11.csv")
Strasburg <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS3_5100_5080_flows_11.csv")

library(dplyr)

#reformat names and select important attributes

prep_model_flow <- function(df, site_no, site_name = NULL) {
  
  df_clean <- df %>%
    mutate(
      Date = as.Date(
        paste(year, month, day, sep = "-"),
        format = "%Y-%m-%d"
      ),
      Flow = Qout,
      site_no = site_no,
      site_name = site_name
    ) %>%
    select(Date, Flow, site_no, site_name, area_sqmi)
  
  return(df_clean)
}

Cootes_Store_flow <- prep_model_flow(
  Cootes_Store,
  site_no = "01632000",
  site_name = "Cootes Store"
)

Mount_Jackson_flow <- prep_model_flow(
  Mount_Jackson,
  site_no = "01633000",
  site_name = "Mount Jackson"
)

Strasburg_flow <- prep_model_flow(
  Strasburg,
  site_no = "01634000",
  site_name = "Strasburg"
)

#Save as .csv files

write.csv(
  Cootes_Store_flow,
  "data/CootesStore_model_flow_daily.csv",
  row.names = FALSE
)

write.csv(
  Mount_Jackson_flow,
  "data/MountJackson_model_flow_daily.csv",
  row.names = FALSE
)

write.csv(
  Strasburg_flow,
  "data/Strasburg_model_flow_daily.csv",
  row.names = FALSE
)
