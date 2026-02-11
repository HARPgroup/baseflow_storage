suppressPackageStartupMessages(library(dplyr))
#argst <- c("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5550_5560_flows_11.csv", " Cootes_Store", "01632000", "Cootes_store_model_flow_daily.csv")

argst <- commandArgs(trailingOnly = T)
if (length(argst) < 4) {
  message("This script will modify a file by changing Qout to flow and changing the date to the correct format.")
  message("Use: model_outflow_calculator.R original_model_data_time_series_daily site_name site_no output_file ")
  q()
}
csv1_path <- argst[1]
site_name <- argst[2]
site_no <- argst[3]
output_file <- argst[4]

#Puts model time series daily data into generalized form for event_identification.R
csv1 <- read.csv(csv1_path)


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

csv1 <- prep_model_flow(
  csv1,
  site_no = site_no,
  site_name = site_name
)



#Save as .csv files
write.csv(csv1, file = output_file,
    row.names = FALSE
)

  
