# Inputs (args):
# 1 = Gage id you want to use
# 2 = End path of new csv
suppressPackageStartupMessages(library(hydrotools))
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) { #if the length of args is not equal to 2...give the following message
  message("Usage: Rscript usgsdata.R gage_id output_path")
  q() #function quit
}

gage_id <- args[1]
write_path <- args[2]

print("Pull csv from input file path")

gage_obj <- hydrotools::WaterGageDaily$new(gage_id = gage_id)
gage_obj$load_sf_da()

flow_data <- gage_obj$gage_data
print("Extract date information from the gage using lubridate as above")
flow_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(flow_data[,gage_obj$date_col])),
                                         month(as.Date(flow_data[,gage_obj$date_col])),
                                         day(as.Date(flow_data[,gage_obj$date_col])))

#Converts the name for flow from usgs to our generic name of obs_flow
#adds drainage area as a column, to be used in later steps
flow_data <- flow_data |> rename(obs_flow = gage_obj$flow_col)
flow_data <- flow_data |> rename(obs_date = gage_obj$date_col)
flow_data <- flow_data |> mutate(dra = gage_obj$drainage_area)




print(paste0("Write csv in new file path: ",write_path))
write.csv(flow_data,write_path)


