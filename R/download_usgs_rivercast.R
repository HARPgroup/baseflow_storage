
args <- commandArgs(trailingOnly = TRUE)
pathout <- args[1]
# pathout <- "C:/Users/gcw73279.COV/Desktop/gitBackups/gitHome/rivercast/"
# Automatically Download USGS Drought Forecast

# install.packages("chromote")
# install.packagse("arrow")
library(chromote)
library(arrow)

### This might have to change depending on whos running it
download_folder <- tempdir()

session <- ChromoteSession$new()

# Set the download settings
session$Browser$setDownloadBehavior(
  behavior = "allow", 
  downloadPath = download_folder
)

# Opens website
session$Page$navigate("https://water.usgs.gov/vizlab/streamflow-drought-forecasts/?extent=Virginia#6.7/38.018/-79.459")
Sys.sleep(3) 

# Bypass 'Get Started' button
session$Runtime$evaluate(expression = "document.querySelector('#access-button').click()")
Sys.sleep(3) 

# Downloads USGS Forecast PARQUET file
session$Runtime$evaluate(expression = "document.querySelector('#download-forecasts > span').click()")

# Allows download to finish before closing
csv_name <- paste0(pathout,"USGS_rc_", Sys.Date() - 1, ".csv")
message("Download folder is ",download_folder)
message("Writing to ",csv_name)
Sys.sleep(5)
session$close()

### Sort files to get most recent file
files <- list.files(path = download_folder, pattern = "\\.parquet$", full.names = TRUE)
most_recent_file <- files[which.max(file.info(files)$mtime)]

dataIn <- read_parquet(most_recent_file)
message("Found ",nrow(dataIn), " rows")
Sys.sleep(5)
# Write csv
write_csv_arrow(dataIn, csv_name)

# push csv to github