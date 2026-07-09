# Automatically Download USGS Drought Forecast

# install.packages("chromote")
# install.packagse("arrow")
library(chromote)
library(arrow)

### This might have to change depending on whos running it
download_folder <- "C:\\HARP\\baseflow_storage\\USGS Forecast"  

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
Sys.sleep(5) 
session$close()

### Sort files to get most recent file
files <- list.files(path = "C:\\HARP\\baseflow_storage\\USGS Forecast", pattern = "\\.parquet$", full.names = TRUE)
most_recent_file <- files[which.max(file.info(files)$mtime)]

# Write csv
csv_name <- paste0("USGS_rc_", Sys.Date() - 1, ".csv")

read_parquet(most_recent_file) |> 
  write_csv_arrow(csv_name)

# push csv to github

