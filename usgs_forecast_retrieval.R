# Automatically Download USGS Drought Forecast

# install.packages("chromote")
# install.packagse("arrow")
library(chromote)
library(arrow)

# This might have to change depending on whos running it
download_folder <- "C:\\HARP\\baseflow_storage\\USGS Forecast"  

b <- ChromoteSession$new()

# Set the download settings
b$Browser$setDownloadBehavior(
  behavior = "allow", 
  downloadPath = download_folder
)

# Opens website
b$Page$navigate("https://water.usgs.gov/vizlab/streamflow-drought-forecasts/?extent=Virginia#6.7/38.018/-79.459")
Sys.sleep(3) 

# Bypass 'Get Started' button
b$Runtime$evaluate(expression = "document.querySelector('#access-button').click()")
Sys.sleep(3) 

# Downloads USGS Forecast PARQUET file
b$Runtime$evaluate(expression = "document.querySelector('#download-forecasts > span').click()")

# Allows download to finish before closing
Sys.sleep(5) 
b$close()

### -----------------Read in and filter to our sites---------------------- ###

gages <- c("01628500", "01629500", "01631000", "01636500", "02055000", "02024000", "02034000")

usgs_fc <- read_parquet("C:\\HARP\\baseflow_storage\\USGS Forecast\\USGS_streamflow_drought_forecasts_2026-06-10.parquet") |> 
  filter(StaID == gages)

