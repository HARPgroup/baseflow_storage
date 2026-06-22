## global.R  ------------------------------------------------------------
#Hydro Configs
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))

# Packages
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(plotly)
  library(DT)
  library(dataRetrieval)
  library(httr)
  library(sqldf)
  #For getting data from R server
#  library(DEQmethods)
  library(pins)
  #Spatial
  library(sf)
})


# ---- source Shiny modules ----
# (Update these paths if your modules are stored elsewhere.)
source("R/calc_storage.R")
source("R/add_storage_cols.R")
source("R/drought_misc_utils.R")
source("R/rest_utils.R")
source("R/forwardForecastv2.R")

source("modules/droughtModuleUI.R")
source("modules/droughtModuleServer.R")

# use API key to register board
# deqBoard <- DEQmethods::pinsConnect("PROD")


#### Get USGS Gage Pin ####
all_usgs_gages <- read.csv("data/usgs.csv", colClasses = c("site_no" = "character")) 
#Convert to sf using NAD 83 projection
all_usgs_gages <- st_as_sf(all_usgs_gages, coords = c("dec_long_va", "dec_lat_va"),
                           crs = 4269)
all_usgs_gages <- st_transform(all_usgs_gages,4326)

# =============================================================================
# GitHub configuration + caching
# =============================================================================
# analyzed CSV name templates (tries in order)
BF_GAGE_TEMPLATES_DEFAULT <- c(
  "baseflow_trimmed_stats_{gage_id}.csv"
)

BF_SUMMARY_TEMPLATES_DEFAULT <- c(
  "baseflow_summary_df_{gage_id}.csv"
)

# cache for analysis dfs and script text
.bf_cache <- new.env(parent = emptyenv())
