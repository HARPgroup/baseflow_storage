#### Packages ####
#library(jsonlite)
#library(geojsonsf)
#library(sf)
#library(dataRetrieval)
#library(dplyr)

#### Local - Function Inputs ####
#state = 'VA'
#UID = "USGS-02016000"
#
#site_geo_test <- dataRetrieval::read_waterdata_monitoring_location(monitoring_location_id = UID)
#site_geo_test$Longitude <- sf::st_coordinates(site_geo_test)[, 1]
#site_geo_test$Latitude  <- sf::st_coordinates(site_geo_test)[, 2]
#site_data_geo_test <- sf::st_drop_geometry(site_geo_test)
#
#latitude = site_data_geo_test$Latitude
#longitude = site_data_geo_test$Longitude

#### Function Start ####

#'@title streamStats_Delineation_single
#'@name
#'streamStats_Delineation_single
#'@description
#'WKT text for POINT and POLYGON of USGS gage
#'@details
#'Using USGS streamStats service API https://streamstats.usgs.gov/ss-delineate/docs#/
#'provides WKT file of USGS gage watershed geometry, including POINT and POLYGON columns
#'@param state Character. Abbreviation of state of USGS gage, e.g. 'VA'
#'@param longitude Numeric. longitude value of USGS gage
#'@param latitude Numeric. latitude value of USGS gage
#'@param UID Character. USGS monitoring_location_id for output df
#' \code{read_waterdata_monitoring_location()}
#'@return A df with list columns of UID, POINT geometry and POLYGON geometry for a USGS gage
#'@importFrom jsonlite fromJSON toJSON
#'@importFrom geojsonsf geojson_wkt
#'@export
streamStats_Delineation_single <- function(state, # StreamStats state info e.g. 'VA'
                                           longitude, # longitude value, numeric
                                           latitude, # latitude value, numeric
                                           UID # Unique station identifier to append to dataset
){
  query <-  paste0('https://streamstats.usgs.gov/ss-delineate/v1/delineate/sshydro/',
                   state, '?lat=', toString(latitude),
                   '&lon=', toString(longitude))

  # Imports df from query URL, attempts to catch any URL errors in import step
  JSON <- tryCatch({
    jsonlite::fromJSON(query, simplifyVector = F, simplifyDataFrame = F)},
    error = function(cond){
      message(paste('StreamStats Error:', cond))
      return(NULL)},
    warning = function(cond){
      message(paste('StreamStats Error:', cond))
      return(NULL)})

  # Recursive loop for finding "FeatureCollection" in JSON file, typically about 5 objects in bedded from input df
  find_FeatureCollection <- function(JSON) {
    outData <- list()

    if (is.list(JSON)) {
      if (!is.null(JSON$type) &&
          JSON$type == "FeatureCollection" &&
          !is.null(JSON$features)) {
        outData <- list(JSON)
      } else {
        for (i in seq_along(JSON)) {
          outData <- c(outData, find_FeatureCollection(JSON[[i]]))
        }
      }
    }

    return(outData)
  }

  fcs <- find_FeatureCollection(JSON)

  # Convert from GEOJSON to WKT with POINT and POLYGON geometry
  fcs_final <- unlist(lapply(fcs, function(fc) {
    geojsonsf::geojson_wkt(
      jsonlite::toJSON(fc, auto_unbox = T)
    )$geometry
  }))

  # final df
  WKT <- data.frame(monitoring_location_id = UID, gage_point = fcs_final[1], gage_drainage_polygon = fcs_final[2])

  return(WKT)
}

#### Multi-call Function Start ####

#'@title streamStats_Delineation
#'@name
#'streamStats_Delineation
#'@description
#'WKT text for POINT and POLYGON of multiple USGS gage
#'@details
#'Using USGS streamStats service API https://streamstats.usgs.gov/ss-delineate/docs#/
#'provides WKT file of multiple USGS gage watershed geometries, including POINT and POLYGON columns
#'@param state Character. Abbreviation of state of USGS gage, e.g. 'VA'
#'@param longitude Numeric vector. longitude value of each USGS gage
#'@param latitude Numeric vector. latitude value of each USGS gage
#'@param UID Character vector. USGS monitoring_location_id for output df
#' \code{read_waterdata_monitoring_location()}
#'@return A df with list columns of UID, POINT geometry and POLYGON geometry for all input USGS gages
#'@importFrom jsonlite fromJSON toJSON
#'@importFrom geojsonsf geojson_wkt
#'@export
streamStats_Delineation <- function(# accepts multiple
  state, # StreamStats state info e.g. 'VA'
  longitude, # longitude value, numeric
  latitude, # latitude value, numeric
  UID # Unique station identifier to append to dataset
){

  # Empty df for length of input df
  GageData <- data.frame(matrix(nrow = length(longitude), ncol = 0))

  # indexing loop for length of input df, runs streamStats_Delination_single for length
  for(i in 1:length(longitude)){
    print(paste('Delineating Site:',i, 'of', length(longitude)))

    dat <- streamStats_Delineation_single(state= 'VA',
                                          longitude = longitude[i],
                                          latitude = latitude[i],
                                          UID = UID[i])

    # Updates df for each index
    GageData$mointoring_location[i] <- dat[1]
    GageData$gage_point[i] <- dat[2]
    GageData$gage_drainage_polygon[i] <- dat[3]
  }

  return(GageData)
}

#### Local - Testing Single ####
#test1 <- streamStats_Delineation_single(state = state, longitude = longitude, latitude = latitude, UID = UID)

#### Local - Testing Multi ####
#state <- "VA"
#states <- "Virginia"
#
#state_sta <- read_waterdata_ts_meta(
#  state_name        = states,
#  parameter_code    = "00060",
#  statistic_id      = "00003",
#  skipGeometry      = TRUE
#)
#
#state_sta <- state_sta |>
#  filter(grepl("USGS-", monitoring_location_id))
#
#site_geo <- dataRetrieval::read_waterdata_monitoring_location(monitoring_location_id = state_sta$monitoring_location_id)
#
#site_geo$longitude <- sf::st_coordinates(site_geo)[, 1]
#site_geo$latitude  <- sf::st_coordinates(site_geo)[, 2]
#
#site_geo <- site_geo |>
#  dplyr::select(monitoring_location_id, latitude, longitude)
#
#USGS_VA <- streamStats_Delineation(state, site_geo$longitude, site_geo$latitude, site_geo$monitoring_location_id)
#
#test <- data.frame(lapply(USGS_VA, as.character), stringsAsFactors = FALSE)
#
#write.csv(test, file = "All_USGS_VA.csv", row.names = F)

