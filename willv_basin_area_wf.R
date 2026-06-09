library(whitebox)
library(sf)
library(terra)
library(tmap)
library(tidyverse)

options(whitebox.max_print_bytes = 10000000)

gage_data <- gage_obj$gage_data_sf
pp <- st_as_sf(gage_data, wkt = "geometry", crs = 4267) #lynnwood

dem_raw <- rast("Geospatial/shenandoah_30m_dem.tif")
writeRaster(dem_raw, "Geospatial/rawdem.asc", filetype = "AAIGrid", overwrite = TRUE)

states <- st_read("Geospatial/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")


wbt_hillshade("Geospatial/rawdem.asc", "Geospatial/lynnwood_hillshade.tif",
              azimuth = 115)


wbt_breach_depressions_least_cost(
  dem = "Geospatial/rawdem.asc",
  output = "Geospatial/lynnwood_breached.tif",
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = "Geospatial/lynnwood_breached.tif",
  output = "Geospatial/lynnwood_filled_breached.tif"
)

wbt_d8_flow_accumulation(input = "Geospatial/lynnwood_filled_breached.tif",
                         output = "Geospatial/lynnwood_d8_flowacc.tif")

wbt_d8_pointer(dem = "Geospatial/lynnwood_filled_breached.tif",
               output = "Geospatial/d8_pointer.tif")
pp |>
  select(geometry) |>
  st_write("Geospatial/pp.shp", overwrite = TRUE)

wbt_extract_streams(flow_accum = "Geospatial/lynnwood_d8_flowacc.tif",
                    output = "Geospatial/raster_streams.tif",
                    threshold = 6000)

wbt_jenson_snap_pour_points(pour_pts = "Geospatial/pp.shp",
                            streams = "Geospatial/raster_streams.tif",
                            output = "Geospatial/snappedpp.shp",
                            snap_dist = 0.0005)

wbt_watershed(d8_pntr = "Geospatial/d8_pointer.tif",
              pour_pts = "Geospatial/pp.shp",
              output = "Geospatial/lynnwood_ws.tif")

ws <- rast("Geospatial/lynnwood_ws.tif")
streams <- rast("Geospatial/raster_streams.tif")
hillshade <- rast("Geospatial/lynnwood_hillshade.tif")


tm_shape(streams)+
  tm_raster(legend.show = TRUE, palette = "Blues")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)





