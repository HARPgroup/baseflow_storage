library("terra")
# grace
grf <- rast("https://nasagrace.unl.edu/data/20241118/GRACE_GWS_20241118.png")
GDALinfo(grf)
terra::describe("https://nasagrace.unl.edu/data/20241118/GRACE_GWS_20241118.png")

gr2024 <- raster::raster(
  "https://nasagrace.unl.edu/data/20241118/GRACE_GWS_20241118.png"
)
gr2024 <- raster::raster(
  "https://nasagrace.unl.edu/data/20241118/gws_perc_0125deg_US_20241118.tif"
)
gr2024 <- raster::raster(
  "https://nasagrace.unl.edu/data/20241118/GRACE_GWS_20241118.png"
)
# downloaded manually and this works
gr2024 <- raster::raster(
  "C:/Workspace/tmp/GRACE_GWS_20241118.png"
)

plot(gr2024)