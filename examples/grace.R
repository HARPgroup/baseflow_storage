library("terra")
# grace
# this fails with:
#    Error in .rasterObjectFromFile(x, band = band, objecttype = "RasterLayer",  : 
#    Cannot create a RasterLayer object from this file. (file does not exist)
gr2024 <- raster::raster(
  "https://nasagrace.unl.edu/data/20241118/GRACE_GWS_20241118.png"
)
# downloaded manually, with a save from web browser and this works
gr2024 <- raster::raster(
  "C:/Workspace/tmp/GRACE_GWS_20241118.png"
)

plot(gr2024)