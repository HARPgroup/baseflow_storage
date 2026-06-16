#### Initialize ####
library(nhdplusTools)


get_basin_area <- function(gage_obj){

  #Outlet coordinates
  brlat <- sf::st_coordinates(gage_obj$gage_data_sf["geometry"])[,2]
  brlon <- sf::st_coordinates(gage_obj$gage_data_sf["geometry"])[,1]
  out_point_br = sf::st_sfc(sf::st_point(c(brlon, brlat)), crs = 4326)
  nhd_out_br <- get_nhdplus(out_point_br)
  return(nhd_out_br$slope)
}

# get basin area


