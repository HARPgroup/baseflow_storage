library("hydrotools")
library("sqldf")
basepath="/var/www/R"
source("/var/www/R/config.R")
sqldf(
  "select a.hydrocode, b.hydrocode, 
     st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom))
     / st_area2d(b.dh_geofield_geom) 
   from dh_feature_fielded as a 
   left outer join dh_feature_fielded as b 
   on (
     a.dh_geofield_geom && b.dh_geofield_geom 
     and b.ftype='cbp6_landseg'
   ) 
   where a.hydrocode = 'usgs_ws_01634000' 
   and a.ftype='usgs_full_drainage' 
   and st_area2d(b.dh_geofield_geom) > 0 
   and st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom)) > 0
  ",
  connection=ds$connection
)

