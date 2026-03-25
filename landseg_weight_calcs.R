# Area weighted AGWRC
library("hydrotools")
library("sqldf")
basepath="/var/www/R"
source("/var/www/R/config.R")

# Get Cootes Store Weights
CS_weight <- sqldf(
  "WITH g AS (
    select a.hydrocode as gage, b.hydrocode as landseg, 
      st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom))
        / st_area2d(a.dh_geofield_geom) as pct_overlaps
    from dh_feature_fielded as a 
    left outer join dh_feature_fielded as b 
    on (
      a.dh_geofield_geom && b.dh_geofield_geom 
      and b.ftype='cbp6_landseg'
    ) 
    where a.hydrocode = 'usgs_ws_01632000' 
      and a.ftype='usgs_full_drainage' 
      and st_area2d(b.dh_geofield_geom) > 0 
      and st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom)) > 0
  ),
  gsum as ( 
    select sum(g.pct_overlaps) as total 
    from g
  ) 
  select g.gage, g.landseg, (g.pct_overlaps / gsum.total) as pct_overlaps
  from g, gsum;
  ",
  connection = ds$connection
)

# Get Mount Jackson Weights
MJ_weight <- sqldf(
  "WITH g AS (
    select a.hydrocode as gage, b.hydrocode as landseg, 
      st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom))
        / st_area2d(a.dh_geofield_geom) as pct_overlaps
    from dh_feature_fielded as a 
    left outer join dh_feature_fielded as b 
    on (
      a.dh_geofield_geom && b.dh_geofield_geom 
      and b.ftype='cbp6_landseg'
    ) 
    where a.hydrocode = 'usgs_ws_01633000' 
      and a.ftype='usgs_full_drainage' 
      and st_area2d(b.dh_geofield_geom) > 0 
      and st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom)) > 0
  ),
  gsum as ( 
    select sum(g.pct_overlaps) as total 
    from g
  ) 
  select g.gage, g.landseg, (g.pct_overlaps / gsum.total) as pct_overlaps
  from g, gsum;
  ",
  connection = ds$connection
)

# Get strasburg weights
SB_weight <- sqldf(
  "WITH g AS (
    select a.hydrocode as gage, b.hydrocode as landseg, 
      st_area2d(st_intersection(b.dh_geofield_geom, a.dh_geofield_geom))
        / st_area2d(a.dh_geofield_geom) as pct_overlaps
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
  ),
  gsum as ( 
    select sum(g.pct_overlaps) as total 
    from g
  ) 
  select g.gage, g.landseg, (g.pct_overlaps / gsum.total) as pct_overlaps
  from g, gsum;
  ",
  connection = ds$connection
)

# Download model parameter data and make first row column names
model_params <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/data/P620171001WQf_model_params.csv")
colnames(model_params)[2] <- "LANDSEG"
colnames(model_params)[18] <- "AGWR"
colnames(model_params) <- model_params[1,]
model_params <- model_params[-1,]
colnames(model_params)[2] <- "LANDSEG"


#get landseg agwrc table
ex <- sqldf("select LANDSEG, AGWR from model_params")
colnames(ex) <- ex[1,]
ex <- ex[-1,]
colnames(ex)[1] <- "LANDSEG"

name <- df("H51165", "N51139", "N51165", "N51171", "N51187", "N51660", "N54031", "N54071")
ex2 <- sqldf("select * from ex where LANDSEG in ('H51165', 'N51139', 'N51165', 'N51171', 'N51187', 'N51660', 'N54031', 'N54071') ")

kableExtra::kable(ex2, format = "markdown")

# select only necessary columns from model params to avoid sql issues
model_params <- model_params |> dplyr::select(LANDSEG, AGWR)

# Add column for gage_name
CS_weight$gage_name <- "Cootes Store"

# match model params onto weight data
CS_weight <- sqldf(
  "select a.*, b.AGWR from CS_weight as a
  inner join model_params as b
  on (a.landseg=b.LANDSEG)
  "
)

CS_weight$AGWR <- as.numeric(CS_weight$AGWR)

# Repeat for other gages
MJ_weight$gage_name <- "Mount Jackson"

MJ_weight <- sqldf(
  "select a.*, b.AGWR from MJ_weight as a
  inner join model_params as b
  on (a.landseg=b.LANDSEG)
  "
)

MJ_weight$AGWR <- as.numeric(MJ_weight$AGWR)

#Strasburg
SB_weight$gage_name <- "Strasburg"

SB_weight <- sqldf(
  "select a.*, b.AGWR from SB_weight as a
  inner join model_params as b
  on (a.landseg=b.LANDSEG)
  "
)

SB_weight$AGWR <- as.numeric(SB_weight$AGWR)

# Multiply AGWR by prct overlap and extract agwr and gage name
CS_weight$AGWR_portion <- CS_weight$pct_overlaps * CS_weight$AGWR

a <- data.frame(CS_weight$gage[1], CS_weight$gage_name[1], sum(CS_weight$AGWR_portion))
colnames(a)<-c("usgs_gage", "gage_name", "w_AGWRC")

# MOunt Jackson
MJ_weight$AGWR_portion <- MJ_weight$pct_overlaps * MJ_weight$AGWR

b <- data.frame(MJ_weight$gage[1], MJ_weight$gage_name[1], sum(MJ_weight$AGWR_portion))
colnames(b)<-c("usgs_gage", "gage_name", "w_AGWRC")

# Strasburg
SB_weight$AGWR_portion <- SB_weight$pct_overlaps * SB_weight$AGWR

c <- data.frame(SB_weight$gage[1], SB_weight$gage_name[1], sum(SB_weight$AGWR_portion))
colnames(c)<-c("usgs_gage", "gage_name", "w_AGWRC")

# Put weighted AGWRC info into its own df
weighted_AGWRC <- data.frame(usgs_gage = character(),
                             gage_name = character(),
                             w_AGWRC = numeric(),
                             stringsAsFactors = FALSE)

weighted_AGWRC <- rbind(weighted_AGWRC,a,b,c)

write.csv(weighted_AGWRC, "C:/Users/ilona/OneDrive - Virginia Tech/HARP/Github/baseflow_storage/data/weighted_AGWRC.csv")
