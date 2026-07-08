# Hydraulic Conductivity (Ksat) Raster Analysis — Recovered Pipeline
# Author: Will Prokopik (recovered + reorganized)
# Last updated: 2026-03-03
#
# PURPOSE
#   Recover and standardize the June–July 2025 workflow discussed in Issue #1427.
#   This script is written to be readable + modular:
#     - Load geometry layers (watersheds, gages, CBP6 land segments, river segments)
#     - Load and prep Ksat raster
#     - Produce: watershed-level stats, gage-buffer stats, landseg stats, riverseg stats
#     - Produce: area-weighted riverseg Ksat via landseg×riverseg intersection
#     - Produce: tables/plots for deliverables
#
# HOW TO USE
#   1) Set PATHS in Section 0 (raster + local GIS files if needed)
#   2) If running on the HARP server, keep basepath='/var/www/R' and config.R sourcing
#   3) Run sections top-to-bottom; each section can be toggled with RUN_* flags
#
# NOTES
#   - This script intentionally avoids relying on objects created in other scripts.
#   - If you want split files later, the section headers are designed to become scripts.

# -----------------------------#
# 0. SETUP
# -----------------------------#

RUN_WShed_Summary      <- TRUE  # raster clipped to each gaged watershed + summary stats + map
RUN_Gage_Buffer        <- TRUE  # 3 km buffer around gage points + summary stats + map
RUN_LandSeg_Ksat       <- TRUE  # Ksat per CBP6 land segment (whole + clipped variants)
RUN_RiverSeg_Ksat      <- TRUE  # Ksat per river segment (unweighted)
RUN_RiverSeg_Weighted  <- TRUE  # area-weighted riverseg Ksat via (landseg stats × intersection area)
RUN_Zone_LandSeg       <- TRUE  # non-overlapping upstream zones & landseg summaries per zone

# Libraries
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(terra)
  library(flextable)
  library(readr)
  library(DBI)
  library(hydrotools)
  library(Hmisc) # for wtd.quantile
})

# If on HARP server:
basepath <- '/var/www/R'
CONFIG_R <- file.path(basepath, 'config.R')
if (file.exists(CONFIG_R)) source(CONFIG_R)

# Paths (edit as needed)
KSAT_RASTER_PATH <- "C://HARPgeneral//GeologyGIS//HydraulicConductivityRaster//cd//raster_data//HydaulicCond.tif"
LOCAL_SHP_NFSHEN <- "C://HARPgeneral//GeologyGIS//HydraulicConductivityRaster//NFShenandoah.shp"  # optional river polygon example

# Output directory
OUTDIR <- "C://Users//willp//OneDrive//Desktop//HARP_testing"
if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)

# Helper: safe write
write_csv_safe <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path)
}

# -----------------------------#
# 1. LOAD RASTER
# -----------------------------#
stopifnot(file.exists(KSAT_RASTER_PATH))
hydraulic_raster <- rast(KSAT_RASTER_PATH)

# Optional: enforce positive-only values (Issue #1427 notes that <=0 and NA should be excluded)
# Keep the raw raster intact; filter at extraction time.
KSAT_FIELD <- names(hydraulic_raster)[1]

# -----------------------------#
# 2. LOAD GEOMETRY LAYERS
# -----------------------------#
# NOTE: these DB pulls assume `ds$connection` exists via config.R.
# If you’re running locally without DB, replace these with local files.

if (!exists("ds")) {
  stop("`ds` not found. If running on server, make sure config.R is sourced and creates ds.")
}

# 2.1 Gages + Watersheds
gages <- dbGetQuery(ds$connection,"
  SELECT * FROM dh_feature_fielded
  WHERE bundle = 'point' AND ftype = 'usgs_gage'
    AND hydrocode IN ('01632000','01633000','01634000')")

gages_sf <- st_as_sf(gages, wkt = "dh_geofield", crs = 4326) %>%
  st_make_valid() %>%
  mutate(Site = case_when(
    hydrocode == '01632000' ~ 'Cootes Store',
    hydrocode == '01633000' ~ 'Mt Jackson',
    hydrocode == '01634000' ~ 'Strasburg',
    TRUE ~ hydrocode
  ))

# Watersheds (full drainage polygons)
wshed_codes <- c("01632000","01633000","01634000")
wsheds <- lapply(wshed_codes, function(hc) {
  rf <- RomFeature$new(ds, config = list(hydrocode = hc, ftype = "usgs_full_drainage"), TRUE)
  st_as_sf(data.frame(hydrocode = hc, wkt = rf$geom), wkt = "wkt", crs = 4326) %>% st_make_valid()
})
wsheds_sf <- bind_rows(wsheds) %>%
  mutate(Site = case_when(
    hydrocode == '01632000' ~ 'Cootes Store',
    hydrocode == '01633000' ~ 'Mt Jackson',
    hydrocode == '01634000' ~ 'Strasburg',
    TRUE ~ hydrocode
  ))

# 2.2 CBP6 Land Segments (full layer)
landsegs <- dbGetQuery(ds$connection,"
  SELECT * FROM dh_feature_fielded
  WHERE bundle = 'landunit' AND ftype = 'cbp6_landseg'")

landsegs_sf <- st_as_sf(landsegs, wkt = "dh_geofield", crs = 4326) %>% st_make_valid()

# 2.3 River segments used in the NF Shenandoah study area (from Issue #1427 examples)
riversegs <- dbGetQuery(ds$connection,"
  SELECT * FROM dh_feature_fielded
  WHERE bundle = 'watershed' AND ftype = 'vahydro' AND hydrocode in
  ('vahydrosw_wshed_PS3_5100_5080',
   'vahydrosw_wshed_PS2_5560_5100',
   'vahydrosw_wshed_PS2_5550_5560')")

riversegs_sf <- st_as_sf(riversegs, wkt = "dh_geofield", crs = 4326) %>% st_make_valid()

# Filter landsegs to those intersecting the study area (optional but speeds things up)
landsegs_clipped <- st_filter(landsegs_sf, riversegs_sf, .predicate = st_intersects)

# Project everything to raster CRS for extraction/intersection
target_crs <- crs(hydraulic_raster)
gages_proj      <- st_transform(gages_sf, target_crs)
wsheds_proj     <- st_transform(wsheds_sf, target_crs)
landsegs_proj   <- st_transform(landsegs_clipped, target_crs)
riversegs_proj  <- st_transform(riversegs_sf, target_crs)

# -----------------------------#
# 3. FUNCTIONS
# -----------------------------#

extract_ksat <- function(r, polys_sf) {
  # Returns long table of raster values per polygon (ID col from terra)
  v <- vect(polys_sf)
  v$poly_id <- 1:nrow(v)
  x <- terra::extract(r, v, ID = TRUE)
  x$poly_id <- v$poly_id[x$ID]
  # Standardize name
  names(x)[names(x) == KSAT_FIELD] <- "Ksat"
  x %>% filter(!is.na(Ksat), Ksat > 0)
}

summarise_ksat <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      mean_ksat   = mean(Ksat),
      p10_ksat    = wtd.quantile(Ksat, weights = rep(1, n()), probs = 0.10),
      p25_ksat    = wtd.quantile(Ksat, weights = rep(1, n()), probs = 0.25),
      median_ksat = wtd.quantile(Ksat, weights = rep(1, n()), probs = 0.50),
      p75_ksat    = wtd.quantile(Ksat, weights = rep(1, n()), probs = 0.75),
      p90_ksat    = wtd.quantile(Ksat, weights = rep(1, n()), probs = 0.90),
      min_ksat    = min(Ksat),
      max_ksat    = max(Ksat),
      sd_ksat     = sd(Ksat),
      n_cells     = n(),
      .groups = "drop"
    )
}

save_flex <- function(ft, filename) {
  flextable::save_as_docx(ft, path = file.path(OUTDIR, filename))
}

# -----------------------------#
# 4. WATERSHED SUMMARY (clip/mask)
# -----------------------------#
if (RUN_WShed_Summary) {
  wshed_stats <- lapply(split(wsheds_proj, wsheds_proj$Site), function(w) {
    w_vect <- vect(w)
    clipped <- crop(hydraulic_raster, w_vect)
    masked  <- mask(clipped, w_vect)
    vals <- values(masked, na.rm = TRUE)
    vals <- vals[vals > 0]
    data.frame(
      Site = unique(w$Site),
      mean_ksat = mean(vals),
      median_ksat = median(vals),
      min_ksat = min(vals),
      max_ksat = max(vals),
      sd_ksat = sd(vals),
      n_cells = length(vals)
    )
  }) %>% bind_rows()

  write_csv_safe(wshed_stats, file.path(OUTDIR, "wshed_ksat_summary.csv"))

  ft <- flextable(wshed_stats) %>% autofit()
  save_flex(ft, "wshed_ksat_summary.docx")
}

# -----------------------------#
# 5. GAGE BUFFER SUMMARY (3 km)
# -----------------------------#
if (RUN_Gage_Buffer) {
  buffer_dist_m <- 3000

  gage_buffers <- gages_proj %>%
    mutate(buffer_m = buffer_dist_m) %>%
    st_buffer(dist = buffer_dist_m)

  buf_vals <- extract_ksat(hydraulic_raster, gage_buffers) %>%
    left_join(gage_buffers %>% st_drop_geometry() %>% mutate(poly_id = 1:n()), by = "poly_id")

  buf_stats <- summarise_ksat(buf_vals, c("Site")) %>%
    mutate(buffer_m = buffer_dist_m)

  write_csv_safe(buf_stats, file.path(OUTDIR, "gage_buffer_ksat_summary.csv"))
  ft <- flextable(buf_stats) %>% autofit()
  save_flex(ft, "gage_buffer_ksat_summary.docx")
}

# -----------------------------#
# 6. LAND SEGMENT KSAT (unweighted)
# -----------------------------#
if (RUN_LandSeg_Ksat) {
  # Ksat per land segment based on raster cells inside each landseg polygon
  landseg_vals <- extract_ksat(hydraulic_raster, landsegs_proj) %>%
    left_join(landsegs_proj %>% st_drop_geometry() %>% transmute(poly_id = 1:n(), landseg_name = name), by = "poly_id")

  landseg_stats <- summarise_ksat(landseg_vals, c("landseg_name")) %>%
    arrange(desc(mean_ksat))

  write_csv_safe(landseg_stats, file.path(OUTDIR, "landseg_ksat_summary.csv"))
  ft <- flextable(landseg_stats) %>% autofit()
  save_flex(ft, "landseg_ksat_summary.docx")
}

# -----------------------------#
# 7. RIVER SEGMENT KSAT (unweighted)
# -----------------------------#
if (RUN_RiverSeg_Ksat) {
  river_vals <- extract_ksat(hydraulic_raster, riversegs_proj) %>%
    left_join(riversegs_proj %>% st_drop_geometry() %>% transmute(poly_id = 1:n(), riverseg_name = name), by = "poly_id")

  riverseg_stats <- summarise_ksat(river_vals, c("riverseg_name")) %>%
    arrange(desc(mean_ksat))

  write_csv_safe(riverseg_stats, file.path(OUTDIR, "riverseg_ksat_summary_unweighted.csv"))
  ft <- flextable(riverseg_stats) %>% autofit()
  save_flex(ft, "riverseg_ksat_summary_unweighted.docx")
}

# -----------------------------#
# 8. RIVER SEGMENT KSAT (AREA-WEIGHTED via landseg×riverseg intersection)
# -----------------------------#
if (RUN_RiverSeg_Weighted) {
  # Pre-req: landseg_stats exists; if not, compute quickly from current session
  if (!exists("landseg_stats")) {
    landseg_vals <- extract_ksat(hydraulic_raster, landsegs_proj) %>%
      left_join(landsegs_proj %>% st_drop_geometry() %>% transmute(poly_id = 1:n(), landseg_name = name), by = "poly_id")
    landseg_stats <- summarise_ksat(landseg_vals, c("landseg_name"))
  }

  inter <- st_intersection(
    landsegs_proj %>% rename(landseg_name = name),
    riversegs_proj %>% rename(riverseg_name = name)
  )
  inter$area_m2 <- as.numeric(st_area(inter))

  inter_stats <- inter %>%
    left_join(landseg_stats %>% select(landseg_name, mean_ksat), by = "landseg_name") %>%
    filter(!is.na(mean_ksat), area_m2 > 0)

  weighted_riverseg <- inter_stats %>%
    group_by(riverseg_name) %>%
    summarise(
      area_m2 = sum(area_m2),
      weighted_mean_ksat = sum(mean_ksat * area_m2) / sum(area_m2),
      n_parts = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(weighted_mean_ksat))

  write_csv_safe(weighted_riverseg, file.path(OUTDIR, "riverseg_ksat_summary_weighted.csv"))
  ft <- flextable(weighted_riverseg) %>% autofit()
  save_flex(ft, "riverseg_ksat_summary_weighted.docx")
}

# -----------------------------#
# 9. NON-OVERLAPPING UPSTREAM ZONES (COOTES vs MTJACK vs STRAS)
# -----------------------------#
if (RUN_Zone_LandSeg) {
  # Build incremental zones (as noted in Issue #1427)
  cootes_zone <- wsheds_proj %>% filter(Site == "Cootes Store")
  mtjack_zone <- st_difference(wsheds_proj %>% filter(Site == "Mt Jackson"), cootes_zone)
  strasb_zone <- st_difference(wsheds_proj %>% filter(Site == "Strasburg"), st_union(mtjack_zone, cootes_zone))

  zones_sf <- bind_rows(
    cootes_zone %>% mutate(zone = "Cootes Store"),
    mtjack_zone %>% mutate(zone = "Mt Jackson (minus Cootes)"),
    strasb_zone %>% mutate(zone = "Strasburg (minus upstream)")
  ) %>% st_make_valid()

  # Intersect zones with landsegs and extract Ksat
  zone_landseg <- st_intersection(zones_sf, landsegs_proj %>% rename(landseg_name = name))
  zone_landseg$int_id <- 1:nrow(zone_landseg)

  zone_vals <- extract_ksat(hydraulic_raster, zone_landseg) %>%
    left_join(zone_landseg %>% st_drop_geometry() %>% transmute(poly_id = 1:n(), zone, landseg_name), by = "poly_id")

  zone_landseg_stats <- summarise_ksat(zone_vals, c("zone","landseg_name")) %>%
    arrange(zone, desc(mean_ksat))

  write_csv_safe(zone_landseg_stats, file.path(OUTDIR, "zone_landseg_ksat_summary.csv"))
  ft <- flextable(zone_landseg_stats) %>% autofit()
  save_flex(ft, "zone_landseg_ksat_summary.docx")
}

message("Recovered Ksat pipeline complete. Outputs written to: ", OUTDIR)
