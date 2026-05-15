## global.R  ------------------------------------------------------------
#Hydro Configs
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))

# Packages
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(dataRetrieval)
  library(httr)
  library(sqldf)
})

# Small helper (used in server code sometimes)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- source Shiny modules ----
# (Update these paths if your modules are stored elsewhere.)
source("R/nwis_utils.R")
source("R/calc_storage.R")
source("R/add_storage_cols.R")
source("R/get_data.R")

source("modules/droughtModuleUI.R")
source("modules/droughtModuleServer.R")

# =============================================================================
# GitHub configuration + caching
# =============================================================================
BF_GH_OWNER  <- "HARPgroup"
BF_GH_REPO   <- "baseflow_storage"
BF_GH_BRANCH_DEFAULT <- "ben_bf_csvs"

# analyzed CSV name templates (tries in order)
BF_MODEL_TEMPLATES_DEFAULT <- c(
  "model_baseflow_trimmed_stats_{gage_id}.csv"
)

BF_GAGE_TEMPLATES_DEFAULT <- c(
  "baseflow_trimmed_stats_{gage_id}.csv"
)

BF_SUMMARY_TEMPLATES_DEFAULT <- c(
  "baseflow_summary_df_{gage_id}.csv"
)

# cache for analysis dfs and script text
.bf_cache <- new.env(parent = emptyenv())
.sf_script_cache <- new.env(parent = emptyenv())

bf_raw_url <- function(gage_id,
                       path_template,
                       host_site = omsite) {
  stopifnot(!is.null(gage_id), nzchar(gage_id))
  path <- gsub("\\{gage_id\\}", as.character(gage_id), path_template)
  sprintf("%s/usgs/agws/%s", host_site, path)
}

bf_url_exists <- function(url, timeout_sec = 10) {
  resp <- tryCatch(httr::HEAD(url, httr::timeout(timeout_sec)), error = function(e) NULL)
  if (is.null(resp)) return(FALSE)
  httr::status_code(resp) == 200
}

.sf_readlines_cached <- function(url) {
  if (exists(url, envir = .sf_script_cache, inherits = FALSE)) {
    return(get(url, envir = .sf_script_cache, inherits = FALSE))
  }
  txt <- readLines(url, warn = FALSE)
  assign(url, txt, envir = .sf_script_cache)
  txt
}

# =============================================================================
# Analyze CSV loader (used by the app)
# =============================================================================
bf_standardize_analysis_df <- function(df, gage_id) {
  df$site_no <- as.character(gage_id)
  
  if (!("site_name" %in% names(df))) {
    df$site_name <- NA_character_
  }
  
  required <- c("GroupID", "Date", "Flow", "AGWR", "delta_AGWR", "kept", "met_alpha")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Analysis CSV missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # normalize column name variants
  if (!("AGWRC" %in% names(df)) && "trimmed_AGWRC" %in% names(df)) {
    df <- dplyr::rename(df, AGWRC = trimmed_AGWRC)
  }
  if (!("R_squared" %in% names(df)) && "trimmed_R2" %in% names(df)) {
    df <- dplyr::rename(df, R_squared = trimmed_R2)
  }
  if ("R_squared" %in% names(df)) {
    df <- dplyr::rename(df, R2 = R_squared)
  } else if (!("R2" %in% names(df))) {
    df$R2 <- NA_real_
  }
  
  df %>%
    dplyr::mutate(
      Date      = as.Date(Date),
      GroupID   = as.integer(GroupID),
      kept      = as.logical(kept),
      met_alpha = as.logical(met_alpha)
    )
}
#Either read URL or retrieve from cache and return data read from URL and cache_key
read_ows_data <- function(gage_id,
                     kind = c("model", "gage"),
                     templates,
                     use_cache = TRUE) {
  hit_url <- NULL
  hit_template <- NULL
  for (tpl in templates) {
    candidate_url <- bf_raw_url(
      gage_id = gage_id,
      path_template = tpl
    )
    if (bf_url_exists(candidate_url)) {
      hit_url <- candidate_url
      hit_template <- tpl
      break
    }
  }
  
  if (is.null(hit_url)) {
    stop(
      "No analyzed ", kind, " file found for gage_id = ", gage_id, "\n",
      "Tried templates: ", paste(templates, collapse = ", "), "\n"
    )
  }
  
  cache_key <- paste(hit_template, gage_id, kind, sep = "|")
  if (use_cache && exists(cache_key, envir = .bf_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .bf_cache, inherits = FALSE))
  }
  
  df <- readr::read_csv(hit_url, show_col_types = FALSE)
  
  return(
    list(
      df = df,
      cache_key = cache_key
    )
  )
}

bf_get_analysis <- function(gage_id,
                            kind = c("model", "gage"),
                            templates_model = BF_MODEL_TEMPLATES_DEFAULT,
                            templates_gage  = BF_GAGE_TEMPLATES_DEFAULT,
                            use_cache = TRUE) {
  templates <- if (kind == "model") templates_model else templates_gage
  
  ows_results <- read_ows_data(gage_id = gage_id,
                 kind = kind,
                 templates = templates,
                 use_cache = use_cache)
  
  df <- bf_standardize_analysis_df(ows_results$df, gage_id)
  df$analysis_kind <- kind
  
  if (use_cache) assign(ows_results$cache_key, ows_results$df, envir = .bf_cache)
  return(df)
}

# =============================================================================
# Event summary helper for DT + regression
# =============================================================================
make_ben_event_summary <- function(points_df) {
  has_agwrc   <- "AGWRC" %in% names(points_df)
  has_mk_pval <- "mk_pval" %in% names(points_df)
  has_win_len <- "win_len" %in% names(points_df)
  has_short   <- "is_short" %in% names(points_df)
  has_alpha   <- "alpha" %in% names(points_df)
  
  points_df %>%
    dplyr::filter(kept == TRUE, met_alpha == TRUE) %>%
    dplyr::group_by(site_no, site_name, GroupID) %>%
    dplyr::summarise(
      start_date  = min(Date, na.rm = TRUE),
      end_date    = max(Date, na.rm = TRUE),
      n_days      = dplyr::n(),
      median_flow = median(Flow, na.rm = TRUE),
      min_flow    = min(Flow, na.rm = TRUE),
      max_flow    = max(Flow, na.rm = TRUE),
      event_AGWRC = if (has_agwrc) dplyr::first(AGWRC) else NA_real_,
      event_R2    = dplyr::first(R2),
      mk_pval     = if (has_mk_pval) dplyr::first(mk_pval) else NA_real_,
      win_len     = if (has_win_len) dplyr::first(win_len) else NA_real_,
      is_short    = if (has_short) dplyr::first(is_short) else NA,
      alpha       = if (has_alpha) dplyr::first(alpha) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::arrange(start_date)
}

# =============================================================================
# bf_events CSV locator (used for SF_event_summary execution)
# =============================================================================
BF_EVENTS_BRANCH <- "ben_trimming"
BF_EVENTS_TEMPLATES <- c("bf_events_{gage_id}.csv")

bf_get_events_url <- function(gage_id,
                              branch = BF_EVENTS_BRANCH,
                              owner = BF_GH_OWNER,
                              repo  = BF_GH_REPO,
                              templates = BF_EVENTS_TEMPLATES) {
  hit_url <- NULL
  for (tpl in templates) {
    candidate_url <- bf_github_raw_url(
      gage_id = gage_id,
      branch = branch,
      path_template = tpl,
      owner = owner,
      repo = repo
    )
    if (bf_url_exists(candidate_url)) {
      hit_url <- candidate_url
      break
    }
  }
  if (is.null(hit_url)) {
    stop("No bf_events file found for gage_id = ", gage_id, " on branch ", branch,
         ". Tried: ", paste(templates, collapse = ", "))
  }
  hit_url
}

# =============================================================================
# SF_event_summary execution WITHOUT editing colleague file
# =============================================================================
# We execute the colleague script inside an isolated environment, but:
#  - override commandArgs() so args[1:3] are what we want
#  - stub add_model_data() so the script doesn't crash
#  - strip their example args overrides
#  - strip source(...) lines
#  - truncate right after calc_storage(...) so no plotting happens
#
# Output: event_data with Flow_in and Storage_in
run_sf_event_data_from_github <- function(
    event_csv_url,
    land_type_code,
    site_num,
    sf_url = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/SF_event_summary.R",
    calc_storage_url = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/calc_storage.R"
) {
  sf_lines   <- .sf_readlines_cached(sf_url)
  calc_lines <- .sf_readlines_cached(calc_storage_url)
  
  # A) remove example arg overrides (args[1] <- ..., etc.)
  sf_lines <- sf_lines[!grepl("^\\s*args\\[[123]\\]\\s*<-", sf_lines)]
  
  # B) remove any source(...) lines (we control sourcing here)
  sf_lines <- sf_lines[!grepl("^\\s*source\\(", sf_lines)]
  
  # C) truncate after storage calc
  cut_idx <- grep("event_data\\s*<-\\s*calc_storage\\(", sf_lines)
  if (length(cut_idx) == 0) stop("Could not find calc_storage call in SF_event_summary.R (script changed).")
  sf_lines <- sf_lines[seq_len(cut_idx[1])]
  
  # isolated execution env
  env <- new.env(parent = globalenv())
  
  # load calc_storage() into env
  eval(parse(text = paste(calc_lines, collapse = "\n")), envir = env)
  
  # Provide args via BOTH commandArgs() and args
  args_vec <- c(event_csv_url, land_type_code, site_num)
  env$commandArgs <- function(trailingOnly = TRUE) args_vec
  env$args <- args_vec
  
  # Stub add_model_data() so colleague script doesn't crash
  env$add_model_data <- function(df, land_type_code, varname) {
    if (!varname %in% names(df)) df[[varname]] <- NA_real_
    df
  }
  
  # run patched script
  eval(parse(text = paste(sf_lines, collapse = "\n")), envir = env)
  
  if (!exists("event_data", envir = env)) stop("SF_event_summary did not create event_data.")
  event_data <- get("event_data", envir = env)
  
  if (!("Flow_in" %in% names(event_data)))   stop("event_data missing Flow_in after SF_event_summary execution.")
  if (!("Storage_in" %in% names(event_data))) stop("event_data missing Storage_in after calc_storage execution.")
  
  event_data
}
