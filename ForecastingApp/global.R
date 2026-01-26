## global.R

library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(purrr)
library(tibble)
library(dataRetrieval)
library(httr)

# ---- source modules ----
source("modules/droughtModuleUI.R")
source("modules/droughtModuleServer.R")

# ============================================================
# GITHUB-BASED ANALYSIS CSV RETRIEVAL (NO RE-CALC)
# ============================================================
# Current (temporary) location: Ben branch root
#   https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_bf_csvs/bf_events_{gage_id}.csv
#
# Future (main branch): data directory
#   https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/data/bf_events_{gage_id}.csv

BF_GH_OWNER  <- "HARPgroup"
BF_GH_REPO   <- "baseflow_storage"

# ---- defaults for the CURRENT state (Ben branch; file at repo root) ----
BF_GH_BRANCH_DEFAULT <- "ben_bf_csvs"
BF_PATH_TEMPLATE_DEFAULT <- "bf_events_{gage_id}.csv"

# Cache so we don't re-download the same CSV repeatedly
.bf_cache <- new.env(parent = emptyenv())

bf_github_raw_url <- function(gage_id,
                              branch = BF_GH_BRANCH_DEFAULT,
                              path_template = BF_PATH_TEMPLATE_DEFAULT,
                              owner = BF_GH_OWNER,
                              repo  = BF_GH_REPO) {
  stopifnot(!is.null(gage_id), nzchar(gage_id))
  path <- gsub("\\{gage_id\\}", as.character(gage_id), path_template)
  sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s", owner, repo, branch, path)
}

bf_url_exists <- function(url, timeout_sec = 10) {
  # HEAD is fast; treat 200 as "exists"
  resp <- httr::HEAD(url, httr::timeout(timeout_sec))
  httr::status_code(resp) == 200
}

bf_standardize_analysis_df <- function(df, gage_id) {
  # hard-set these so we never lose leading zeros
  df$site_no <- as.character(gage_id)

  # ensure site_name exists for downstream group_by()
  if (!("site_name" %in% names(df))) {
    df$site_name <- NA_character_
  }
  
  # --- required columns check (fail fast with clear error) ---
  required <- c("GroupID", "Date", "Flow", "AGWR", "delta_AGWR", "kept", "met_alpha")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Analysis CSV missing required columns: ", paste(missing, collapse = ", "))
  }

  # --- standardize recession-fit naming ---
  # New files likely: AGWRC + R_squared
  # Old files: trimmed_AGWRC + trimmed_R2
  if (!("AGWRC" %in% names(df)) && "trimmed_AGWRC" %in% names(df)) {
    df <- dplyr::rename(df, AGWRC = trimmed_AGWRC)
  }
  if (!("R_squared" %in% names(df)) && "trimmed_R2" %in% names(df)) {
    df <- dplyr::rename(df, R_squared = trimmed_R2)
  }

  # canonical R2 column used by app
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

# ---- main getter used by the app ----
bf_get_gage_analysis <- function(gage_id,
                                branch = BF_GH_BRANCH_DEFAULT,
                                path_template = BF_PATH_TEMPLATE_DEFAULT,
                                owner = BF_GH_OWNER,
                                repo  = BF_GH_REPO,
                                use_cache = TRUE) {
  url <- bf_github_raw_url(
    gage_id = gage_id,
    branch = branch,
    path_template = path_template,
    owner = owner,
    repo = repo
  )

  cache_key <- paste(owner, repo, branch, path_template, gage_id, sep = "|")

  if (use_cache && exists(cache_key, envir = .bf_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .bf_cache, inherits = FALSE))
  }

  if (!bf_url_exists(url)) {
    stop(
      "No analysis file found for gage_id = ", gage_id, "\n",
      "Expected GitHub raw URL: ", url
    )
  }

  df <- readr::read_csv(url, show_col_types = FALSE)
  df <- bf_standardize_analysis_df(df, gage_id)

  if (use_cache) assign(cache_key, df, envir = .bf_cache)
  df
}

# ============================================================
# EVENT SUMMARY (UNCHANGED)
# ============================================================
make_ben_event_summary <- function(points_df) {
  # Determine if optional columns exist once (no cur_data(), no deprecation)
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
