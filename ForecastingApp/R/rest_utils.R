
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
    message(candidate_url)
    hit_url <- candidate_url
  }

  if (is.null(hit_url)) {
    stop(
      "No analyzed ", kind, " file found for gage_id = ", gage_id, "\n",
      "Tried templates: ", paste(templates, collapse = ", "), "\n
      and candidate_url was last: ",candidate_url
    )
  }
  
  cache_key <- paste(hit_template, gage_id, kind, sep = "|")
  if (use_cache && exists(cache_key, envir = .bf_cache, inherits = FALSE)) {
    return(
      list(
        df = get(cache_key, envir = .bf_cache, inherits = FALSE),
        cache_key = cache_key
      )
    )
  }
  
  df <- read.csv(hit_url)
  
  return(
    list(
      df = df,
      cache_key = cache_key
    )
  )
}

bf_get_analysis <- function(gage_id,
                            kind = c("model", "gage"),
                            templates_model = NULL,
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

validate_required_cols <- function(df, required, context = "data") {
  missing <- setdiff(required, names(df))
  out <- TRUE
  if (length(missing) > 0) {
    msg <- paste0(
      "Missing required column(s) in ", context, ": ",
      paste(missing, collapse = ", "),
      ".\n\n",
      "Available columns: ", paste(names(df), collapse = ", ")
    )
    showNotification(msg, type = "error", duration = NULL)
    out <- FALSE
  }
  return(out)
}