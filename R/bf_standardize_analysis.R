#'@title bf_standardize_analysis_df
#'@name
#'bf_standardize_analysis_df
#'@description
#'Standardized column names
#'@details
#'Input df is checked for specific column names, required names are stated if missing.
#'Outputs new df with standardized naming convention for common variables.
#'@param df df from locationTrimStats.csv requiring GroupID, Date, Flow, AGWR, delta_AGWR, kept, met_alpha columns
#'@param gage_id char usgs gage number from commandArgs in function call
#'@return df with columns: Date, GroupID, Flow, AGWR, delta_AGWR, AGWRC, kept, met_alpha
#'@importFrom rlang .data
#'@export
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

  if (!("AGWRC" %in% names(df)) && "trimmed_AGWRC" %in% names(df)) {
    df <- dplyr::rename(df, AGWRC = .data$trimmed_AGWRC)
  }

  if (!("AGWRC" %in% names(df))) {
    stop("Analysis CSV does not contain AGWRC or trimmed_AGWRC.")
  }

  out <- df |>
    mutate(
      Date = as.Date(.data$Date),
      GroupID = as.integer(.data$GroupID),
      Flow = as.numeric(.data$Flow),
      AGWR = as.numeric(.data$AGWR),
      delta_AGWR = as.numeric(.data$delta_AGWR),
      AGWRC = as.numeric(.data$AGWRC),
      kept = as.logical(.data$kept),
      met_alpha = as.logical(.data$met_alpha)
    )
  return(out)
}
