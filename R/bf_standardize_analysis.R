#'@title bf_standardize_analysis_df
#'@name
#'bf_standardize_analysis_df
#'@description
#'Cehck for missing analysis fields in data.frame
#'@details
#'Input df is checked for specific column names \code{c("GroupID", "Date",
#'"Flow", "AGWR", "delta_AGWR", "kept", "met_alpha")}, required names are stated
#'if missing. Formats these variables with standardized classes by ensuring
#'expected numeric data is numeric, expectedlogical is logical, and expected
#'character is character
#'@param df data.frame to check
#'@param gage_id chararacter. A site name to append to the data if not present.
#'@return data.frame with the columns: Date, GroupID, Flow, AGWR, delta_AGWR,
#'  AGWRC, kept, met_alpha now formatted as character, numeric, or logical as
#'  appropriate
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
