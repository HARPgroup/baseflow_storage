#' @title monthly_group_count
#' @name
#' monthly_group_count
#' @details
#' creates a df with county of total groups and count of groups per month
#' @param event_df data.frame. A data frame with fields that have, at a
#'   minimumm, group ID and start date. Often derived from step 06 of DEQ bf
#'   workflow
#' @param date_col Character, the name of the field with event start dates
#' @param group_col Character, the name of the field with event group IDs
#' @returns df with columns for month, event_cnt, and gage_total
#' @importFrom rlang .data
#' @export monthly_group_count
monthly_group_count <- function(event_df,
                                date_col = "start_date", group_col = "GroupID"){
  gage_total <- dplyr::n_distinct(event_df[,group_col])
  monthly_event_count <- event_df |>
    dplyr::mutate(
      month = as.numeric(format(as.Date(!!dplyr::sym(date_col)), "%m"))
    ) |>
    dplyr::group_by(.data$month) |>
    dplyr::summarise(event_cnt = .data$n_distinct(!!dplyr::sym(group_col))) |>
    dplyr::mutate(gage_total = gage_total) |>
    as.data.frame()

  return(monthly_event_count)
}
