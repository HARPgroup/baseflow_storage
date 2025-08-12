require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)  

###FUNCTION 1: ENSURE REVIEW COLUMNS EXIST###
ensure_review_columns <- function(summary_df) {
  n <- NROW(summary_df)
  if (!"review_checklist" %in% names(summary_df)) {
    summary_df$review_checklist <- vector("list", n)
  }
  if (!"overall_review" %in% names(summary_df)) {
    summary_df$overall_review <- rep(NA_character_, n)
  }
  summary_df
}

###FUNCTION 2: BUILD EVENT WINDOW & THRESHOLD FLAGS###
event_window <- function(df, start_date, end_date, buffer_days = 5,
                         agwr_lt = 1.0, dagwr_min = 0.97, dagwr_max = 1.03) {
  buffer_start <- start_date - buffer_days
  buffer_end   <- end_date
  
  df %>%
    filter(Date >= buffer_start & Date <= buffer_end) %>%
    mutate(
      AGWR_flag = case_when(
        AGWR < agwr_lt & delta_AGWR >= dagwr_min & delta_AGWR <= dagwr_max ~ "In Threshold",
        TRUE ~ "Out of Threshold"
      ),
      threshold_flag = ifelse(AGWR_flag == "In Threshold", "In Threshold", "Out of Threshold")
    )
}

###FUNCTION 3: FLOW PLOT BUILDER###
build_flow_plot <- function(data, start_date, group_id) {
  ggplot(data, aes(x = Date)) +
    geom_line(aes(y = Flow)) +
    geom_point(aes(y = Flow, color = threshold_flag), size = 2) +
    geom_vline(xintercept = as.numeric(start_date), linetype = "dotted") +
    scale_color_manual(
      name = "Flow Point Status",
      values = c("In Threshold" = "forestgreen", "Out of Threshold" = "red")
    ) +
    labs(
      title = paste("Flow during Recession Event", group_id),
      y = "Flow (CFS)", x = "Date"
    ) +
    ylim(0, NA) +
    theme_minimal()
}

###FUNCTION 4: AGWR / dAGWR PLOT BUILDER###
build_agwr_plot <- function(data, start_date, group_id) {
  data <- data %>%
    mutate(
      AGWR_flag2 = ifelse(AGWR < 1.0, "AGWR In", "AGWR Out"),
      d_flag2    = ifelse(delta_AGWR >= 0.97 & delta_AGWR <= 1.03, "dAGWR In", "dAGWR Out")
    )
  
  ggplot(data, aes(x = Date)) +
    geom_line(aes(y = AGWR), linetype = "dashed") +
    geom_point(aes(y = AGWR, shape = AGWR_flag2, color = AGWR_flag2), size = 2, stroke = 1) +
    geom_line(aes(y = delta_AGWR), linetype = "dotted") +
    geom_point(aes(y = delta_AGWR, shape = d_flag2, color = d_flag2), size = 2, stroke = 1) +
    geom_hline(yintercept = 1.0) +
    geom_hline(yintercept = c(0.97, 1.03), linetype = "dashed") +
    geom_vline(xintercept = as.numeric(start_date), linetype = "dotted") +
    scale_color_manual(
      name = "Threshold Status",
      values = c(
        "AGWR In" = "blue", "AGWR Out" = "blue",
        "dAGWR In" = "orange", "dAGWR Out" = "orange"
      )
    ) +
    scale_shape_manual(
      name = "Threshold Status",
      values = c("AGWR In" = 16, "AGWR Out" = 1, "dAGWR In" = 15, "dAGWR Out" = 0)
    ) +
    labs(
      title = paste("AGWR + delta_AGWR – Event", group_id),
      y = "AGWR / delta_AGWR", x = "Date"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

###FUNCTION 5: SUMMARY TEXT###
event_summary_text <- function(start_date, end_date, agwr_val, r2_val) {
  duration <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
  paste0(
    "<b>Start Date:</b> ", start_date, "<br/>",
    "<b>End Date:</b> ", end_date, "<br/>",
    "<b>Duration:</b> ", duration, " days<br/>",
    "<b>Calculated AGWR:</b> ",
    ifelse(!is.na(agwr_val), sprintf("%.3f", agwr_val), "Unavailable"), "<br/>",
    "<b>R-Squared:</b> ",
    ifelse(!is.na(r2_val), sprintf("%.3f", r2_val), "Unavailable")
  )
}

###FUNCTION 6: SUMMARY FOOTER TEXT###
event_summary_footer <- function(gid, start_date, end_date, agwr_val, r2_val) {
  duration <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
  paste0(
    "Event ", gid, "\n",
    "Start Date: ", start_date, "\n",
    "End Date: ", end_date, "\n",
    "Duration: ", duration, " days\n",
    "Calculated AGWR: ",
    ifelse(!is.na(agwr_val), sprintf("%.3f", agwr_val), "Unavailable"), "\n",
    "R-Squared: ",
    ifelse(!is.na(r2_val), sprintf("%.3f", r2_val), "Unavailable")
  )
}

