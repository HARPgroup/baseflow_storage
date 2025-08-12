##OPTIONAL: PLOT A RECESSION GROUP##
plot_recession_group <- function(flows_df, recession_df, group_id, site_name = "") {
  event <- recession_df %>% filter(GroupID == group_id)
  if (nrow(event) == 0) stop("Group ID not found.")
  
  start_date <- event$StartDate
  end_date   <- event$EndDate
  window_start <- start_date - 30
  window_end   <- end_date + 30
  
  subset_df <- flows_df %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(InGroup = RecessionDay & GroupID == group_id)
  
  ggplot(subset_df, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +
    geom_point(data = subset_df %>% filter(InGroup), aes(x = Date, y = Flow), color = "red", size = 0.75) +
    labs(
      title = paste(site_name, "- Recession Group", group_id),
      subtitle = paste("From", format(start_date), "to", format(end_date)),
      x = "Date", y = "Flow (CFS)"
    ) +
    theme_minimal()
}
#cootes store, GroupID ___ example
plot_recession_group(
  flows_df    = results$CS$df,
  recession_df = results$CS$summary,
  group_id    = 12,
  site_name   = "Cootes Store"
)

##COMPUTE IQR##
AGWR_summary_CS <- analysis_CS %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )

AGWR_summary_MJ <- analysis_MJ %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )

AGWR_summary_S <- analysis_S %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )
#merge
results$CS$summary <- left_join(results$CS$summary, AGWR_summary_CS, by = "GroupID")
results$MJ$summary <- left_join(results$MJ$summary, AGWR_summary_MJ, by = "GroupID")
results$S$summary  <- left_join(results$S$summary,  AGWR_summary_S,  by = "GroupID")

##TRYING TO DO PLOT-BATCH AUTOMATION##
batch_plot_recessions <- function(flows_df, summary_df, site_name, site_abbr,
                                  iqr_threshold = 0.05, min_duration = 14) {
  # Only keep events that have IQR column (computed during earlier quantile summary)
  if (!"iqr" %in% names(summary_df)) {
    stop("summary_df must include 'iqr' column.")
  }
  
  filtered <- summary_df %>%
    filter(Duration >= min_duration, iqr < iqr_threshold)
  
  message("Found ", nrow(filtered), " events for ", site_name)
  
  for (gid in filtered$GroupID) {
    p <- plot_recession_group(
      flows_df    = flows_df,
      recession_df = summary_df,
      group_id    = gid,
      site_name   = site_name
    )
    
    ggsave(
      filename = paste0("Recession_Plots/", site_abbr, "_Group_", gid, ".jpg"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300
    )
  }
}

batch_plot_recessions(results$CS$df, results$CS$summary, "Cootes Store", "CS")
batch_plot_recessions(results$MJ$df, results$MJ$summary, "Mount Jackson", "MJ")
batch_plot_recessions(results$S$df,  results$S$summary,  "Strasburg",     "S")

##COMBINED AGWR SUMMARY##
combined_events <- bind_rows(
  results$CS$analysis %>% mutate(site = "CS"),
  results$MJ$analysis %>% mutate(site = "MJ"),
  results$S$analysis  %>% mutate(site = "S")
)
summary(combined_events$AGWR)