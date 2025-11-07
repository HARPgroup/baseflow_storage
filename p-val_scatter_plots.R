library(dplyr)
library(ggplot2)

# List of gages and their trimmed data frames
trimmed_list <- list(
  "Cootes Store" = CS_trimmed_analysis_df,
  "Strasburg"    = S_trimmed_analysis_df,
  "Mount Jackson"= MJ_trimmed_analysis_df
)

# Prepare data
trimmed_results <- lapply(names(trimmed_list), function(site) {
  df <- trimmed_list[[site]] %>%
    mutate(site_name = site) %>%
    select(site_name, GroupID, Date, Flow, mk_pval, mk_pval_orig) %>%
    mutate(pval_dif = mk_pval - mk_pval_orig)
}) %>%
  bind_rows()

# Compute average flow per event
event_summary <- trimmed_results %>%
  group_by(site_name, GroupID) %>%
  summarize(
    avg_flow = mean(Flow, na.rm = TRUE),
    mean_pval_dif = mean(pval_dif, na.rm = TRUE),
    .groups = "drop"
  )

# Scatter plot
ggplot(event_summary, aes(x = avg_flow, y = mean_pval_dif, color = site_name)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~ site_name, scales = "free_x") +
  labs(
    x = "Average Flow per Drought Event",
    y = "Difference in MK p-value (trimmed - original)",
    title = "Change in MK p-value vs Average Flow by Gage"
  ) +
  theme_minimal()
