library(dplyr)
library(purrr)
library(ggplot2)

# Your original data
analysis_list <- list(
  "Cootes Store" = CS_original_analysis_df,
  "Strasburg" = S_original_analysis_df,
  "Mount Jackson" = MJ_original_analysis_df
)

alpha_vals <- c(0.05, 0.10, 0.20)

# Simple trimming function wrapper
process_gage_alpha <- function(df, gage_name, alpha) {
  df <- df %>%
    mutate(GroupID = paste0(gage_name, "_", GroupID))  # make unique GroupIDs
  
  df %>%
    group_by(GroupID) %>%
    group_modify(~ trim_event_mk(.x, alpha = alpha)) %>%
    ungroup() %>%
    filter(kept == TRUE) %>%
    mutate(
      trimmed_calc_AGWR = calc_AGWR(Flow),
      delta_AGWR = calc_delta_AGWR(trimmed_calc_AGWR),
      alpha = alpha,
      site_no = gage_name
    ) %>%
    select(site_no, alpha, Flow, trimmed_calc_AGWR, delta_AGWR)
}


# Apply for all gages and alpha values
trimmed_sweep <- imap_dfr(
  analysis_list,
  function(df, gage_name) {
    map_dfr(alpha_vals, ~ process_gage_alpha(df, gage_name, .x))
  }
)

p_value_stats_summary <- trimmed_sweep %>%
  group_by(site_no, alpha) %>%
  summarize(
    mean_AGWR = mean(trimmed_calc_AGWR, na.rm = TRUE),
    p25 = quantile(trimmed_calc_AGWR, 0.25, na.rm = TRUE),
    median = quantile(trimmed_calc_AGWR, 0.50, na.rm = TRUE),
    p75 = quantile(trimmed_calc_AGWR, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Boxplots
ggplot(trimmed_sweep, aes(x = site_no, y = trimmed_calc_AGWR, fill = site_no)) +
  geom_boxplot() +
  facet_wrap(~ alpha, scales = "free_y") +
  labs(
    title = "AGWR Distribution by Gage and Alpha Threshold",
    x = "Gage",
    y = "Trimmed AGWR"
  )

