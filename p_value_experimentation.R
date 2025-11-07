library(dplyr)
library(purrr)
library(ggplot2)

# original data
analysis_list <- list(
  "Cootes Store" = CS_original_analysis_df,
  "Strasburg" = S_original_analysis_df,
  "Mount Jackson" = MJ_original_analysis_df
)

names(S_original_analysis_df)

S_original_analysis_df <- S_original_analysis_df %>%
  select(-matches("\\.\\.\\.\\d+$")) %>%  # drops columns with ...11, ...12, etc.
  rename(
    calc_AGWR = calc_AGWR, 
    event_R_squared = event_R_squared
  )

alpha_vals <- c(0.05, 0.10, 0.20)

process_trim_with_event_stats <- function(df, gage_name, alpha) {
  df_trimmed <- df %>%
    group_by(GroupID) %>%
    group_modify(~ trim_event_mk(.x, alpha = alpha)) %>%
    ungroup() %>%
    filter(kept == TRUE) %>%
    mutate(
      trimmed_calc_AGWR = calc_AGWR(Flow),
      trimmed_delta_AGWR = calc_delta_AGWR(trimmed_calc_AGWR),
      mk_pval_orig = mk_pval_orig  # keep original Mann-Kendall p-value
    )
  
  # Summarize event after trimming
  event_stats <- summarize_event(df_trimmed)
  
  df_with_stats <- df_trimmed %>%
    left_join(event_stats, by = c("GroupID" = "event_num")) %>%
    mutate(
      alpha = alpha,
      site_name = gage_name,
      trimmed_event_R_squared = R_squared
    )
  
  df_with_stats
}


trimmed_event_results <- imap_dfr(
  analysis_list,
  function(df, gage_name) {
    map_dfr(alpha_vals, ~ process_trim_with_event_stats(df, gage_name, .x))
  }
)

event_population_stats <- trimmed_event_results %>%
  group_by(site_name, alpha, GroupID) %>%
  summarize(
    event_mean_AGWR = mean(trimmed_calc_AGWR, na.rm = TRUE),
    event_median_AGWR = median(trimmed_calc_AGWR, na.rm = TRUE),
    event_duration = n(),
    event_R2 = first(trimmed_event_R_squared),
    .groups = "drop"
  )

site_summary <- event_population_stats %>%
  group_by(site_name, alpha) %>%
  summarize(
    mean_of_event_means = mean(event_mean_AGWR),
    p25 = quantile(event_mean_AGWR, 0.25),
    median = median(event_mean_AGWR),
    p75 = quantile(event_mean_AGWR, 0.75),
    n_events = n()
  )



# Apply for all gages and alpha values
trimmed_sweep <- imap_dfr(
  analysis_list,
  function(df, gage_name) {
    map_dfr(alpha_vals, ~ process_gage_alpha(df, gage_name, .x))
  }
)

p_value_stats_summary <- trimmed_sweep %>%
  group_by(site_name, alpha) %>%
  summarize(
    mean_AGWR = mean(trimmed_calc_AGWR, na.rm = TRUE),
    p25 = quantile(trimmed_calc_AGWR, 0.25, na.rm = TRUE),
    median = quantile(trimmed_calc_AGWR, 0.50, na.rm = TRUE),
    p75 = quantile(trimmed_calc_AGWR, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Boxplots
ggplot(trimmed_sweep, aes(x = site_name, y = trimmed_calc_AGWR, fill = site_name)) +
  geom_boxplot() +
  facet_wrap(~ alpha, scales = "free_y") +
  labs(
    title = "AGWR Distribution by Gage and Alpha Threshold",
    x = "Gage",
    y = "Trimmed AGWR"
  )

process_trim_with_event_stats <- function(df, gage_name, alpha) {
  df_trimmed <- df %>%
    group_by(GroupID) %>%
    group_modify(~ trim_event_mk(.x, alpha = alpha)) %>%
    ungroup() %>%
    filter(kept == TRUE) %>%
    mutate(
      trimmed_calc_AGWR = calc_AGWR(Flow),
      trimmed_delta_AGWR = calc_delta_AGWR(trimmed_calc_AGWR)
    )
  
  event_stats <- summarize_event(df_trimmed) %>%
    rename(
      event_calc_AGWR = calc_AGWR,
      event_R_squared = R_squared
    )
  
  df_with_stats <- df_trimmed %>%
    left_join(event_stats, by = c("GroupID" = "event_num")) %>%
    mutate(
      alpha = alpha,
      site_name = gage_name,
      trimmed_event_R_squared = event_R_squared
    )
  
  df_with_stats
}


trimmed_event_results <- imap_dfr(
  analysis_list,
  function(df, gage_name) {
    map_dfr(alpha_vals, ~ process_trim_with_event_stats(df, gage_name, .x))
  }
)

event_population_stats <- trimmed_event_results %>%
  group_by(site_name, alpha, GroupID) %>%
  summarize(
    event_mean_AGWR = mean(trimmed_calc_AGWR, na.rm = TRUE),
    event_median_AGWR = median(trimmed_calc_AGWR, na.rm = TRUE),
    event_duration = n(),
    event_R2 = first(trimmed_event_R_squared),
    .groups = "drop"
  )

site_summary <- event_population_stats %>%
  group_by(site_name, alpha) %>%
  summarize(
    mean_of_event_means = mean(event_mean_AGWR),
    p25 = quantile(event_mean_AGWR, 0.25),
    median = median(event_mean_AGWR),
    p75 = quantile(event_mean_AGWR, 0.75),
    n_events = n()
  )




