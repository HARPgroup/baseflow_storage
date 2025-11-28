# --- dependencies ---
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

alpha_vals <- c(0.10, 0.20, 0.30)

analysis_list <- list(
  "Cootes Store" = CS_original_analysis_df,
  "Strasburg"    = S_original_analysis_df,
  "Mount Jackson"= MJ_original_analysis_df
)


# ---------------- REGRESSION-BASED EVENT STATS ----------------
#Based of methodology from calc_event_stats
calc_regression_AGWRC <- function(df) {
  # Need at least 3 rows for a regression
  if (nrow(df) < 3) {
    return(tibble(
      trimmed_AGWRC = NA_real_,
      trimmed_R2   = NA_real_
    ))
  }
  
  # Fit log(Flow)
  fit <- try(lm(log(Flow) ~ Date, data = df), silent = TRUE)
  
  if (inherits(fit, "try-error")) {
    return(tibble(
      trimmed_AGWRC = NA_real_,
      trimmed_R2   = NA_real_
    ))
  }
  
  s <- summary(fit)
  
  slope <- coef(fit)[2]
  AGWRC_val <- exp(slope)
  R2_val <- s$r.squared
  
  tibble(
    trimmed_AGWRC = AGWRC_val,
    trimmed_R2   = R2_val
  )
}



# ---------------- SAFE summarize_event WRAPPER ----------------
safe_summarize_event <- function(df) {
  res <- tryCatch({
    summarize_event(df)
  }, error = function(e) {
    tibble(
      event_num = unique(df$GroupID),
      calc_AGWRC = NA_real_,
      R_squared = NA_real_,
      n_points  = nrow(df)
    )
  })
  as_tibble(res)
}



# ---------------- PROCESS ONE SITE + ALPHA ----------------
process_site_alpha <- function(df, site_name, alpha) {
  
  # 1) Trim using MK alpha window
  df_trimmed <- df %>%
    group_by(GroupID) %>%
    group_modify(~ trim_event_mk(.x, alpha = alpha)) %>%
    ungroup()
  
  # 2) Keep rows in the MK window
  df_trimmed_kept <- df_trimmed %>% filter(kept == TRUE)
  
  # 3) Regression-based AGWRC for each trimmed event
  event_regression_stats <- df_trimmed_kept %>%
    group_by(GroupID) %>%
    group_modify(~ calc_regression_AGWRC(.x)) %>%
    ungroup()
  
  # 4) Join event-level regression values back to rows
  df_trimmed_kept <- df_trimmed_kept %>%
    left_join(event_regression_stats, by = "GroupID")
  
  # 5) Filter out trimmed_AGWRC >= 1.0
  df_trimmed_kept <- df_trimmed_kept %>%
    filter(is.na(trimmed_AGWRC) | trimmed_AGWRC < 1.0)
  
  # 6) Also compute summary_event (original AWGR & R²)wri
  orig_event_stats <- df_trimmed_kept %>%
    group_split(GroupID) %>%
    map_dfr(~ safe_summarize_event(.x))
  
  df_out <- df_trimmed_kept %>%
    left_join(orig_event_stats, by = c("GroupID" = "event_num")) %>%
    mutate(
      alpha     = alpha,
      site_name = site_name,
      trimmed_event_R_squared = trimmed_R2
    )
  
  df_out
}



# ---------------- RUN SWEEP ----------------
trimmed_event_results <- imap_dfr(
  analysis_list,
  function(df, site_name) {
    map_dfr(alpha_vals, ~ process_site_alpha(df, site_name, .x))
  }
)



# ---------------- EVENT-LEVEL STATS ----------------
event_population_stats <- trimmed_event_results %>%
  group_by(site_name, alpha, GroupID) %>%
  summarise(
    event_AGWRC_mean   = mean(trimmed_AGWRC, na.rm = TRUE),
    event_AGWRC_median = median(trimmed_AGWRC, na.rm = TRUE),
    event_duration    = n(),
    event_R2          = first(trimmed_event_R_squared),
    n_kept_rows       = sum(!is.na(trimmed_AGWRC)),
    .groups = "drop"
  )



# ---------------- SITE-LEVEL STATS ----------------
site_summary <- event_population_stats %>%
  group_by(site_name, alpha) %>%
  summarise(
    Average_AGWRCC_Across_All_Events = mean(event_AGWRC_mean, na.rm = TRUE),
    p25    = quantile(event_AGWRC_mean, 0.25, na.rm = TRUE),
    median = median(event_AGWRC_mean, na.rm = TRUE),
    p75    = quantile(event_AGWRC_mean, 0.75, na.rm = TRUE),
    n_events = n(),
    .groups = "drop"
  )



# ---------------- DIAGNOSTIC PLOT ----------------
ggplot(event_population_stats, aes(x = factor(alpha), y = event_AGWRC_mean)) +
  geom_boxplot() +
  facet_wrap(~ site_name, scales = "free_y") +
  labs(
    title = "Event-level AGWRC after MK trimming",
    x = "alpha",
    y = "Mean AGWRC per Event"
  )



list(
  trimmed_event_results = trimmed_event_results,
  event_population_stats = event_population_stats,
  site_summary = site_summary
) -> outputs
