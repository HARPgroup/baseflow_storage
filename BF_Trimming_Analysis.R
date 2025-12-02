# --- dependencies ---
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

#load original data
CS_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/CS_original_analysis_df.csv"
)

S_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/S_original_analysis_df.csv"
)

MJ_original_analysis_df <- read.csv(
  "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/MJ_original_analysis_df.csv"
)

# load MK trimming function
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/ben_trimming/will_mk_trim.R")

alpha_vals <- c(0.10, 0.20, 0.30)

analysis_list <- list(
  "Cootes Store" = CS_original_analysis_df,
  "Strasburg"    = S_original_analysis_df,
  "Mount Jackson"= MJ_original_analysis_df
)


# ---------------- REGRESSION-BASED EVENT STATS ----------------
#Based of methodology from calc_event_stats function
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
  df_trimmed_kept <- df_trimmed
  
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



# ---------------- RUN SWEEP SEPARATELY FOR EACH SITE ----------------
site_results_list <- imap(
  analysis_list,
  function(df, site_name) {
    map_dfr(alpha_vals, ~ process_site_alpha(df, site_name, .x))
  }
)

# Extract individually
CS_trimmed_event_results <- site_results_list[["Cootes Store"]]
S_trimmed_event_results  <- site_results_list[["Strasburg"]]
MJ_trimmed_event_results <- site_results_list[["Mount Jackson"]]




# ---------------- EVENT-LEVEL STATS ----------------
CS_event_population_stats <- CS_trimmed_event_results %>%
  group_by(alpha, GroupID) %>%
  summarise(
    event_AGWRC_mean   = mean(trimmed_AGWRC, na.rm = TRUE),
    event_AGWRC_median = median(trimmed_AGWRC, na.rm = TRUE),
    event_duration     = n(),
    event_R2           = first(trimmed_event_R_squared),
    n_kept_rows        = sum(!is.na(trimmed_AGWRC)),
    .groups = "drop"
  )

S_event_population_stats <- S_trimmed_event_results %>%
  group_by(alpha, GroupID) %>%
  summarise(
    event_AGWRC_mean   = mean(trimmed_AGWRC, na.rm = TRUE),
    event_AGWRC_median = median(trimmed_AGWRC, na.rm = TRUE),
    event_duration     = n(),
    event_R2           = first(trimmed_event_R_squared),
    n_kept_rows        = sum(!is.na(trimmed_AGWRC)),
    .groups = "drop"
  )

MJ_event_population_stats <- MJ_trimmed_event_results %>% 
  group_by(alpha, GroupID) %>%
  summarise(
    event_AGWRC_mean   = mean(trimmed_AGWRC, na.rm = TRUE),
    event_AGWRC_median = median(trimmed_AGWRC, na.rm = TRUE),
    event_duration     = n(),
    event_R2           = first(trimmed_event_R_squared),
    n_kept_rows        = sum(!is.na(trimmed_AGWRC)),
    .groups = "drop"
  )




# ---------------- SITE-LEVEL STATS ----------------
CS_site_summary <- CS_event_population_stats %>%
  group_by(alpha) %>%
  summarise(
    Average_AGWRCC_Across_All_Events = mean(event_AGWRC_mean, na.rm = TRUE),
    p25    = quantile(event_AGWRC_mean, 0.25, na.rm = TRUE),
    median = median(event_AGWRC_mean, na.rm = TRUE),
    p75    = quantile(event_AGWRC_mean, 0.75, na.rm = TRUE),
    n_events = n(),
    .groups = "drop"
  )

S_site_summary <- S_event_population_stats %>%
  group_by(alpha) %>%
  summarise(
    Average_AGWRCC_Across_All_Events = mean(event_AGWRC_mean, na.rm = TRUE),
    p25    = quantile(event_AGWRC_mean, 0.25, na.rm = TRUE),
    median = median(event_AGWRC_mean, na.rm = TRUE),
    p75    = quantile(event_AGWRC_mean, 0.75, na.rm = TRUE),
    n_events = n(),
    .groups = "drop"
  )



MJ_site_summary <- MJ_event_population_stats %>%
  group_by(alpha) %>%
  summarise(
    Average_AGWRCC_Across_All_Events = mean(event_AGWRC_mean, na.rm = TRUE),
    p25    = quantile(event_AGWRC_mean, 0.25, na.rm = TRUE),
    median = median(event_AGWRC_mean, na.rm = TRUE),
    p75    = quantile(event_AGWRC_mean, 0.75, na.rm = TRUE),
    n_events = n(),
    .groups = "drop"
  )






# # ---------------- DIAGNOSTIC PLOT ----------------
# ggplot(CS_event_population_stats, aes(x = factor(alpha), y = event_AGWRC_mean)) +
#   geom_boxplot() +
#   facet_wrap(~ site_name, scales = "free_y") +
#   labs(
#     title = "CS Event-level AGWRC after MK trimming",
#     x = "alpha",
#     y = "Mean AGWRC per Event"
#   )
# 
# ggplot(S_event_population_stats, aes(x = factor(alpha), y = event_AGWRC_mean)) +
#   geom_boxplot() +
#   facet_wrap(~ site_name, scales = "free_y") +
#   labs(
#     title = "S Event-level AGWRC after MK trimming",
#     x = "alpha",
#     y = "Mean AGWRC per Event"
#   )
# 
# ggplot(MJ_event_population_stats, aes(x = factor(alpha), y = event_AGWRC_mean)) +
#   geom_boxplot() +
#   facet_wrap(~ site_name, scales = "free_y") +
#   labs(
#     title = "MJ_Event-level AGWRC after MK trimming",
#     x = "alpha",
#     y = "Mean AGWRC per Event"
#   )

clean_event_df <- function(df) {
  
  df %>%
    # 1. Drop all-NA columns
    select(where(~ !all(is.na(.x)))) %>%
    
    # 2. Rename theoriginal regression columns
    rename(
      AGWRC        = AGWR,
      delta_AGWRC  = delta_AGWR,
      calc_AGWRC   = calc_AGWR
    ) %>%
    
    # 3. Keep all relevant columns (whether trimmed or original)
    select(
      GroupID, site_no, site_name, Date, Year, Month, Day, Season,
      Flow,
      
      # original untrimmed flow regression
      AGWRC, delta_AGWRC, calc_AGWRC,
      event_R_squared,
      
      # MK/trimming info
      mk_pval, mk_pval_orig,
      trim_start, trim_end, win_len,
      is_short, met_alpha, kept,
      
      # trimmed regression results
      trimmed_AGWRC,        # trimmed slope
      trimmed_R2, trimmed_event_R_squared,
      
      # metadata
      n_points, alpha
    )
}




CS_trimmed_events_full  <- clean_event_df(CS_trimmed_event_results)
S_trimmed_events_full   <- clean_event_df(S_trimmed_event_results)
MJ_trimmed_events_full  <- clean_event_df(MJ_trimmed_event_results)


