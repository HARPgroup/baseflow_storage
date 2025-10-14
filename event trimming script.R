library(dplyr)
library(ggplot2)
library(Kendall)
library(gridExtra)

trim_event_mk <- function(event_df, alpha = 0.1) {
  n <- nrow(event_df)
  
  # compute MK p-value for the full event (no trimming)
  mk_full <- tryCatch(Kendall::MannKendall(event_df$AGWR), error = function(e) NULL)
  pval_orig <- if (!is.null(mk_full)) mk_full$sl else NA
  
  if (n < 5) {
    return(event_df %>%
             mutate(
               mk_pval = NA,         
               mk_pval_orig = pval_orig,  
               trim_start = 1,
               kept = TRUE
             ))
  }
  
  trim_start <- 1
  pval_final <- NA
  
  for (i in seq_len(n - 2)) {
    sub_df <- event_df[i:n, ]
    
    mk <- tryCatch(Kendall::MannKendall(sub_df$AGWR), error = function(e) NULL)
    pval <- if (!is.null(mk)) mk$sl else NA
    
    if (!is.na(pval) && pval > alpha) {
      trim_start <- i
      pval_final <- pval
      break
    }
  }
  

  event_df %>%
    mutate(
      mk_pval = pval_final,     
      mk_pval_orig = pval_orig,
      trim_start = trim_start,
      kept = row_number() >= trim_start
    )
}

trimmed_combined <- combined_events %>%
  group_by(site, GroupID) %>%
  group_modify(~ trim_event_mk(.x, alpha = 0.1)) %>%
  ungroup()

# Plot function to show full event
plot_single_event <- function(combined_df, site_name, GroupID, alpha = 0.1) {
  event <- combined_df %>%
    filter(site == site_name, GroupID == GroupID)
  
  if (nrow(event) == 0) {
    stop("No data found for that site/group combination.")
  }
  
  trimmed_event <- trim_event_mk(event, alpha = alpha)
  
  ggplot(event, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +                      # full event
    geom_point(data = event %>% 
                 slice(1:(trimmed_event$trim_start[1]-1)),  # only trimmed days
               aes(x = Date, y = Flow),
               color = "red", size = 2) +
    labs(
      title = paste(site_name, "- Group", GroupID),
      subtitle = paste("Trim start day:", trimmed_event$trim_start[1]),
      x = "Date", y = "Flow (CFS)"
    ) +
    theme_minimal()
}

plot_single_event(combined_events, site_name = "MJ", GroupID = 94, alpha = 0.1)

filter_trimmed_events <- trimmed_combined %>% filter(kept)


# Pick one event
event_df <- trimmed_combined %>%
  filter(site == "MJ", GroupID == 94)

# Grab the original p-value for this event
mk_pval <- unique(event_df$mk_pval_orig)

# Hydrograph (Flow)
p1 <- ggplot(event_df, aes(x = Date, y = Flow)) +
  geom_line(color = "blue") +
  labs(
    title = paste0("Original Event (MJ Group 94)"),
    subtitle = paste0("M-K p-value: ", signif(mk_pval, 3)),
    y = "Flow (CFS)",
    x = "Time"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal()

# AGWR & ΔAGWR
p2 <- ggplot(event_df, aes(x = Date)) +
  geom_line(aes(y = AGWR), color = "blue") +
  geom_line(aes(y = delta_AGWR), color = "red") +
  labs(
    title = paste0("MJ 94 AGWR (blue) delta AGWR (red)"),
    y = "AGWR & ΔAGWR", x = "Time") +
  theme_minimal()

# Pick one event
trimmed_event_df <- filter_trimmed_events %>%
  filter(site == "MJ", GroupID == 94)

#Trimmed event 
trimmed_event_df <- trimmed_combined %>%
  filter(site == "MJ", GroupID == 94, kept)

# Grab the trimmed p-value
mk_pval_trimmed <- unique(trimmed_event_df$mk_pval)

# Trimmed Hydrograph (Flow)
p3 <- ggplot(trimmed_event_df, aes(x = Date, y = Flow)) +
  geom_line(color = "blue") +
  labs(
    title = "Trimmed Event (MJ Group 94)",
    subtitle = paste0("Trimmed M-K p-value: ", signif(mk_pval_trimmed, 3)),
    y = "Flow (CFS)",
    x = "Time"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal()

# Trimmed AGWR & ΔAGWR
p4 <- ggplot(trimmed_event_df, aes(x = Date)) +
  geom_line(aes(y = AGWR), color = "blue") +
  geom_line(aes(y = delta_AGWR), color = "red") +
  labs(
    title = paste0("Trimmed MJ 94 AGWR (blue) delta AGWR (red)"),
    y = "AGWR & ΔAGWR", x = "Time") +
  theme_minimal()

p1 
p2

p3
p4
