library(dplyr)
library(Kendall)

trim_event_buffer_simple <- function(event_df, tol = 0.03) {
  ok <- abs(event_df$AGWR - 1) <= tol
  
  #first and last "stable" points
  i_start <- which(ok)[1]
  i_end   <- tail(which(ok), 1)
  
  #if nothing stable or bad indices: return empty
  if (is.na(i_start) || is.na(i_end) || i_start >= i_end)
    return(event_df[0, , drop = FALSE])
  
  trimmed <- event_df[i_start:i_end, , drop = FALSE]
  
  #Mann–Kendall p-value
  mk <- tryCatch(Kendall::MannKendall(trimmed$AGWR), error = function(e) NULL)
  trimmed$mk_pval <- if (!is.null(mk)) mk$sl else NA_real_
  
  trimmed
}

#apply to dataset grouped by event
trimmed_simple <- event_df %>%
  arrange(GroupID, Date) %>%          #make sure rows per event are sorted
  group_by(GroupID) %>%
  group_modify(~ trim_event_buffer_simple(.x, tol = 0.03)) %>%
  ungroup()

########VISUALIZATION FOR MY FUNCTION#############
library(dplyr)
library(ggplot2)

# id = GroupID to plot; metric = "AGWR" or "Flow"
plot_event_comparison <- function(id, metric = "AGWR", tol = 0.03) {
  stopifnot(metric %in% c("AGWR", "Flow"))
  
  full_ev <- analysis_df %>%
    filter(GroupID == id) %>%
    arrange(Date) %>%
    mutate(source = "full")
  
  trim_ev <- trimmed_simple %>%
    filter(GroupID == id) %>%
    arrange(Date) %>%
    mutate(source = "trimmed")
  
  if (nrow(trim_ev) == 0) {
    message(sprintf("No trimmed window for GroupID %s (nothing within tolerance).", id))
    return(invisible(NULL))
  }
  
  ev <- bind_rows(full_ev, trim_ev)
  
  # start/end markers for trimmed window
  x_start <- min(trim_ev$Date)
  x_end   <- max(trim_ev$Date)
  
  p <- ggplot(full_ev, aes(x = Date, y = .data[[metric]])) +
    # (optional) show the AGWR tolerance band if metric == "AGWR"
    { if (metric == "AGWR") 
      geom_hline(yintercept = 1 + c(-tol, tol), linetype = "dashed", alpha = 0.4)
    } +
    geom_line(color = "grey60", linewidth = 0.8) +                         # full series
    geom_line(data = trim_ev, aes(y = .data[[metric]]), linewidth = 1.2) + # trimmed overlay
    geom_vline(xintercept = as.numeric(x_start), linetype = "dotted") +
    geom_vline(xintercept = as.numeric(x_end),   linetype = "dotted") +
    labs(title = paste("Event", id, "—", metric, "comparison"),
         subtitle = "Grey = full event; solid = trimmed window; dotted = start/end",
         y = metric, x = NULL) +
    theme_minimal()
  
  print(p)
}

# Example usage:
plot_event_comparison(id = 4, metric = "AGWR", tol = 0.03)
# or:
plot_event_comparison(id = 13, metric = "Flow")
#################################################################
#################################################################