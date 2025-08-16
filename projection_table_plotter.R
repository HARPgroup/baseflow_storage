if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")

plot_projected_decay <- function(df, proj_days = c(30, 60, 120, 240, 360, 540), gage_col = NULL) {
  
  # Split by event
  unique_events <- df %>%
    dplyr::group_by(GroupID) %>%
    dplyr::group_split()
  
  plots <- lapply(unique_events, function(event_df) {
    gage_name <- if (!is.null(gage_col)) unique(event_df[[gage_col]]) else NULL
    event_id <- unique(event_df$GroupID)
    
    # Last day of the event
    last_day <- tail(event_df, 1)
    
    # Daily projection
    total_days <- max(proj_days)
    proj_dates <- seq(from = last_day$Date + 1, by = "day", length.out = total_days)
    proj_flows <- last_day$Flow * (last_day$calc_AGWR)^(1:total_days)
    
    proj_df <- data.frame(
      Date = proj_dates,
      Flow = proj_flows,
      Projected = "Projected"
    )
    
    # Highlighted points at specific projection days
    highlight_df <- proj_df[proj_days, , drop = FALSE]
    
    # Plot
    p <- ggplot2::ggplot(proj_df, ggplot2::aes(x = Date, y = Flow, color = Projected)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(data = highlight_df, ggplot2::aes(x = Date, y = Flow), color = "black", size = 2) +
      ggplot2::geom_text(data = highlight_df, ggplot2::aes(x = Date, y = Flow, label = round(Flow, 1)),
                         vjust = -0.5, color = "black", size = 3) +
      ggplot2::labs(
        title = paste("Projected Decay - Event", event_id,
                      if (!is.null(gage_name)) paste("-", gage_name)),
        x = "Date",
        y = "Flow",
        color = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(values = c("Projected" = "red")) +
      ggplot2::scale_x_date(date_labels = "%b %Y")
    
    return(p)
  })
  
  return(plots)
}

# Example usage:
plots <- plot_projected_decay(all_results, gage_col = "gage")
plots[[3]]  # display first event
