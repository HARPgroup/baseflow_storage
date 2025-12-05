## modules/droughtModuleServer.R

droughtModuleServer <- function(id, site_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------------------------------------
    # 1. Convenience reactives
    # ---------------------------------------------
    original_df <- reactive({
      dat <- site_data()$original
      req(dat)
      dat %>%
        mutate(Date = as.Date(Date)) %>%
        arrange(Date)
    })
    
    trimmed_df <- reactive({
      dat <- site_data()$trimmed
      req(dat)
      dat %>%
        mutate(Date = as.Date(Date)) %>%
        # focus on events actually kept in trimming
        filter(is.na(kept) | kept)   # if kept exists, use it; else keep all
    })
    
    site_name <- reactive({
      # prefer site_name column if present, otherwise fallback
      df <- trimmed_df()
      if ("site_name" %in% names(df)) {
        unique(df$site_name)[1]
      } else if ("site_no" %in% names(df)) {
        paste("site", unique(df$site_no)[1])
      } else {
        "Selected site"
      }
    })
    
    # ---------------------------------------------
    # 2. Event-level summary (for DT & regression)
    # ---------------------------------------------
    events_summary <- reactive({
      df <- trimmed_df()
      req(nrow(df) > 0)
      
      agwrc_col <- dplyr::case_when(
        "trimmed_AGWRC" %in% names(df) ~ "trimmed_AGWRC",
        "calc_AGWRC"    %in% names(df) ~ "calc_AGWRC",
        "AGWR.y"        %in% names(df) ~ "AGWR.y",
        "AGWR.x"        %in% names(df) ~ "AGWR.x",
        TRUE                             ~ NA_character_
      )
      
      df %>%
        dplyr::group_by(GroupID) %>%
        dplyr::summarise(
          site_name   = first(site_name),
          start_date  = min(Date),
          end_date    = max(Date),
          n_days      = dplyr::n_distinct(Date),  # <<-- fix here
          median_flow = median(Flow, na.rm = TRUE),
          min_flow    = min(Flow, na.rm = TRUE),
          max_flow    = max(Flow, na.rm = TRUE),
          event_AGWRC = if (!is.na(agwrc_col)) first(.data[[agwrc_col]]) else NA_real_,
          event_R2    = first(dplyr::coalesce(
            if ("trimmed_event_R_squared" %in% names(.)) trimmed_event_R_squared else NA_real_,
            if ("event_R_squared"         %in% names(.)) event_R_squared         else NA_real_
          )),
          .groups = "drop"
        ) %>%
        dplyr::arrange(start_date)
    })
    
    # ---------------------------------------------
    # 3. Historical plot (recent window)
    # ---------------------------------------------
    output$historical_plot <- renderPlotly({
      df <- original_df()
      req(nrow(df) > 0)
      
      # show last 2 years (or full if shorter)
      max_date <- max(df$Date, na.rm = TRUE)
      min_window <- max_date - years(2)
      df_recent <- df %>% filter(Date >= min_window)
      
      plot_ly(
        df_recent,
        x = ~Date,
        y = ~Flow,
        type = "scatter",
        mode = "lines",
        name = "Daily flow"
      ) |>
        layout(
          title = paste("Recent Daily Flow -", site_name()),
          xaxis = list(title = "Date"),
          yaxis = list(
            title = "Flow (cfs)",
            rangemode = "tozero"    # <-- ensure axis can go to 0
          )
        )
    })
    
    # ---------------------------------------------
    # 4. Events table (DT)
    # ---------------------------------------------
    output$events_table <- renderDT({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      datatable(
        evt,
        selection = "single",
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # ---------------------------------------------
    # 5. AGWRC vs Flow regression
    # ---------------------------------------------
    output$agwrc_regression_plot <- renderPlotly({
      evt <- events_summary()
      req(nrow(evt) > 1)
      
      evt <- evt %>% filter(!is.na(event_AGWRC), !is.na(median_flow))
      req(nrow(evt) > 1)
      
      # Simple linear regression AGWRC ~ log(flow) for stability
      model <- lm(event_AGWRC ~ log(median_flow), data = evt)
      
      # Build smooth line
      flow_seq <- seq(min(evt$median_flow, na.rm = TRUE),
                      max(evt$median_flow, na.rm = TRUE),
                      length.out = 100)
      pred_df <- data.frame(
        median_flow = flow_seq,
        event_AGWRC = predict(model, newdata = data.frame(median_flow = flow_seq))
      )
      
      p <- plot_ly() |>
        add_markers(
          data = evt,
          x    = ~median_flow,
          y    = ~event_AGWRC,
          name = "Events",
          hovertemplate = paste(
            "GroupID: %{customdata[1]}<br>",
            "Median flow: %{x:.1f} cfs<br>",
            "Event AGWRC: %{y:.3f}<br>",
            "<extra></extra>"
          ),
          customdata = matrix(evt$GroupID, ncol = 1)
        ) |>
        add_lines(
          data = pred_df,
          x    = ~median_flow,
          y    = ~event_AGWRC,
          name = "Regression fit"
        ) |>
        layout(
          title = paste("AGWRC vs Flow (event-level) -", site_name()),
          xaxis = list(title = "Characteristic event flow (median, cfs)",
                       rangemode = "tozero"),
          yaxis = list(title = "Event AGWRC",
                       rangemode = "tozero")
        )
      
      p
    })
    
    output$regression_summary <- renderPrint({
      evt <- events_summary()
      req(nrow(evt) > 1)
      evt <- evt %>% filter(!is.na(event_AGWRC), !is.na(median_flow))
      req(nrow(evt) > 1)
      
      model <- lm(event_AGWRC ~ log(median_flow), data = evt)
      summary(model)
    })
    
    # ---------------------------------------------
    # 6. Event inspection modal
    # ---------------------------------------------
    selected_group <- reactiveVal(NULL)
    
    observeEvent(input$inspect_event, {
      evt_tbl <- events_summary()
      s <- input$events_table_rows_selected
      
      if (is.null(s) || length(s) == 0) {
        showNotification("Please select an event (row) in the table first.", type = "warning")
        return(NULL)
      }
      
      group_id <- evt_tbl$GroupID[s[1]]
      selected_group(group_id)
      
      showModal(
        modalDialog(
          title = paste("Event Details - GroupID", group_id, "(", site_name(), ")"),
          size = "l",
          fluidRow(
            column(
              6,
              h4("Flow over Event"),
              plotlyOutput(ns("event_flow_plot"))
            ),
            column(
              6,
              h4("AGWR over Event"),
              plotlyOutput(ns("event_agwr_plot"))
            )
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
    
    output$event_flow_plot <- renderPlotly({
      df <- trimmed_df()
      gid <- selected_group()
      req(!is.null(gid))
      
      df_event <- df %>% filter(GroupID == gid)
      req(nrow(df_event) > 0)
      
      plot_ly(
        df_event,
        x = ~Date,
        y = ~Flow,
        type = "scatter",
        mode = "lines+markers",
        name = "Flow"
      ) |>
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Flow (cfs)", rangemode = "tozero")
        )
    })
    
    output$event_agwr_plot <- renderPlotly({
      df <- trimmed_df()
      gid <- selected_group()
      req(!is.null(gid))
      
      df_event <- df %>% filter(GroupID == gid)
      req(nrow(df_event) > 0)
      
      # prefer AGWR.y (post-trim) then AGWR.x
      agwr_col <- if ("AGWR.y" %in% names(df_event)) "AGWR.y" else
        if ("AGWR.x" %in% names(df_event)) "AGWR.x" else "AGWR"
      
      plot_ly(
        df_event,
        x = ~Date,
        y = df_event[[agwr_col]],
        type = "scatter",
        mode = "lines+markers",
        name = agwr_col
      ) |>
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "AGWR", rangemode = "tozero")
        )
    })
    
    # ---------------------------------------------
    # 7. Forecast logic (single AGWRC for now)
    # ---------------------------------------------
    
    # Update default start date when site changes: use last date in original_df
    observeEvent(original_df(), {
      df <- original_df()
      if (nrow(df) > 0) {
        updateDateInput(
          session,
          "forecast_start",
          value = max(df$Date, na.rm = TRUE)
        )
      }
    }, ignoreInit = FALSE)
    
    # horizons in days
    forecast_horizons <- c(15, 30, 45, 90)
    
    forecast_results <- reactive({
      df <- original_df()
      req(nrow(df) > 0)
      
      start_date <- input$forecast_start
      agwrc      <- input$agwrc_single
      req(!is.na(start_date), !is.na(agwrc))
      
      df <- df %>% arrange(Date)
      
      # flow on the selected start date
      Q0 <- df$Flow[df$Date == start_date]
      if (length(Q0) == 0) {
        showNotification("Selected forecast start date has no flow record.", type = "error")
        return(NULL)
      }
      
      Q0 <- Q0[1]
      
      tibble::tibble(
        horizon_days  = forecast_horizons,
        forecast_date = start_date + horizon_days,
        AGWRC         = agwrc,
        proj_flow     = Q0 * (agwrc ^ horizon_days)
      )
    })
    
    output$forecast_table <- renderDT({
      fr <- forecast_results()
      req(fr)
      datatable(
        fr,
        rownames = FALSE,
        options = list(dom = "tp", pageLength = 5)
      )
    })
    
    output$forecast_plot <- renderPlotly({
      df <- original_df()
      fr <- forecast_results()
      req(df, fr)
      
      start_date <- input$forecast_start
      req(start_date)
      
      # Historical window: 90 days before start_date
      hist_window <- 90
      df_hist <- df %>%
        filter(Date >= start_date - hist_window & Date <= start_date)
      
      # Construct combined data for plotting
      hist_plot <- df_hist %>%
        transmute(
          Date = Date,
          Flow = Flow,
          type = "Observed"
        )
      
      proj_plot <- fr %>%
        transmute(
          Date = forecast_date,
          Flow = proj_flow,
          type = "Projected"
        )
      
      combined <- bind_rows(hist_plot, proj_plot)
      
      plot_ly() |>
        add_lines(
          data = combined %>% filter(type == "Observed"),
          x = ~Date,
          y = ~Flow,
          name = "Observed",
          mode = "lines"
        ) |>
        add_lines(
          data = combined %>% filter(type == "Projected"),
          x = ~Date,
          y = ~Flow,
          name = "Projected",
          mode = "lines+markers"
        ) |>
        layout(
          title = paste("Observed & Projected Flow -", site_name()),
          xaxis = list(title = "Date"),
          yaxis = list(
            title = "Flow (cfs)",
            rangemode = "tozero"    # <-- keep axis grounded at 0
          ),
          legend = list(orientation = "h", x = 0.1, y = -0.1)
        )
    })
    
  })
}

