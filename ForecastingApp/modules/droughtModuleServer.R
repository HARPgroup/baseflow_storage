## modules/droughtModuleServer.R

droughtModuleServer <- function(id, gage_id, data_source, site_choice) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------------------------------------
    # 0. Core data for this site
    #    - analyzed events/points: from GitHub via bf_get_analysis(kind = model|gage)
    #    - raw daily flow for plots:
    #        * model: model daily flows CSV from GitHub
    #        * gage:  USGS via dataRetrieval
    # ---------------------------------------------
    
    analysis_points <- reactive({
      req(gage_id(), data_source())
      
      out <- tryCatch(
        bf_get_analysis(gage_id(), kind = data_source()),
        error = function(e) {
          showNotification(paste("Analysis CSV load failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      
      req(!is.null(out), nrow(out) > 0)
      message("Loaded GitHub analysis rows: ", nrow(out), " for gage_id = ", gage_id(), " (", data_source(), ")")
      out
    })
    
    # ---- FIXED: model raw daily loader ----
    model_raw_daily <- reactive({
      req(site_choice())
      
      url_map <- c(
        "Cootes Store"  = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5550_5560_flows_11.csv",
        "Mount Jackson" = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS2_5560_5100_flows_11.csv",
        "Strasburg"     = "https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/data/PS3_5100_5080_flows_11.csv"
      )
      
      url <- unname(url_map[site_choice()])
      req(!is.na(url), nzchar(url))
      
      raw <- tryCatch(
        readr::read_csv(url, show_col_types = FALSE),
        error = function(e) {
          showNotification(paste("Model raw CSV load failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      req(!is.null(raw), nrow(raw) > 0)
      
      # --- FLOW column ---
      flow_col <- if ("Qout" %in% names(raw)) "Qout" else if ("Flow" %in% names(raw)) "Flow" else NA_character_
      req(!is.na(flow_col))
      
      # --- DATE column logic ---
      # 1) Prefer thisdate/Date if it exists AND has non-NA values
      date_vec <- NULL
      if ("thisdate" %in% names(raw)) date_vec <- raw$thisdate
      if (is.null(date_vec) && "Date" %in% names(raw)) date_vec <- raw$Date
      
      # if thisdate exists but is all NA/blank, build from year/month/day
      need_build <- is.null(date_vec) || all(is.na(date_vec)) || all(trimws(as.character(date_vec)) == "")
      
      if (need_build) {
        req(all(c("year", "month", "day") %in% names(raw)))
        date_vec <- sprintf("%04d-%02d-%02d", raw$year, raw$month, raw$day)
      }
      
      out <- tibble::tibble(
        Date = as.Date(date_vec),
        Flow = as.numeric(raw[[flow_col]])
      ) %>%
        dplyr::filter(!is.na(Date)) %>%
        dplyr::arrange(Date)
      
      if (nrow(out) == 0) {
        showNotification(
          "Model raw data loaded, but no valid dates were parsed (Date ended up empty). Check date fields in the model CSV.",
          type = "error", duration = NULL
        )
      } else {
        message(
          "Model raw_daily: rows=", nrow(out),
          " min=", min(out$Date, na.rm = TRUE),
          " max=", max(out$Date, na.rm = TRUE)
        )
      }
      
      out
    })
    
    gage_raw_daily <- reactive({
      req(gage_id())
      
      pts <- analysis_points()
      start_date <- min(pts$Date, na.rm = TRUE)
      
      dv <- tryCatch(
        dataRetrieval::readNWISdv(
          siteNumbers = gage_id(),
          parameterCd = "00060",
          startDate = as.character(start_date)
        ),
        error = function(e) {
          showNotification(paste("USGS daily flow download failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      
      req(!is.null(dv), nrow(dv) > 0)
      
      q_col <- grep("^X_00060_00003$", names(dv), value = TRUE)
      if (length(q_col) == 0) {
        q_col <- grep("00060", names(dv), value = TRUE)[1]
      }
      
      dv %>%
        dplyr::transmute(
          Date = as.Date(Date),
          Flow = as.numeric(.data[[q_col]])
        ) %>%
        dplyr::arrange(Date)
    })
    
    raw_daily <- reactive({
      req(data_source())
      if (data_source() == "model") model_raw_daily() else gage_raw_daily()
    })
    
    # ---------------------------------------------
    # 1. Convenience reactives
    # ---------------------------------------------
    original_df <- reactive({
      df <- analysis_points()
      req(nrow(df) > 0)
      df %>% dplyr::arrange(Date)
    })
    
    trimmed_df <- reactive({
      df <- analysis_points()
      req(nrow(df) > 0)
      df %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::filter(kept == TRUE, met_alpha == TRUE)
    })
    
    site_name <- reactive({
      df <- analysis_points()
      
      if ("site_name" %in% names(df)) {
        nm <- unique(df$site_name)
        nm <- nm[!is.na(nm) & nzchar(nm)]
        if (length(nm) > 0) return(nm[1])
      }
      
      if (!is.null(site_choice()) && nzchar(site_choice())) {
        site_choice()
      } else {
        paste("USGS", gage_id())
      }
    })
    
    # ---------------------------------------------
    # 2. Event-level summary (for DT & regression)
    # ---------------------------------------------
    events_summary <- reactive({
      df <- trimmed_df()
      req(nrow(df) > 0)
      make_ben_event_summary(df)
    })
    
    # ---------------------------------------------
    # 3. Historical plot (recent window)
    # ---------------------------------------------
    output$historical_plot <- renderPlotly({
      df <- raw_daily()
      req(nrow(df) > 0)
      
      max_date   <- max(df$Date, na.rm = TRUE)
      min_window <- max_date - lubridate::years(2)
      df_recent  <- df %>% dplyr::filter(Date >= min_window)
      
      plotly::plot_ly(
        df_recent,
        x = ~Date,
        y = ~Flow,
        type = "scatter",
        mode = "lines",
        name = if (data_source() == "model") "Model flow" else "USGS flow"
      ) |>
        plotly::layout(
          title = paste("Recent Daily Flow -", site_name(), "(", toupper(data_source()), ")"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Flow (cfs)", rangemode = "tozero")
        )
    })
    
    # ---------------------------------------------
    # 4. Events table (DT)
    # ---------------------------------------------
    output$events_table <- renderDT({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      DT::datatable(
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
      
      evt <- evt %>% dplyr::filter(!is.na(event_AGWRC), !is.na(median_flow))
      req(nrow(evt) > 1)
      
      model <- stats::lm(event_AGWRC ~ log(median_flow), data = evt)
      
      flow_seq <- seq(min(evt$median_flow, na.rm = TRUE),
                      max(evt$median_flow, na.rm = TRUE),
                      length.out = 100)
      
      pred_df <- data.frame(
        median_flow = flow_seq,
        event_AGWRC = predict(model, newdata = data.frame(median_flow = flow_seq))
      )
      
      plotly::plot_ly() |>
        plotly::add_markers(
          data = evt,
          x    = ~median_flow,
          y    = ~event_AGWRC,
          name = "Events",
          customdata = ~GroupID,
          hovertemplate = paste0(
            "GroupID: %{customdata}<br>",
            "Median flow: %{x:.1f} cfs<br>",
            "Event AGWRC: %{y:.3f}",
            "<extra></extra>"
          )
        ) |>
        plotly::add_lines(
          data = pred_df,
          x    = ~median_flow,
          y    = ~event_AGWRC,
          name = "Regression fit"
        ) |>
        plotly::layout(
          title = paste("AGWRC vs Flow (event-level) -", site_name(), "(", toupper(data_source()), ")"),
          xaxis = list(title = "Characteristic event flow (median, cfs)", rangemode = "tozero"),
          yaxis = list(title = "Event AGWRC", rangemode = "tozero")
        )
    })
    
    output$regression_summary <- renderPrint({
      evt <- events_summary()
      req(nrow(evt) > 1)
      
      evt <- evt %>% dplyr::filter(!is.na(event_AGWRC), !is.na(median_flow))
      req(nrow(evt) > 1)
      
      model <- stats::lm(event_AGWRC ~ log(median_flow), data = evt)
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
            column(6, h4("Flow over Event"), plotlyOutput(ns("event_flow_plot"))),
            column(6, h4("AGWR over Event"), plotlyOutput(ns("event_agwr_plot")))
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
      
      df_event <- df %>% dplyr::filter(GroupID == gid)
      req(nrow(df_event) > 0)
      
      plotly::plot_ly(
        df_event,
        x = ~Date,
        y = ~Flow,
        type = "scatter",
        mode = "lines+markers",
        name = "Flow"
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Flow (cfs)", rangemode = "tozero")
        )
    })
    
    output$event_agwr_plot <- renderPlotly({
      df <- trimmed_df()
      gid <- selected_group()
      req(!is.null(gid))
      
      df_event <- df %>% dplyr::filter(GroupID == gid)
      req(nrow(df_event) > 0)
      
      plotly::plot_ly(
        df_event,
        x = ~Date,
        y = ~AGWR,
        type = "scatter",
        mode = "lines+markers",
        name = "AGWR"
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "AGWR", rangemode = "tozero")
        )
    })
    
    # ---------------------------------------------
    # 6b. Auto-default AGWRC based on event containing forecast_start
    # ---------------------------------------------
    observeEvent(list(input$forecast_start, events_summary()), {
      req(input$forecast_start)
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      sd <- as.Date(input$forecast_start)
      
      hit <- evt %>%
        dplyr::filter(start_date <= sd, end_date >= sd) %>%
        dplyr::slice(1)
      
      if (nrow(hit) == 1 && !is.na(hit$event_AGWRC)) {
        updateNumericInput(session, "agwrc_single", value = round(hit$event_AGWRC, 3))
      }
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------
    # 7. Forecast logic (single AGWRC for now)
    # ---------------------------------------------
    observeEvent(raw_daily(), {
      df <- raw_daily()
      if (nrow(df) > 0) {
        updateDateInput(session, "forecast_start", value = max(df$Date, na.rm = TRUE))
      }
    }, ignoreInit = FALSE)
    
    forecast_horizons <- c(15, 30, 45, 90)
    
    forecast_results <- reactive({
      df <- raw_daily()
      req(nrow(df) > 0)
      
      start_date <- input$forecast_start
      agwrc      <- input$agwrc_single
      req(!is.na(start_date), !is.na(agwrc))
      
      df <- df %>% dplyr::arrange(Date)
      
      Q0 <- df$Flow[df$Date == start_date]
      if (length(Q0) == 0) {
        showNotification("Selected forecast start date has no flow record.", type = "error")
        return(NULL)
      }
      
      Q0 <- Q0[1]
      
      tibble::tibble(
        horizon_days  = forecast_horizons,
        forecast_date = as.Date(start_date) + horizon_days,
        AGWRC         = agwrc,
        proj_flow     = Q0 * (agwrc ^ horizon_days)
      )
    })
    
    output$forecast_table <- renderDT({
      fr <- forecast_results()
      req(fr)
      DT::datatable(fr, rownames = FALSE, options = list(dom = "tp", pageLength = 5))
    })
    
    output$forecast_plot <- renderPlotly({
      df <- raw_daily()
      fr <- forecast_results()
      req(df, fr)
      
      start_date <- as.Date(input$forecast_start)
      req(start_date)
      
      hist_window <- 90
      df_hist <- df %>% dplyr::filter(Date >= start_date - hist_window & Date <= start_date)
      
      hist_plot <- df_hist %>% dplyr::transmute(Date = Date, Flow = Flow, type = "Observed")
      proj_plot <- fr %>% dplyr::transmute(Date = forecast_date, Flow = proj_flow, type = "Projected")
      
      end_date <- fr$forecast_date[1]
      
      transition_dates <- seq.Date(start_date, end_date, by = "day")
      Q_start <- df$Flow[df$Date == start_date][1]
      Q_end   <- fr$proj_flow[1]
      
      transition_plot <- tibble::tibble(
        Date = transition_dates,
        Flow = stats::approx(
          x = c(as.numeric(start_date), as.numeric(end_date)),
          y = c(Q_start, Q_end),
          xout = as.numeric(transition_dates)
        )$y
      )
      
      combined <- dplyr::bind_rows(hist_plot, proj_plot)
      
      plotly::plot_ly() |>
        plotly::add_lines(
          data = combined %>% dplyr::filter(type == "Observed"),
          x = ~Date, y = ~Flow,
          name = "Observed",
          mode = "lines"
        ) |>
        plotly::add_lines(
          data = transition_plot,
          x = ~Date, y = ~Flow,
          name = "Transition",
          line = list(dash = "dash", width = 2, color = "rgba(100,100,100,0.7)"),
          hovertemplate = paste0(
            "<b>Transition</b><br>",
            "Date: %{x}<br>",
            "Flow: %{y:.2f} cfs",
            "<extra></extra>"
          ),
          showlegend = FALSE
        ) |>
        plotly::add_lines(
          data = combined %>% dplyr::filter(type == "Projected"),
          x = ~Date, y = ~Flow,
          name = "Projected",
          mode = "lines+markers"
        ) |>
        plotly::layout(
          title = paste("Observed & Projected Flow -", site_name(), "(", toupper(data_source()), ")"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Flow (cfs)", rangemode = "tozero"),
          legend = list(orientation = "h", x = 0.1, y = -0.1)
        )
    })
    
  })
}
