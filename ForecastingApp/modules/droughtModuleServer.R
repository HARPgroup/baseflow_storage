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
      
      flow_col <- if ("Qout" %in% names(raw)) "Qout" else if ("Flow" %in% names(raw)) "Flow" else NA_character_
      req(!is.na(flow_col))
      
      date_vec <- NULL
      if ("thisdate" %in% names(raw)) date_vec <- raw$thisdate
      if (is.null(date_vec) && "Date" %in% names(raw)) date_vec <- raw$Date
      
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
      trimmed_points()
    })
    
    
    # ---------------------------------------------
    # 0b. Storage (AGWS-equivalent) computed locally
    # ---------------------------------------------
    # Storage is computed from the analyzed "points" table for the ACTIVE source (model|gage):
    #   - required: Date, Flow (cfs), AGWRC
    #   - optional: kept, met_alpha (used for trimming if present)
    #
    # This reproduces the middle portion of SF_event_summary.R (Flow_in + Storage_in),
    # but keeps everything local (no GitHub runtime sourcing) for reproducibility.
    
    validate_required_cols <- function(df, required, context = "data") {
      missing <- setdiff(required, names(df))
      if (length(missing) > 0) {
        msg <- paste0(
          "Missing required column(s) in ", context, ": ",
          paste(missing, collapse = ", "),
          ".\n\n",
          "Available columns: ", paste(names(df), collapse = ", ")
        )
        showNotification(msg, type = "error", duration = NULL)
        stop(msg, call. = FALSE)
      }
      invisible(TRUE)
    }
    
    trimmed_points <- reactive({
      df <- analysis_points()
      req(nrow(df) > 0)
      
      # Date coercion (defensive)
      if (!inherits(df$Date, "Date")) {
        df$Date <- as.Date(df$Date)
      }
      
      validate_required_cols(df, c("Date", "Flow", "AGWRC"), context = paste0("analysis points (", data_source(), ")"))
      
      # Optional trimming: only apply if columns exist
      if (all(c("kept", "met_alpha") %in% names(df))) {
        df <- df %>% dplyr::filter(.data$kept == TRUE, .data$met_alpha == TRUE)
      }
      
      # Remove rows that can't support storage math
      df <- df %>%
        dplyr::filter(!is.na(.data$Date), !is.na(.data$Flow), !is.na(.data$AGWRC))
      
      df
    })
    
    storage_points <- reactive({
      df <- trimmed_points()
      req(nrow(df) > 0)
      
      out <- tryCatch(
        add_storage_cols(
          df = df,
          site_num = gage_id(),
          flow_col = "Flow",
          agwrc_col = "AGWRC"
        ),
        error = function(e) {
          showNotification(paste("Storage calculation failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      
      req(!is.null(out), nrow(out) > 0)
      validate_required_cols(out, c("Date", "Flow_in", "Storage_in"), context = "storage_points() output")
      
      out
    })
    
    storage_event_sums <- reactive({
      df <- storage_points()
      req(nrow(df) > 0)
      
      validate_required_cols(df, c("Date", "GroupID", "Flow_in", "Storage_in"), context = "storage_points() for event summary")
      
      has_agwet <- "AGWET" %in% names(df)
      
      df %>%
        dplyr::arrange(.data$GroupID, .data$Date) %>%
        dplyr::group_by(.data$GroupID) %>%
        dplyr::summarise(
          start_date = min(.data$Date, na.rm = TRUE),
          end_date   = max(.data$Date, na.rm = TRUE),
          Storage_0  = .data$Storage_in[which.min(.data$Date)],
          Storage_f  = .data$Storage_in[which.max(.data$Date)],
          Flow_tot   = sum(.data$Flow_in, na.rm = TRUE),
          AGWET_tot  = if (has_agwet) sum(.data$AGWET, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          remainder = .data$Storage_0 - .data$Flow_tot - .data$Storage_f
        )
    })
    
    # When plotting/forecasting STORAGE, enforce that the chosen start date exists in storage_points().
    observeEvent(list(input$forecast_metric, storage_points()), {
      metric <- input$forecast_metric %||% "flow"
      if (metric != "storage") return()
      
      sp <- storage_points()
      req(nrow(sp) > 0)
      
      storage_dates <- sort(unique(sp$Date))
      min_d <- min(storage_dates, na.rm = TRUE)
      max_d <- max(storage_dates, na.rm = TRUE)
      
      current <- as.Date(input$forecast_start)
      if (is.na(current) || !(current %in% storage_dates)) {
        # default to latest available storage date
        updateDateInput(session, "forecast_start", value = max_d, min = min_d, max = max_d)
        showNotification(
          paste0("Storage forecast start date must exist in Storage_in series. Set to ", as.character(max_d), "."),
          type = "message",
          duration = 6
        )
      } else {
        # also constrain picker range when storage is selected
        updateDateInput(session, "forecast_start", min = min_d, max = max_d)
      }
    }, ignoreInit = TRUE)
    
    
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
    
    observeEvent(events_summary(), {
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      min_d <- min(evt$start_date, na.rm = TRUE)
      max_d <- max(evt$end_date,   na.rm = TRUE)
      
      updateDateRangeInput(
        session,
        "reg_date_range",
        start = min_d,
        end   = max_d,
        min   = min_d,
        max   = max_d
      )
    }, ignoreInit = FALSE)
    
    reg_events_filtered <- reactive({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      dr <- input$reg_date_range
      if (is.null(dr) || any(is.na(dr))) return(evt)
      
      start_win <- as.Date(dr[1])
      end_win   <- as.Date(dr[2])
      
      evt %>%
        dplyr::filter(
          end_date   >= start_win,
          start_date <= end_win
        )
    })
    
    # ---------------------------------------------
    # 3. Historical plot (recent window)
    # ---------------------------------------------
    selected_event <- reactive({
      evt <- events_summary()
      s   <- input$events_table_rows_selected
      if (is.null(s) || length(s) == 0) return(NULL)
      evt[s[1], , drop = FALSE]
    })
    
    output$historical_plot <- renderPlotly({
      df <- raw_daily()
      req(nrow(df) > 0)
      
      max_date <- max(df$Date, na.rm = TRUE)
      
      window_start <- max_date - lubridate::years(2)
      window_end   <- max_date
      
      ev <- selected_event()
      if (!is.null(ev)) {
        ev_start <- as.Date(ev$start_date[1])
        ev_end   <- as.Date(ev$end_date[1])
        if (!is.na(ev_start) && !is.na(ev_end)) {
          window_start <- ev_start %m-% lubridate::period(months = 9)
          desired_end  <- ev_end   %m+% lubridate::period(months = 3)
          window_end   <- min(desired_end, max_date, na.rm = TRUE)
        }
      }
      
      window_start <- max(window_start, min(df$Date, na.rm = TRUE), na.rm = TRUE)
      df_window    <- df %>% dplyr::filter(Date >= window_start, Date <= window_end)
      req(nrow(df_window) > 1)
      
      p <- plotly::plot_ly(
        df_window,
        x = ~Date,
        y = ~Flow,
        type = "scatter",
        mode = "lines",
        name = if (data_source() == "model") "Model flow" else "USGS flow"
      )
      
      if (!is.null(ev)) {
        ev_start <- as.Date(ev$start_date[1])
        ev_end   <- as.Date(ev$end_date[1])
        if (!is.na(ev_start) && !is.na(ev_end)) {
          y_max <- max(df_window$Flow, na.rm = TRUE)
          p <- p |>
            plotly::add_segments(
              x = ev_start, xend = ev_start,
              y = 0, yend = y_max,
              inherit = FALSE,
              line = list(dash = "dot"),
              showlegend = FALSE,
              hovertemplate = paste0("Event start: ", ev_start, "<extra></extra>")
            ) |>
            plotly::add_segments(
              x = ev_end, xend = ev_end,
              y = 0, yend = y_max,
              inherit = FALSE,
              line = list(dash = "dot"),
              showlegend = FALSE,
              hovertemplate = paste0("Event end: ", ev_end, "<extra></extra>")
            )
        }
      }
      
      title_txt <- if (is.null(ev)) {
        paste("Recent Daily Flow -", site_name(), "(", toupper(data_source()), ")")
      } else {
        paste0(
          "Daily Flow (event window) - ", site_name(), " (", toupper(data_source()), ") | GroupID ",
          ev$GroupID[1]
        )
      }
      
      p |>
        plotly::layout(
          title = title_txt,
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
    # 5. AGWRC vs Flow regression (NOW FILTERED)
    # ---------------------------------------------
    output$agwrc_regression_plot <- renderPlotly({
      evt <- reg_events_filtered()
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
      evt <- reg_events_filtered()
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
    # 7. Forecast logic (single AGWRC for now) + AGWS display
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
      
      start_date <- as.Date(input$forecast_start)
      agwrc      <- input$agwrc_single
      req(!is.na(start_date), !is.na(agwrc))
      
      df <- df %>% dplyr::arrange(Date)
      Q0 <- df$Flow[df$Date == start_date]
      if (length(Q0) == 0) {
        showNotification("Selected forecast start date has no flow record.", type = "error")
        return(NULL)
      }
      Q0 <- as.numeric(Q0[1])
      
      proj_flow <- Q0 * (agwrc ^ forecast_horizons)
      
      
      metric <- input$forecast_metric %||% "flow"
      
      # Convert projected flow to inches/day (used for diagnostics/table; not required for storage projection)
      da <- da_sqmi(gage_id())
      sp_conv <- if (!is.na(da) && da > 0) ((86400 * 12) / (5280 * 5280)) / da else NA_real_
      proj_flow_in <- proj_flow * sp_conv
      
      # Storage projection:
      # - if metric == "storage": anchor to observed Storage_in at start_date (must exist)
      # - else: keep a simple derived estimate for display (may be NA if DA missing)
      if (metric == "storage") {
        sp <- storage_points()
        if (is.null(sp) || nrow(sp) == 0 || !(start_date %in% sp$Date)) {
          showNotification("Storage forecast start date must exist in Storage_in series (after trimming).", type = "error", duration = 10)
          return(NULL)
        }
        S0 <- sp$Storage_in[sp$Date == start_date][1]
        if (is.na(S0)) {
          showNotification("Storage_in at the selected start date is NA; cannot project storage.", type = "error", duration = 10)
          return(NULL)
        }
        proj_storage_in <- S0 * (agwrc ^ forecast_horizons)
      } else {
        proj_storage_in <- if (isTRUE(agwrc < 0.9999) && !is.na(sp_conv)) {
          proj_flow_in / (1 - agwrc)
        } else {
          rep(NA_real_, length(proj_flow))
        }
      }
      
      tibble::tibble(
        horizon_days      = forecast_horizons,
        forecast_date     = start_date + horizon_days,
        AGWRC             = agwrc,
        proj_flow_cfs     = proj_flow,
        proj_flow_in_day  = proj_flow_in,
        proj_storage_in   = proj_storage_in
      )
    })
    
    output$forecast_table <- renderDT({
      fr <- forecast_results()
      req(fr)
      
      DT::datatable(
        fr %>%
          dplyr::transmute(
            horizon_days,
            forecast_date,
            AGWRC,
            proj_flow_cfs    = round(proj_flow_cfs, 2),
            proj_storage_in  = round(proj_storage_in, 4)
          ),
        rownames = FALSE,
        options = list(dom = "tp", pageLength = 6)
      )
    })
    
    output$storage_event_table <- renderDT({
      evs <- storage_event_sums()
      req(nrow(evs) > 0)
      
      DT::datatable(
        evs %>%
          dplyr::mutate(
            Storage_0 = round(Storage_0, 4),
            Storage_f = round(Storage_f, 4),
            Flow_tot  = round(Flow_tot, 4),
            AGWET_tot = round(AGWET_tot, 4),
            remainder = round(remainder, 4)
          ),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    output$forecast_plot <- renderPlotly({
      metric <- input$forecast_metric %||% "flow"
      
      df <- raw_daily()
      fr <- forecast_results()
      req(nrow(df) > 0, fr)
      
      start_date <- as.Date(input$forecast_start)
      agwrc      <- input$agwrc_single
      req(!is.na(start_date), !is.na(agwrc))
      
      hist_window <- 90
      
      if (metric == "storage") {
        ed <- tryCatch(storage_points(), error = function(e) NULL)
        if (is.null(ed) || nrow(ed) == 0) {
          showNotification("No historical Storage_in available for this site.", type = "warning", duration = 6)
          return(plotly::plotly_empty())
        }
        
        
        
        # Enforce start_date exists in storage series
        if (!(start_date %in% ed$Date)) {
          showNotification(
            "Storage forecast start date must exist in Storage_in series (after trimming). Choose a highlighted/valid date or switch to Flow.",
            type = "error", duration = 10
          )
          return(plotly::plotly_empty())
        }
        ed_win <- ed %>%
          dplyr::filter(Date >= start_date - hist_window, Date <= start_date) %>%
          dplyr::arrange(Date)
        req(nrow(ed_win) > 1)
        
        hist_plot <- ed_win %>% dplyr::transmute(Date = Date, value = Storage_in, type = "Observed")
        proj_plot <- fr %>% dplyr::transmute(Date = forecast_date, value = proj_storage_in, type = "Projected")
        
        S_start <- ed$Storage_in[ed$Date == start_date]
        if (length(S_start) == 0 || is.na(S_start[1])) {
          da <- da_sqmi(gage_id())
          sp_conv <- if (!is.na(da) && da > 0) ((86400 * 12) / (5280 * 5280)) / da else NA_real_
          Q_start <- df$Flow[df$Date == start_date][1]
          S_start <- if (isTRUE(agwrc < 0.9999) && !is.na(sp_conv)) (Q_start * sp_conv) / (1 - agwrc) else NA_real_
        } else {
          S_start <- S_start[1]
        }
        
        end_date <- fr$forecast_date[1]
        S_end    <- fr$proj_storage_in[1]
        
        transition_dates <- seq.Date(start_date, end_date, by = "day")
        transition_vals <- stats::approx(
          x = c(as.numeric(start_date), as.numeric(end_date)),
          y = c(S_start, S_end),
          xout = as.numeric(transition_dates)
        )$y
        
        transition_plot <- tibble::tibble(Date = transition_dates, value = transition_vals)
        combined <- dplyr::bind_rows(hist_plot, proj_plot)
        
        plotly::plot_ly() |>
          plotly::add_lines(
            data = combined %>% dplyr::filter(type == "Observed"),
            x = ~Date, y = ~value,
            name = "Observed storage",
            mode = "lines"
          ) |>
          plotly::add_lines(
            data = transition_plot,
            x = ~Date, y = ~value,
            name = "Transition",
            line = list(dash = "dash", width = 2, color = "rgba(100,100,100,0.7)"),
            hovertemplate = paste0(
              "<b>Transition</b><br>",
              "Date: %{x}<br>",
              "Storage: %{y:.4f} in",
              "<extra></extra>"
            ),
            showlegend = FALSE
          ) |>
          plotly::add_lines(
            data = combined %>% dplyr::filter(type == "Projected"),
            x = ~Date, y = ~value,
            name = "Projected storage",
            mode = "lines+markers"
          ) |>
          plotly::layout(
            title = paste("Observed & Projected Storage (AGWS) -", site_name(), "(", toupper(data_source()), ")"),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Storage (in)", rangemode = "tozero"),
            legend = list(orientation = "h", x = 0.1, y = -0.1)
          )
      } else {
        df_hist <- df %>% dplyr::filter(Date >= start_date - hist_window & Date <= start_date)
        req(nrow(df_hist) > 1)
        
        hist_plot <- df_hist %>% dplyr::transmute(Date = Date, value = Flow, type = "Observed")
        proj_plot <- fr %>% dplyr::transmute(Date = forecast_date, value = proj_flow_cfs, type = "Projected")
        
        end_date <- fr$forecast_date[1]
        transition_dates <- seq.Date(start_date, end_date, by = "day")
        Q_start <- df$Flow[df$Date == start_date][1]
        Q_end   <- fr$proj_flow_cfs[1]
        
        transition_vals <- stats::approx(
          x = c(as.numeric(start_date), as.numeric(end_date)),
          y = c(Q_start, Q_end),
          xout = as.numeric(transition_dates)
        )$y
        
        transition_plot <- tibble::tibble(Date = transition_dates, value = transition_vals)
        combined <- dplyr::bind_rows(hist_plot, proj_plot)
        
        plotly::plot_ly() |>
          plotly::add_lines(
            data = combined %>% dplyr::filter(type == "Observed"),
            x = ~Date, y = ~value,
            name = "Observed flow",
            mode = "lines"
          ) |>
          plotly::add_lines(
            data = transition_plot,
            x = ~Date, y = ~value,
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
            x = ~Date, y = ~value,
            name = "Projected flow",
            mode = "lines+markers"
          ) |>
          plotly::layout(
            title = paste("Observed & Projected Flow -", site_name(), "(", toupper(data_source()), ")"),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Flow (cfs)", rangemode = "tozero"),
            legend = list(orientation = "h", x = 0.1, y = -0.1)
          )
      }
    })
    
  })
}
