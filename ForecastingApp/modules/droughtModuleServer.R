## modules/droughtModuleServer.R
droughtModuleServer <- function(id, gage_obj) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 0. Core Data Initialize ####
    ## Read in Trimmed Stats data ####
    #Read in the trimmed stats from the workflow
    analysis_points <- reactive({
      gage_id <- gage_obj()$gage_id
      req(gage_id)
      
      out <- tryCatch(
        bf_get_analysis(gage_id, kind = "gage"),
        error = function(e) {
          showNotification(paste("Analysis CSV load failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      req(!is.null(out), nrow(out) > 0)
      message("Loaded analysis rows from om_site: ", nrow(out), " for gage_id = ", gage_id, " (gage)")
      return(out)
    })
    
    ## USGS Daily values with standard names ####
    raw_daily <- reactive({
      req(gage_obj())
      dv <- gage_obj()$gage_data |> 
        dplyr::transmute(
          Date = as.Date(.data[[gage_obj()$date_col]]),
          Flow = as.numeric(.data[[gage_obj()$flow_col]])
        ) %>%
        dplyr::arrange(Date)
      return(dv)
    })
    
    
    # 1. Convenience reactives ####
    ## Site Name ####
    #Used in plot labels and titles
    site_name <- reactive({
      df <- analysis_points()
      if ("site_name" %in% names(df)) {
        nm <- unique(df$site_name)
        nm <- nm[!is.na(nm) & nzchar(nm)]
        if (length(nm) > 0) return(nm[1])
      }
      paste("USGS", gage_obj()$gage_id)
    })
    
    ## Trimmed Points ####
    #Trimmed stats data frame, but now further validated and refined
    trimmed_points <- reactive({
      df <- analysis_points()
      req(nrow(df) > 0)
      
      # Date coercion (defensive)
      if (!inherits(df$Date, "Date")) {
        df$Date <- as.Date(df$Date)
      }
      
      QC <- validate_required_cols(df, c("Date", "Flow", "AGWRC"), context = paste0("analysis points (gage)"))
      
      if(QC){
        # Remove rows that can't support storage math
        df <- df %>%
          dplyr::filter(!is.na(.data$Date), !is.na(.data$Flow), !is.na(.data$AGWRC))
      }else{
        df <- NULL
      }
      return(df)
    })
    
    ## Trimmed DF ####
    # Copy of Trimmed Points
    trimmed_df <- reactive({
      trimmed_points()
    })
    
    # 1b. Storage (AGWS-equivalent) computed locally ####
    ## Calculate Trimmed Storage ####
    #WORK DONE HERE -> Separate Workflow?
    storage_points <- reactive({
      df <- trimmed_points()
      req(nrow(df) > 0)
      
      out <- tryCatch(
        add_storage_cols(
          df = df,
          data_obj = gage_obj,
          site_num = gage_obj()$gage_id,
          flow_col = "Flow",
          agwrc_col = "AGWRC"
        ),
        error = function(e) {
          showNotification(paste("Storage calculation failed:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      
      req(!is.null(out), nrow(out) > 0)
      QC <- validate_required_cols(out, c("Date", "Flow_in", "Storage_in"), context = "storage_points() output")
      if(!QC){
        out <- NULL
      }
      return(out)
    })
    
    ## Summarize Storage Events ####
    #WORK DONE HERE -> Separate Workflow?
    storage_event_sums <- reactive({
      req(storage_points())
      df <- storage_points()
      QC <- validate_required_cols(df, c("Date", "GroupID", "Flow_in", "Storage_in"),
                                   context = "storage_points() for event summary")
      if(QC & nrow(df) > 0){
        has_agwet <- "AGWET" %in% names(df)
        
        out <- df %>%
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
      }else{
        out <- NULL
      }
      
      return(out)
    })
    
    ## Interpolate Storage ####
    #WORK DONE HERE -> Move to object?
    #Interpolate storage between events
    full_storage_df <- reactive({
      req(storage_points())
      #Has user requested storage or flow?
      metric <- input$forecast_metric %||% "flow"
      sp <- storage_points()
      if(metric == "storage"){
        flow_storage <- raw_daily() |> 
          #Remove data prior to the first known baseflow event. This data has
          #unknown last AGWRC
          filter(Date >= min(sp$Date)) |> 
          left_join(sp, by = c("Date","Flow"))
        
        for(i in 1:nrow(flow_storage)){
          if(is.na(flow_storage$Storage_in[i])){
            #If today's storage is na, first use the previous day (the last known
            #baseflow event) AGWRC
            flow_storage$AGWRC[i] <- flow_storage$AGWRC[i - 1]
          }
        }
        #Now find storage based on that AGWRC
        storage_df_i <- add_storage_cols(
          df = flow_storage,
          data_obj = gage_obj,
          site_num = gage_obj()$gage_id,
          flow_col = "Flow",
          agwrc_col = "AGWRC"
        )
        #Store in flow_storage
        flow_storage$Flow_in <- storage_df_i$Flow_in
        flow_storage$Storage_in <- storage_df_i$Storage_in
      }else{
        flow_storage <- raw_daily()
      }
      
      return(flow_storage)
    })
    
    # 2. Event-level summary (for DT & regression) ####
    ## Read summary data ####
    events_summary <- reactive({
      df <- trimmed_df()
      req(nrow(df) > 0)
      #Read in baseflow event summary for gage:
      ows_results <- read_ows_data(gage_id = gage_obj()$gage_id,
                                   kind = "baseflow_summary",
                                   templates = BF_SUMMARY_TEMPLATES_DEFAULT,
                                   use_cache = TRUE)
      #Update cache:
      assign(ows_results$cache_key, ows_results$df, envir = .bf_cache)
      
      return(ows_results$df)
    })
    ## Update event dateInputs ####
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
    
    # 3. Historical plot (recent window) ####
    selected_event <- reactive({
      evt <- events_summary()
      s   <- input$events_table_rows_selected
      if (is.null(s) || length(s) == 0) return(NULL)
      evt[s[1], ]
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
        name = "USGS flow"
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
        paste("Recent Daily Flow -", site_name())
      } else {
        paste0(
          "Daily Flow (event window) - ", site_name(), " | GroupID ",
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
    
    # 4. Events table (DT) ####
    output$events_table <- renderDT({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      DT::datatable(
        evt,
        selection = "single",
        options = list(pageLength = 10),
        rownames = FALSE
      ) |> DT::formatRound(6,4)
    })
    
    # 5. AGWRC vs Flow regression (NOW FILTERED) ####
    ## Workflow data ####
    workflowLM <- reactiveVal(NULL)
    #If a gage object was returned, check to see if the AGWRC-1.0 simple_lm
    #workflow has been run for this gage. If so, an m and b coefficient should
    #already be stored in database
    observeEvent(gage_obj(),{
      out <- NULL
      if(inherits(gage_obj(),"waterGageDaily")){
        show_modal_spinner(text = "Retrieving baseflow data from DEQ servers...")
        gage_feature <- gage_obj()$load_wshd_feat()
        AGWRC_model <- gage_feature$get_prop(propcode = "AGWRC-1.0")
        regression_scenario <- AGWRC_model$get_prop(propcode = "simple_lm")
        regression_m <- regression_scenario$get_prop("regression_m")$propvalue
        regression_b <- regression_scenario$get_prop("regression_b")$propvalue
        
        if(!is.na(regression_b) & !is.na(regression_m)){
          regression_m_pvalue <- regression_scenario$get_prop("regression_m_pvalue")$propvalue
          regression_b_pvalue <- regression_scenario$get_prop("regression_b_pvalue")$propvalue
          regression_Rsq <- regression_scenario$get_prop("regression_Rsq")$propvalue
          out <- list(
            m = regression_m,
            b = regression_b,
            m_pvalue = regression_m_pvalue,
            b_pvalue = regression_b_pvalue,
            Rsq = regression_Rsq
          )
        }
      }
      workflowLM(out)
    })
    
    ## Filter for regression inputs ####
    reg_events_filtered <- reactive({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      dr <- input$reg_date_range
      if (is.null(dr) || any(is.na(dr))) return(evt)
      
      start_win <- as.Date(dr[1])
      end_win   <- as.Date(dr[2])
      
      evt <- evt %>%
        dplyr::filter(
          end_date   >= start_win,
          start_date <= end_win
        )
      
      if(!is.na(input$regression_flow_max)){
        evt <- evt[evt$median_flow < input$regression_flow_max,]
      }
      
      return(evt)
    })
    
    output$agwrc_population_stats <- renderDT({
      evt <- reg_events_filtered()
      req(nrow(evt) > 0)
      
      QC <- validate_required_cols(
        evt,
        c("event_AGWRC"),
        context = "events_summary() for AGWRC population stats"
      )
      req(QC)
      
      agwrc_vals <- evt$event_AGWRC
      agwrc_vals <- agwrc_vals[!is.na(agwrc_vals)]
      
      req(length(agwrc_vals) > 0)
      
      stats_df <- tibble::tibble(
        n_events = length(agwrc_vals),
        min      = min(agwrc_vals, na.rm = TRUE),
        p05      = unname(stats::quantile(agwrc_vals, 0.05, na.rm = TRUE)),
        p10      = unname(stats::quantile(agwrc_vals, 0.10, na.rm = TRUE)),
        p25      = unname(stats::quantile(agwrc_vals, 0.25, na.rm = TRUE)),
        median   = stats::median(agwrc_vals, na.rm = TRUE),
        mean     = mean(agwrc_vals, na.rm = TRUE),
        p75      = unname(stats::quantile(agwrc_vals, 0.75, na.rm = TRUE)),
        p90      = unname(stats::quantile(agwrc_vals, 0.90, na.rm = TRUE)),
        p95      = unname(stats::quantile(agwrc_vals, 0.95, na.rm = TRUE)),
        max      = max(agwrc_vals, na.rm = TRUE)
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) |>
        DT::formatRound(
          columns = c("min", "p05", "p10", "p25", "median", "mean", "p75", "p90", "p95", "max"),
          digits = 4
        )
    })
    
    output$median_flow_population_stats <- renderDT({
      evt <- reg_events_filtered()
      req(nrow(evt) > 0)
      
      QC <- validate_required_cols(
        evt,
        c("median_flow"),
        context = "events_summary() for median flow population stats"
      )
      req(QC)
      
      flow_vals <- evt$median_flow
      flow_vals <- flow_vals[!is.na(flow_vals)]
      
      req(length(flow_vals) > 0)
      
      stats_df <- tibble::tibble(
        n_events = length(flow_vals),
        min      = min(flow_vals, na.rm = TRUE),
        p05      = unname(stats::quantile(flow_vals, 0.05, na.rm = TRUE)),
        p10      = unname(stats::quantile(flow_vals, 0.10, na.rm = TRUE)),
        p25      = unname(stats::quantile(flow_vals, 0.25, na.rm = TRUE)),
        median   = stats::median(flow_vals, na.rm = TRUE),
        mean     = mean(flow_vals, na.rm = TRUE),
        p75      = unname(stats::quantile(flow_vals, 0.75, na.rm = TRUE)),
        p90      = unname(stats::quantile(flow_vals, 0.90, na.rm = TRUE)),
        p95      = unname(stats::quantile(flow_vals, 0.95, na.rm = TRUE)),
        max      = max(flow_vals, na.rm = TRUE)
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) |>
        DT::formatRound(
          columns = c("min", "p05", "p10", "p25", "median", "mean", "p75", "p90", "p95", "max"),
          digits = 2
        )
    })
    
    ## User Regression and Data Frame ####
    #WORK DONE HERE
    #Caclulate the regression between Flow and AGWRC based on user included date
    #range. We store the regression R object and a pred_df that has the
    #predicted data points along a vector of representative flows for the event
    user_regression <- reactiveVal(NULL)
    observeEvent(reg_events_filtered(),{
      #Reset reactive value
      user_regression(NULL)
      #Check to ensure events are populated
      evt <- reg_events_filtered()
      req(nrow(evt) > 1)
      
      evt <- evt %>% dplyr::filter(!is.na(event_AGWRC), !is.na(median_flow))
      req(nrow(evt) > 1)
      #Create lm of log(q) and AGWRC and store model and prediction data frame
      #for plot
      model <- stats::lm(event_AGWRC ~ log(median_flow), data = evt)
      flow_seq <- seq(min(evt$median_flow, na.rm = TRUE),
                      max(evt$median_flow, na.rm = TRUE),
                      length.out = 100)
      
      pred_df <- data.frame(
        median_flow = flow_seq,
        event_AGWRC = predict(model, newdata = data.frame(median_flow = flow_seq))
      )
      
      #If available, store workflow regression results as well:
      pred_df_workflow <- NULL
      if(!is.null(workflowLM())){
        flow_seq <- seq(min(events_summary()$median_flow, na.rm = TRUE),
                        max(events_summary()$median_flow, na.rm = TRUE),
                        length.out = 100)
        
        pred_df_workflow <- data.frame(
          median_flow = flow_seq,
          event_AGWRC = (workflowLM()$m * log(flow_seq) + workflowLM()$b)
        )
      }
      
      #Update user regression object
      user_regression(
        list(
          model = model,
          pred_df = pred_df,
          pred_df_workflow = pred_df_workflow
        )
      )
    })
    ## Regression Plot ####
    output$agwrc_regression_plot <- renderPlotly({
      req(user_regression())
      
      p <- plotly::plot_ly() |>
        plotly::add_markers(
          data = events_summary(),
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
          data = user_regression()$pred_df,
          x    = ~median_flow,
          y    = ~event_AGWRC,
          name = "User Regression fit"
        ) 
      #Add WSPA regression workflow if available
      if(!is.null(user_regression()$pred_df_workflow)){
        p <- p |>
          plotly::add_lines(
            data = user_regression()$pred_df_workflow,
            x    = ~median_flow,
            y    = ~event_AGWRC,
            name = "WSPA Regression fit"
          ) 
      }
      
      p |>
        plotly::layout(
          title = paste("AGWRC vs Flow (event-level) -", site_name()),
          xaxis = list(title = "Characteristic event flow (median, cfs)", rangemode = "tozero"),
          yaxis = list(title = "Event AGWRC")
        )
    })
    ## User Regression Summary ####
    output$lm_user_summary <- renderPrint({
      evt <- reg_events_filtered()
      req(nrow(evt) > 1)
      summary(user_regression()$model)
    })
    
    output$lm_WSPA_summary <- renderPrint({
      req(workflowLM())
      
      cat(
          "Slope (m) = ",workflowLM()$m,"\n",
          "Intercept (b) = ",workflowLM()$b,"\n",
          "Slope p-value = ",signif(workflowLM()$m_pvalue,4),"\n",
          "Intercept b-pvalue = ",signif(workflowLM()$b_pvalue,4),"\n",
          "R Squared = ",round(workflowLM()$Rsq,4)
      )
      
    })
    
    
    # 6. Event inspection modal ####
    selected_group <- reactiveVal(NULL)
    ## Show Modal ####
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
    ## Flow Plot ####
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
    ## AGWR Plot ####
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
    # 70. Forecast Inputs UI ####
    output$agwrc_inputs <- renderUI({
      if(input$agwrc_calculation == "constant"){
        out <- tagList(
          numericInput(
            ns("agwrc_single"),
            label = "AGWRC (single daily ratio)",
            value = 0.97,
            min = 0.0,
            max = 1.2,
            step = 0.001
          ),
          fluidRow(
            column(6,
                   p("User Regression Predicted AGWRC:"),
                   p("WSPA Regression Predicted AGWRC:")
            ),
            column(6,
                   verbatimTextOutput(ns("agwrc_user_lm")),
                   verbatimTextOutput(ns("agwrc_wspa_lm"))
            )
          )
        )
      }else if(input$agwrc_calculation == "variable"){
        out <- tagList(
          radioButtons(
            ns("agwrc_regression"),
            label = "Which Regression should be used to determine variable AGWRC?",
            choiceNames = c("User", "WSPA"),
            choiceValues = c("user","wspa")
          )
        )
      }
    })
    
    
    # 7a. Auto-default AGWRC ####
    observeEvent(list(input$forecast_start, events_summary()), {
      req(input$forecast_start)
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      sd <- as.Date(input$forecast_start)
      
      hit <- evt %>%
        dplyr::filter(start_date <= sd, end_date >= sd) %>%
        dplyr::slice(1)
      
      if (nrow(hit) == 1 && !is.na(hit$event_AGWRC)) {
        initial_agwrc <- hit$event_AGWRC
      }else{
        #Otherwise use the mean event AGWRC found
        initial_agwrc <- mean(evt$event_AGWRC, na.rm = TRUE)
      }
      updateNumericInput(session, "agwrc_single", value = round(initial_agwrc, 3))
    }, ignoreInit = TRUE)
    
    # Identify known baseflow periods 
    current_baseflow_event <- reactive({
      req(input$forecast_start)
      
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      evt <- evt %>%
        dplyr::filter(
          start_date <= as.Date(input$forecast_start),
          end_date >= as.Date(input$forecast_start)
        )
      
      if (nrow(evt) == 0) {
        return(list(
          data = NULL,
          GroupID = NULL,
          start_date = NULL,
          end_date = NULL
        ))
      }
      
      list(
        data = evt,
        GroupID = evt$GroupID[1],
        start_date = evt$start_date[1],
        end_date = evt$end_date[1],
        AGWRC = evt$event_AGWRC[1]
      )
    })
    
    # define the UI output
    output$baseflow_event_info <- renderUI({
        ev <- current_baseflow_event()
      
      if (is.null(ev$data)) {
      paste("Baseflow event status:",
            "\nselected forecast start date is not within a known baseflow event."
        )
      } else {
      # reduce the number of AGWRC decimal points to 4
      reduced_dec <- sprintf("%.4f", ev$AGWRC)
      paste("Baseflow event status:",
              "Known baseflow event",
              "GroupID:", ev$GroupID,
              "Event Dates:", ev$start_date, 
              "to", ev$end_date,
              "Event AGWRC:", reduced_dec,
              "(use as the static default)"
            )
       }
    })
    
    ## Constant AGWRC Recommendations ####
    #Calculate a recommended AGWRC from today's flow using user regression
    output$agwrc_user_lm <- renderText({
      req(user_regression())
      df <- full_storage_df()
      Q0 <- df$Flow[df$Date == input$forecast_start]
      if(length(Q0) > 0 && !is.null(Q0) && is.na(Q0)){
        out <- coef(user_regression()$model)[2] * log(Q0) + coef(user_regression()$model)[1]
      }else{
        out <- NULL
      }
      return(out)
    })
    #Calculate a recommended AGWRC from today's flow using WSPA regression
    output$agwrc_wspa_lm <- renderText({
      req(workflowLM())
      df <- full_storage_df()
      Q0 <- df$Flow[df$Date == input$forecast_start]
      if(length(Q0) > 0 && !is.null(Q0) && is.na(Q0)){
        out <- workflowLM()$m * log(Q0) + workflowLM()$b
      }else{
        out <- NULL
      }
      return(out)
    })
    #Calculate last known AGWRC value
    last_known_agwrc <- reactive({
      # run the forecast_start code first
      req(input$forecast_start)
      
      # save the input date as a variable
      selected_date <- as.Date(input$forecast_start)
      
      # filter the trimmed data to be on or before the input date
      df <- trimmed_df()
      filtered_df <- df %>% 
        dplyr::filter(Date <= selected_date,
                      !is.na(AGWRC)) %>%
        dplyr::arrange(Date)
      
      # ensure that df has at least one row to prevent errors
      req(nrow(filtered_df) > 0)
     
      # select the last row from the filtered dates
      last_row <- dplyr::slice_tail(filtered_df, n = 1)
      
      # create a list to return date, AGWRC, and GroupID
      list(
        AGWRC = last_row$AGWRC,
        Date = last_row$Date,
        GroupID = last_row$GroupID
      )
      
      # reduce the number of AGWRC decimal points to 4
      reduced_dec <- sprintf("%.4f", last_row$AGWRC)
      
      # paste these values vertically in the output text
      paste(
        "Last known AGWRC:", reduced_dec,
        "\nDate:", last_row$Date,
        "\nGroupID:", last_row$GroupID
      )
    })
    
    # define the output for verbatimTextOuptut in UI
    output$last_known_agwrc <- renderText({
      req(last_known_agwrc())
    })

    # 7b. Forecast logic (single AGWRC for now) + AGWS display ####
    #Update the date input with the max date found in the raw data
    observeEvent(raw_daily(), {
      df <- raw_daily()
      if (nrow(df) > 0) {
        updateDateInput(session, "forecast_start", value = max(df$Date, na.rm = TRUE))
      }
    }, ignoreInit = FALSE)
    
    forecast_horizons <- c(15, 30, 45, 90)
    ## Calculate forecast data frame ####
    #WORK DONE HERE -> Move to object?
    #Calculate forecast results and return a data frame that has the forecast
    #values, date, and days after start (integer)
    forecast_results <- reactive({
      df <- full_storage_df()
      req(nrow(df) > 0)
      start_date <- as.Date(input$forecast_start)
      req(!is.na(start_date))
      #QC checks: Flow must exist on the start date and the input AGWRC must be
      #valid
      Q0 <- df$Flow[df$Date == start_date]
      if (length(Q0) == 0) {
        showNotification("Selected forecast start date has no flow record.", type = "error")
        return(NULL)
      }
      if(!is.null(input$agwrc_single)){
        if (is.na(input$agwrc_single) || input$agwrc_single >= 1 || input$agwrc_single <= 0) {
          showNotification("Selected AGWRC must be between 0 and 1.", type = "error")
          return(NULL)
        }
      }
      
      metric <- input$forecast_metric %||% "flow"
      
      # Convert projected flow to inches/day (used for diagnostics/table; not required for storage projection)
      da <- gage_obj()$drainage_area
      sp_conv <- if (!is.na(da) && da > 0) ((86400 * 12) / (5280 * 5280)) / da else NA_real_
      
      ### Storage and Flow projection: ####
      #If metric is storage, project storage and calculate flow. If metric is
      #flow, project flow and calculate storage.
      if (metric == "storage") {
        sp <- storage_points()
        if (is.null(sp) || nrow(sp) == 0 || !(start_date %in% sp$Date)) {
          showNotification(paste0("The selected start date falls outside of known 
                           baseflow periods. Storage will be estimated between
                                  baseflow periods based on flow and last known 
                                  recession coefficients. The input AGWRC will
                                  ONLY be used in the forward projection."),
                           type = "message", duration = 10)
        }
        S0 <- df$Storage_in[df$Date == start_date]
        #### Projected Storage ####
        agwrc      <- input$agwrc_single
        proj_storage_in <- S0 * (agwrc ^ (1:max(forecast_horizons)))
        # Recalculate flow
        proj_flow_in <- (1 - agwrc) * proj_storage_in
        proj_flow <- proj_flow_in / sp_conv
      } else {
        #### Flow Projection ####
        #Initial Flow
        Q0 <- as.numeric(Q0[1])
        if(input$agwrc_calculation == "constant"){
          ##### Constant ####
          agwrc <- input$agwrc_single
          #Projection
          proj_flow <- Q0 * (agwrc ^ (1:max(forecast_horizons)))
          proj_flow_in <- proj_flow * sp_conv
          #If flow is the metric, we estimate storage via Q / (1 - AGWRC)
          proj_storage_in <- proj_flow_in / (1 - agwrc)
          
        }else if(input$agwrc_calculation == "variable"){
          ##### Variable ####
          proj_flow <- numeric(max(forecast_horizons))
          proj_flow_in <- numeric(max(forecast_horizons))
          proj_storage_in <- numeric(max(forecast_horizons))
          agwrc <- numeric(max(forecast_horizons))
          
          if(input$agwrc_regression == "user"){
            m <- coef(user_regression()$model)[2]
            b <- coef(user_regression()$model)[1]
          }else if(input$agwrc_regression == "wspa"){
            m <- workflowLM()$m
            b <- workflowLM()$b
          }
          #Inital value
          agwrc[1] <- m * log(Q0) + b
          proj_flow[1] <- Q0 * agwrc[1]
          proj_flow_in[1] <- proj_flow[1] * sp_conv
          proj_storage_in[1] <- proj_flow_in[1] / (1 - agwrc[1])
          #Iterate using variable agwr calculated from regression
          for(i in 2:max(forecast_horizons)){
            agwrc[i] <- m * log(proj_flow[i - 1]) + b
            #Projection
            proj_flow[i] <- proj_flow[i - 1] * agwrc[i]
            proj_flow_in[i] <- proj_flow[i] * sp_conv
            #If flow is the metric, we estimate storage via Q / (1 - AGWRC)
            proj_storage_in[i] <- proj_flow_in[i] / (1 - agwrc[i])
          }
        }
        
      }
      #Assemble an output
      out <- tibble::tibble(
        horizon_days      = 1:max(forecast_horizons),
        forecast_date     = start_date + horizon_days,
        AGWRC             = agwrc,
        proj_flow_cfs     = proj_flow,
        proj_flow_in_day  = proj_flow_in,
        proj_storage_in   = proj_storage_in
      )
      return(out)
    })
    ## Tables ####
    output$forecast_table <- renderDT({
      fr <- forecast_results()
      req(fr)
      df <- full_storage_df()
      fr <- fr[fr$horizon_days %in% forecast_horizons,] |> 
        left_join(df, by = c("forecast_date" = "Date"))
      
      DT::datatable(
        fr %>%
          dplyr::transmute(
            horizon_days,
            forecast_date,
            AGWRC,
            proj_flow_cfs    = round(proj_flow_cfs, 2),
            proj_storage_in  = round(proj_storage_in, 4),
            obs_flow_cfs = round(Flow, 2)
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
    ## Plot ####
    output$forecast_plot <- renderPlotly({
      metric <- input$forecast_metric %||% "flow"
      df <- full_storage_df()
      fr <- forecast_results()
      req(nrow(df) > 0, fr)
      
      start_date <- as.Date(input$forecast_start)
      
      hist_window <- 90
      
      #Filter data to only that relevant to this plot
      df_hist <- df %>% dplyr::filter(Date >= start_date - hist_window & Date <= start_date + hist_window)
      req(nrow(df_hist) > 1)
      
      if (metric == "storage") {
        #Combine historic and projected data frames in a long style format with
        #a value field for flow and a type field for the historic or projected
        hist_plot <- df_hist %>% dplyr::transmute(Date = Date, value = Storage_in, type = "Observed")
        proj_plot <- fr %>% dplyr::transmute(Date = forecast_date, value = proj_storage_in, type = "Projected")
        type_label <- "Storage"
        type_unit <- "in/day"
      } else {
        #Combine historic and projected data frames in a long style format with
        #a value field for flow and a type field for the historic or projected
        hist_plot <- df_hist %>% dplyr::transmute(Date = Date, value = Flow, type = "Observed")
        proj_plot <- fr %>% dplyr::transmute(Date = forecast_date, value = proj_flow_cfs, type = "Projected")
        type_label <- "Flow"
        type_unit <- "cfs"
      }
      #Combine data
      combined <- dplyr::bind_rows(hist_plot, proj_plot)
      
      #Plot:
      plotly::plot_ly() |>
        plotly::add_lines(
          data = combined %>% dplyr::filter(type == "Observed",
                                            Date >= start_date - hist_window & Date <= start_date),
          x = ~Date, y = ~value,
          name = paste("Observed",type_label),
          mode = "lines"
        ) |>
        plotly::add_lines(
          data = combined %>% dplyr::filter(type == "Projected"),
          x = ~Date, y = ~round(value,3),
          name = paste("Projected",type_label),
          mode = "lines+markers",
          line = list(dash = "dash")
        ) |>
        #Add future observed values for past dates
        plotly::add_lines(
          data = combined %>% dplyr::filter(type == "Observed",
                                            Date >= start_date & Date <= start_date + hist_window),
          x = ~Date, y = ~value,
          name = paste("Observed",type_label),
          mode = "lines",
          line = list(dash = "dash",
                      color = "#1f77b4")
        ) |>
        plotly::layout(
          title = paste("Observed & Projected",type_label,"-", site_name()),
          xaxis = list(title = "Date"),
          yaxis = list(title = paste0(type_label, " (",type_unit,")"), rangemode = "tozero")
        )
    })
    
  })
}

