## modules/droughtModuleServer.R
droughtModuleServer <- function(id, gage_obj) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 0. Core Data Initialize ####
    ## All Workflow Data ####
    workflow_data <- reactive({
      gage_id <- gage_obj()$gage_id
      req(gage_id)
      #Get all workflow data from gage
      out <- gage_obj()$baseflow_workflow_data(omsite = omsite)
      
      if(is.null(out)){
        showNotification("No baseflow forecasting data available for this gage.",
        type = "message", duration = NULL)
      }
      
      return(out)
    })
    
    ## Trimmed Stats data ####
    #Read in the trimmed stats from the workflow
    trimmed_points <- reactive({
      req(workflow_data())
      
      out <- workflow_data()[["trimmed_events_df"]]
      if(!is.null(out) & nrow(out) > 0){
        #Ensure all needed columns are present
        QC <- validate_required_cols(out, c("Date", "Flow", "AGWRC"),
                                     context = paste0("analysis points (gage)"))
        
        if(QC){
          #Make sure dates are date classes
          if (("Date" %in% names(out)) && !inherits(out$Date, "Date")) {
            out$Date <- as.Date(out$Date)
          }
          
          # Remove rows that can't support storage math
          out <- out %>%
            dplyr::filter(!is.na(.data$Date), !is.na(.data$Flow), !is.na(.data$AGWRC))
          message("Loaded analysis rows from om_site: ", nrow(out), " for gage_id = ", gage_obj()$gage_id, " (gage)")
        }else{
          out <- NULL
        }
      }
      
      if(is.null(out)){
        message("No trimmed stats found for gage.")
      }
      
      return(out)
    })
    
    ## Untrimmed data ####
    untrimmed_points <- reactive({
      req(workflow_data())
      
      out <- workflow_data()[["events_df"]]
      if(is.null(out) || nrow(out) == 0){
        message("No events found for gage.")
      }
      #Make sure dates are date classes
      if (!is.null(out) && nrow(out) > 0 && 
          ("Date" %in% names(out)) && !inherits(out$Date, "Date")) {
        out$Date <- as.Date(out$Date)
      }
      
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
      #Use either the gage id or the name used in the workflow
      df <- trimmed_points()
      if ("site_name" %in% names(df)) {
        nm <- unique(df$site_name)
        nm <- nm[!is.na(nm) & nzchar(nm)]
        if (length(nm) > 0){
          out <- nm[1]
        } 
      }else{
        out <- paste("USGS", gage_obj()$gage_id)
      }
      return(out)
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
    ## Event summary data ####
    events_summary <- reactive({
      req(workflow_data())
      #Get event summary data frame to allow for easy regression
      df <- workflow_data()[["event_summary_df"]]
      return(df)
    })
    
    ## Update event dateInputs ####
    observeEvent(events_summary(), {
      evt <- events_summary()
      req(evt)
      
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
      selected_event <- evt[s[1], ]
      
      
      #Update plot start and end date
      df <- raw_daily()
      #Minimum and maximum observed dates
      min_date <- min(df$Date, na.rm = TRUE)
      max_date <- max(df$Date, na.rm = TRUE)
      #Event start and end, to be modified to show more at the start and end:
      ev_start <- as.Date(selected_event$start_date[1])
      ev_end   <- as.Date(selected_event$end_date[1])
      updateDateRangeInput(
        session,
        "hist_date_range",
        start = max(min_date, ev_start %m-% lubridate::period(months = 9)),
        end   = min(max_date, ev_end   %m+% lubridate::period(months = 3))
      )
      
      return(selected_event)
    })
    
    output$historical_plot <- renderPlotly({
      df <- raw_daily()
      req(nrow(df) > 0)
      #Minimum and maximum observed dates
      min_date <- min(df$Date, na.rm = TRUE)
      max_date <- max(df$Date, na.rm = TRUE)
      
      #If user has provided a date range (which updates with selected events),
      #use it.
      dr <- input$hist_date_range
      if (is.null(dr) || any(is.na(dr))){
        window_start <-  max_date - lubridate::years(2)
        window_end   <- max_date
      } else{
        window_start <- max(min_date, as.Date(dr[1]))
        window_end   <- min(max_date, as.Date(dr[2]))
      }
      #Get any selected event
      ev <- selected_event()
      
      #Limit data to the user selected window
      df_window    <- df %>% 
        dplyr::filter(Date >= window_start, Date <= window_end)
      
      #Can only update plot if data was found:
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
      if(inherits(gage_obj(),"WaterGageDaily")){
        show_modal_spinner(text = "Retrieving baseflow data from DEQ servers...")
        #Use the gage object to get all props on the agwrc model if feature and
        #model exist, otherwise return NULL
        try_agwrc_props <- gage_obj()$agwrc_fun()
        if(!is.null(try_agwrc_props)){
          #Now stored on self from agwrc_fun()
          regression_m <- gage_obj()$agwrc_lm_m
          regression_b <- gage_obj()$agwrc_lm_b
          #If coefficients found, store stats:
          if(!is.na(regression_b) & !is.na(regression_m)){
            regression_m_pvalue <- try_agwrc_props[try_agwrc_props$propname == "regression_m_pvalue","propvalue"]
            regression_b_pvalue <- try_agwrc_props[try_agwrc_props$propname == "regression_b_pvalue","propvalue"]
            regression_Rsq <- try_agwrc_props[try_agwrc_props$propname == "regression_Rsq","propvalue"]

            out <- list(
              m = regression_m,
              b = regression_b,
              m_pvalue = regression_m_pvalue,
              b_pvalue = regression_b_pvalue,
              Rsq = regression_Rsq
            )
          }
        }
        remove_modal_spinner()
      }
      workflowLM(out)
    })
    
    ## Filter for regression inputs ####
    reg_events_filtered <- reactive({
      evt <- events_summary()
      req(nrow(evt) > 0)
      
      #Filter for dates
      dr <- input$reg_date_range
      if (!is.null(dr) && all(!is.na(dr))){
        start_win <- as.Date(dr[1])
        end_win   <- as.Date(dr[2])
        
        evt <- evt %>%
          dplyr::filter(
            end_date   >= start_win,
            start_date <= end_win
          )
      } 
      
      #Filter for flows
      if(!is.na(input$regression_flow_max)){
        evt <- evt[evt$median_flow < input$regression_flow_max,]
      }
      #Only keep events with non-NA flows and AGWRC
      evt <- evt %>% dplyr::filter(!is.na(event_AGWRC), !is.na(median_flow))
      
      return(evt)
    })
    
    ## AGWRC Population Stats ####
    output$agwrc_population_stats <- renderDT({
      evt <- reg_events_filtered()
      req(nrow(evt) > 0)
      
      agwrc_vals <- evt$event_AGWRC
      
      req(length(agwrc_vals) > 0)
      
      stats_df <- population_stats(agwrc_vals)
      
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
    ## Flow Pop Stats ####
    output$median_flow_population_stats <- renderDT({
      evt <- reg_events_filtered()
      req(nrow(evt) > 0)
      
      flow_vals <- evt$median_flow
      
      req(length(flow_vals) > 0)
      
      stats_df <- population_stats(flow_vals)
      
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
    #Calculate the regression between Flow and AGWRC based on user included date
    #range. We store the regression R object and a pred_df that has the
    #predicted data points along a vector of representative flows for the event
    user_regression <- reactiveVal(NULL)
    observeEvent(reg_events_filtered(),{
      #Reset reactive value
      user_regression(NULL)
      #Check to ensure events are populated
      evt <- reg_events_filtered()
      req(nrow(evt) > 1)
      
      #Create lm of log(q) and AGWRC and store model and prediction data frame
      #for plot
      # model <- stats::lm(event_AGWRC ~ log(median_flow), data = evt)
      model <- agws::fit_agwrc_regression(evt)
      
      flow_seq <- seq(min(evt$median_flow, na.rm = TRUE),
                      max(evt$median_flow, na.rm = TRUE),
                      length.out = 100)
      #Get all prediciton data
      pred_df <- as.data.frame(
        predict(model, interval = "confidence", 
                newdata = data.frame(logQ = log(flow_seq)))
      )
      #Reformat
      pred_df_all <- data.frame(
        median_flow = flow_seq,
        event_AGWRC = pred_df$fit,
        CI_low = pred_df$lwr,
        CI_high = pred_df$upr
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
          pred_df = pred_df_all,
          pred_df_workflow = pred_df_workflow
        )
      )
    })
    
    ## Regression Plot ####
    output$agwrc_regression_plot <- renderPlotly({
      req(user_regression(), full_storage_df())
      #For ggplot2::geom_line, add an arbitrary group var:
      user_data <- user_regression()$pred_df
      user_data$grp <- "all"
      
      #Base scatterplot
      p <- gage_obj()$plot_baseflow_agwrc(return_plotly = FALSE, CI = TRUE,
                                        omsite = omsite)
        # name = "User Regression fit"
      p <- p + ggplot2::geom_line(
        data = user_data, 
        aes(x = .data$median_flow, y = .data$event_AGWRC,
            group = .data$grp,
            color = "User Regression",
            text = paste0(
              "Median flow: ", round(.data$median_flow,2)," cfs<br>",
              "Event AGWRC: ", round(.data$event_AGWRC,5)
            )
        )
      ) + 
        scale_color_manual(breaks = c("WSPA Regression", "User Regression"),
                            values = c("steelblue2", "slateblue"))
      browser()
      return(plotly::ggplotly(p, tooltip = "text"))
    })
    
    ## User Regression Summary ####
    output$lm_user_summary <- renderPrint({
      evt <- reg_events_filtered()
      req(nrow(evt) > 1)
      summary(user_regression()$model)
    })
    
    output$lm_WSPA_summary <- renderPrint({
      req(workflowLM())
      browser()
      print(
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
          br(),
          h4("Untrimmed Event Data"),
          p("This table shows all available rows for the selected GroupID before the app removes rows with missing AGWRC values."),
          DTOutput(ns("event_untrimmed_table")),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
    ## Flow Plot ####
    output$event_flow_plot <- renderPlotly({
      df <- trimmed_points()
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
      df <- trimmed_points()
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
    ## untrimmed table ##
    output$event_untrimmed_table <- renderDT({
      df <- untrimmed_points()
      gid <- selected_group()
      req(!is.null(gid))
      
      df_event <- df %>%
        dplyr::filter(.data$GroupID == gid) %>%
        dplyr::arrange(.data$Date)
      
      req(nrow(df_event) > 0)
      
      display_cols <- intersect(
        c(
          "site_no",
          "GroupID",
          "Date",
          "Flow",
          "AGWR",
          "delta_AGWR",
          "calc_AGWR",
          "R_squared",
          "Season"
        ),
        names(df_event)
      )
      
      DT::datatable(
        df_event[, display_cols, drop = FALSE],
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # 70. Forecast Inputs UI ####
    output$agwrc_inputs <- renderUI({
      if(input$agwrc_calculation == "constant"){
        out <- tagList(
          numericInput(
            ns("agwrc_single"),
            label = "AGWRC (single daily ratio)",
            value = 1.0,
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
        #Otherwise use the regression AGWRC
        req(workflowLM())
        df <- full_storage_df()
        Q0 <- df$Flow[df$Date == input$forecast_start]
        if(length(Q0) > 0 && !is.null(Q0) && !is.na(Q0)){
          out <- agws::regressionLimitAGWRC(
            Flow = Q0, m = gage_obj()$agwrc_lm_m, b = gage_obj()$agwrc_lm_b,
            low_flow_limit = gage_obj()$agwrc_lm_limit$agwrc_reg_qlow,
            low_agwrc_limit = gage_obj()$agwrc_lm_limit$agwrc_reg_clow,
            high_flow_limit = gage_obj()$agwrc_lm_limit$agwrc_reg_qhigh,
            high_agwrc_limit = gage_obj()$agwrc_lm_limit$agwrc_reg_chigh
          )
        }else{
          out <- 1.0
        }
        
        initial_agwrc <- out
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
      out <- NULL
      if (is.null(ev$data)) {
        out <- p(strong("Baseflow event status:"),
                 br(),"Selected forecast start date is not within a known baseflow event."
        )
      } else {
        # reduce the number of AGWRC decimal points to 4
        reduced_dec <- sprintf("%.4f", ev$AGWRC)
        out <- p(strong("Baseflow event status:"),
                 br(),em("Known baseflow event"),
                 br(),em("GroupID:"), ev$GroupID,
                 br(),em("Event Dates:"), ev$start_date, 
                 "to", ev$end_date,
                 br(),em("Event AGWRC:"), reduced_dec
        )
      }
      return(out)
    })
    
    ## Constant AGWRC Recommendations ####
    #Calculate a recommended AGWRC from today's flow using user regression
    output$agwrc_user_lm <- renderText({
      req(user_regression(), full_storage_df())
      df <- full_storage_df()
      Q0 <- df$Flow[df$Date == input$forecast_start]
      if(length(Q0) > 0 && !is.null(Q0) && !is.na(Q0)){
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
      if(length(Q0) > 0 && !is.null(Q0) && !is.na(Q0)){
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
      df <- trimmed_points()
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
      agwrc <- input$agwrc_single
      req(!is.na(start_date), !is.na(agwrc))
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
          req(input$agwrc_single)
          ##### Constant ####
          agwrc <- input$agwrc_single
          #Projection
          proj_flow <- Q0 * (agwrc ^ (1:max(forecast_horizons)))
          proj_flow_in <- proj_flow * sp_conv
          #If flow is the metric, we estimate storage via Q / (1 - AGWRC)
          proj_storage_in <- proj_flow_in / (1 - agwrc)
          
        }else if(input$agwrc_calculation == "variable"){
          ##### Variable ####
          req(input$agwrc_regression)
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

