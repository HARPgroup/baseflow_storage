# low flow stuff
library("hydrotools")
library("agws")
library("dataRetrieval")
basedir = "http://deq1.bse.vt.edu:81/usgs/agws/"

gage_id = "01634000"
glist = c(gage_id)
g_roanoke = c("02065500", "02059500", "02056000","02054530", "02056900")
glist = c(
  "03524000", "03167000", "01674500", "01667500",
  "01654000",  "01634000", "02016000", "02039500", "02042500", "02051500",
  "02059500", "02056000","02054530", "02056650", "02056900"
)
# Notes:
# - "02075500" Dan River at Paces VA is too influenced by 50 cfs flowby from Smith River to use
g_list = g_roanoke
odf <- data.frame(
  gage_id = character(),
  gage_name = character(),
  norain_90 = numeric(),
  proj_date = character(),
  proj_emerg = character(),
  record_low = character(),
  C = numeric(),
  c_method = character()
)

for (gage_id in glist) {
  
  hydrocode = paste0('usgs_ws_', gage_id)
  omgage <- hydrotools::WaterGageDaily$new(ds_in = ds, gage_id = gage_id)
  omgage$load_wshd_feat()
  omgage$get_gage_data_old(start_date = '1900-01-01', end_date='2026-06-28', approval_status = 'all')
  omgage$plot_low_flows()
  omgage$low_flows
  # Load model object for retrieving BPJ AGWRC
  # look for l90_agwrc property, use it if it exists
  model <- ModelElementBase$new(
    ds, 
    config = list(
      hydrocode=hydrocode, bundle="watershed", version="AGWRC-1.0")
  )
  # todo: maybe we *should* store it on the model since this IS simple_lm method
  #simple_lm = model$prop$get_prop('simple_lm')
  #l90_agwrc = simple_lm$get_prop('l90_agwrc')'
  # get bounds on relationship if set
  l90_agwrc = model$prop$get_prop('l90_agwrc')
  agwrc_reg_qlow = model$prop$get_prop('agwrc_reg_qlow')
  agwrc_reg_clow = model$prop$get_prop('agwrc_reg_clow')
  # to set these do:
  # agwrc_reg_qlow$propvalue = 39.5
  # agwrc_reg_qlow$varid = l90_agwrc$get_vardef(config = list(varkey='om_class_Constant'))$hydroid
  # agwrc_reg_qlow$save(TRUE)
  # agwrc_reg_clow$propvalue = 0.986
  # agwrc_reg_clow$varid = l90_agwrc$get_vardef(config = list(varkey='om_class_Constant'))$hydroid
  # agwrc_reg_clow$save(TRUE)
  
  # inspect for start date
  plot(
    Flow ~ Date, 
    data=omgage$gage_data[omgage$gage_data$Date >= "2026-05-15",],
    main=paste("Observed", model$feature$name)
  )
  days = nrow(omgage$gage_data)
  last30 = omgage$gage_data[(days - 30):days,]
  Q0 = min(last30$Flow)
  start_date = max(last30[last30$Flow == Q0,]$Date)
  points(start_date, Q0, col="red", bg="red", pch = 21, cex = 2)
  # load the gage regression info from the server
  #es = agws::analyze_recession(eventurl)
  #regurl = paste0(basedir, "baseflow_regression_df_", omgage$gage_id, ".csv")
  #reg = read.csv(regurl)
  if (is.na(l90_agwrc$pid)) {
    #Ce = agws::RegressionAGWRC(Flow = Q0, m = reg$m[1], b = reg$b[1])
    Ce = NA
    method = 'regression'
  } else {
    Ce = l90_agwrc$propvalue
    method = 'manual'
  }
  # now check for a minimum valid Q to regress against
  if (!is.na(agwrc_reg_qlow$propvalue) && Q0 < agwrc_reg_qlow$propvalue) {
    Ce = agwrc_reg_clow$propvalue
    method = 'regression_limit'
  }
  # commented in favor of gage object baseflow_forecast() method below
  #fc = agws::forwardForecast(Q0, AGWRC = Ce)
  #lm_var <- omgage$baseflow_forecast(start_date,AGWRC = "lm_variable")
  if (!is.na(Ce)) {
    if (method != 'regression_limit') {
      AGWRC = list(
        "lm_constant" = "lm_constant",
        "lm_variable" = "lm_variable",
        "BPJ" = Ce
      )
    } else {
      AGWRC = list(
        "lm_constant" = "lm_constant",
        "lm_variable" = "lm_variable",
        "Reg Limit" = Ce
      )
    }
  } else {
    AGWRC = list(
      "lm_constant" = "lm_constant",
      "lm_variable" = "lm_variable"
    )
  }
  bff <- omgage$plot_baseflow_forecast(
    start_date = start_date,
    return_plotly = FALSE,
    AGWRC = AGWRC
  )
  print(bff) # display ggplot
  if (!is.na(Ce)) {
    if (method != 'regression_limit') {
      # use a manually defined constant C
      fc = bff[["plot_env"]][["all_forecasts"]][["BPJ"]]
    } else {
      fc = bff[["plot_env"]][["all_forecasts"]][["Reg Limit"]]
    }
  } else {
    # use the algorithmic regression C
    fc = bff[["plot_env"]][["all_forecasts"]][["lm_variable"]]
  }
  
  fc$Date <- as.Date(start_date + fc$Day)
  Q90 = fc[90,]$Forecast
  end_date <- fc[90,]$Date
  is_emerg = 'No' 
  is_hist = 'No'
  if (Q90 <= min(omgage$low_flows$n1Q10_annDate$minFlow)) {
    is_hist = 'Yes' # look up from percentile tables
  }
  ntab <- omgage$nep_table()
  emo <- month(end_date)
  emo_em <- ntab[emo,3]
  if (Q90 <= emo_em) {
    is_emerg = 'Yes'
  }
  plot(
    fc$Forecast ~ fc$Date,
    main=paste("Projected", model$feature$name)
  )
  Ce = median(fc$AGWRC)
  odl <- data.frame (
    age_id = gage_id,
    gage_name = model$feature$name,
    norain_90 = Q90,
    proj_date = end_date,
    proj_emerg = is_emerg,
    record_low = is_hist,
    C = Ce,
    c_method = method
  )
  odf = rbind(
    odf,
    odl
  )
}


# add USGS prior to event for context
# add dates, not just day number

# other functions:
# agws::fit_agwrc_regression(events)

