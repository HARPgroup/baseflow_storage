# low flow stuff
library("hydrotools")
library("agws")
library("dataRetrieval")
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))

argst <- commandArgs(trailingOnly=T)
# Ex:
# argst = c("02065500,02059500,02056000,02054530,02056900,02058400,02071000,02061500,02064000", '/tmp/test.csv')
# argst = c("02065500,02059500,02056000,02054530,02056900", '/tmp/test.csv', "2002-07-10")
# argst = c("03524000,03167000,01674500,01667500,01654000,01634000,02016000,02039500,02042500,02051500,02059500,02056650", '/tmp/test.csv')

message(paste("length of argst = ", length(argst)))
if (length(argst) < 2) {
  message(paste("Use: deq_norain.R output_file gages( \"02065500,02059500,...\") [start_date] [end_date]"))
  q()
}
gages <- as.character(argst[1])
glist <- stringr::str_split(gages,",",simplify=TRUE)
# get or guess the date to aim for projection
save_path = argst[2]
if (length(argst) > 2) {
  proj_start_date = argst[3]
} else {
  proj_start_date = format(Sys.time(), "%Y-%m-%d")
}
if (length(argst) > 3) { 
  proj_end_date = argst[4]
} else {
  proj_end_date = format(as.Date(proj_start_date) + 90, "%Y-%m-%d")
}
yr = year(proj_start_date)

# Notes:
# - "02075500" Dan River at Paces VA is too influenced by 50 cfs flowby from Smith River to use
odf <- data.frame(
  hydroid = integer(),
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
  omgage$get_gage_data_old(start_date = '1900-01-01', end_date=proj_end_date, approval_status = 'all')
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
    data=omgage$gage_data[omgage$gage_data$Date >= (as.Date(proj_start_date) - 30),],
    main=paste("Observed", model$feature$name),
    ylim=c(0, max(omgage$gage_data[omgage$gage_data$Date >= (as.Date(proj_start_date) - 30),]$Flow))
  )
  days = nrow(omgage$gage_data)
  last30 = omgage$gage_data[(days - 30):days,]
  Q0 = min(last30$Flow)
  start_date = max(last30[last30$Flow == Q0,]$Date)
  points(start_date, Q0, col="red", bg="red", pch = 21, cex = 2)
  # load the gage regression info from the database
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
  fpath = paste0(save_directory, "/Q90_norain_log_", gage_id, yr, "_", ".png")
  ggplot2::ggsave(fpath)
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
  Qmin = min(omgage$low_flows$n1Q10_annDate$minFlow)
  if (Q90 <= Qmin) {
    is_hist = 'Yes' # look up from percentile tables
  }
  ntab <- omgage$nep_table()
  emo <- month(end_date)
  emo_em <- ntab[emo,3]
  if (Q90 <= emo_em) {
    is_emerg = 'Yes'
  }
  yscale = max(fc$Forecast,na.rm=TRUE)
  yinc = yscale / 10
  plot(
    fc$Forecast ~ fc$Date,
    main=paste("Projected", model$feature$name)
  )
  text(as.Date(end_date - 10), Q90 + yinc * 2, paste("Q90 =", round(Q90,1), "cfs"))
  text(as.Date(end_date - 10), Q90 + yinc * 3, paste("Qmin =", round(Qmin,1), "cfs"))
  fpath = paste0(save_directory, "/Q90_norain_", gage_id, yr, "_", ".png")
  png(fpath)
  # now save the same thing
  plot(
    fc$Forecast ~ fc$Date,
    main=paste("Projected", model$feature$name)
  )
  text(as.Date(end_date - 10), Q90 + yinc * 2, paste("Q90 =", round(Q90,1), "cfs"))
  text(as.Date(end_date - 10), Q90 + yinc * 3, paste("Qmin =", round(Qmin,1), "cfs"))
  dev.off()
    Ce = median(fc$AGWRC)
  odl <- data.frame (
    hydroid = omgage$gage_feature$hydroid,
    gage_id = gage_id,
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
write.csv(odf,file=save_path)
