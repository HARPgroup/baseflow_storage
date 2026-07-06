# low flow stuff
library("hydrotools")
library("agws")
basedir = "http://deq1.bse.vt.edu:81/usgs/agws/"

gage_id = "01634000"
glist = c(
  "03524000", "02051500", "02039500", "03167000", "01674500", "01667500",
  "01654000", "02059500", "02056000", "01634000", "02016000", "02042500"
)
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
  omgage <- hydrotools::WaterGageDaily$new(gage_id = gage_id)
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
  #l90_agwrc = simple_lm$get_prop('l90_agwrc')
  l90_agwrc = model$prop$get_prop('l90_agwrc')
  # inspect for start date
  plot(
    Flow ~ Date, 
    data=omgage$gage_data[omgage$gage_data$Date >= "2026-05-15",],
    main=paste("Observed", model$feature$name)
  )
  # baseflow IDed at 6/22
  # Q0 = omgage$gage_data[omgage$gage_data$Date == "2026-05-20",]$Flow
  Q0 = omgage$gage_data[omgage$gage_data$Date == "2026-06-22",]$Flow
  # pick the lowet flows in the last 3 days
  days = nrow(omgage$gage_data)
  last30 = omgage$gage_data[(days - 30):days,]
  Q0 = min(last30$Flow)
  start_date = max(last30[last30$Flow == Q0,]$Date)
  points(start_date, Q0, col="red", bg="red", pch = 21, cex = 2)
  # load the gage regression info from the server
  #es = agws::analyze_recession(eventurl)
  regurl = paste0(basedir, "baseflow_regression_df_", omgage$gage_id, ".csv")
  reg = read.csv(regurl)
  if (is.na(l90_agwrc$pid)) {
    Ce = agws::RegressionAGWRC(Flow = Q0, m = reg$m[1], b = reg$b[1])
    method = 'auto'
  } else {
    Ce = l90_agwrc$propvalue
    method = 'manual'
  }
  fc = agws::forwardForecast(Q0, AGWRC = Ce)
  Q90 = fc[90,]$Forecast
  is_emerg = 'yes' # hard coded for now, need to query tables\
  is_hist = 'No'
  if (Q90 <= min(omgage$low_flows$n1Q10_annDate$minFlow)) {
    is_hist = 'yes' # had codeed query tables
  }
  fc$Date <- as.Date(start_date + fc$Day)
  plot(
    fc$Forecast ~ fc$Date,
    main=paste("Observed", model$feature$name)
  )
  odf = rbind(
    odf,
    data.frame(gage_id, model$feature$name, Q90, fc[90,]$Date, is_emerg, is_hist, Ce, method)
  )
}


# add USGS prior to event for context
# add dates, not just day number

# other functions:
# agws::fit_agwrc_regression(events)

