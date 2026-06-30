# low flow stuff
basedir = "http://deq1.bse.vt.edu:81/usgs/agws/"
omgage <- hydrotools::WaterGageDaily$new(gage_id = "01634000")

omgage$get_gage_data_old(start_date = '1900-01-01', end_date='2026-06-28', approval_status = 'all')

omgage$low_flows
omgage$baseflow_forecast()

# inspect
plot(Flow ~ Date, data=omgage$gage_data[omgage$gage_data$Date >= "2026-05-22",])
# baseflow IDed at 6/22
Q0 = omgage$gage_data[omgage$gage_data$Date == "2026-05-20",]$Flow
Q0 = omgage$gage_data[omgage$gage_data$Date == "2026-06-22",]$Flow

# load the gage regression info from the server
es = agws::analyze_recession(eventurl)
regurl = paste0(basedir, "baseflow_regression_df_", omgage$gage_id, ".csv")
reg = read.csv(regurl)


Ce = agws::RegressionAGWRC(Flow = Q0, m = reg$m[1], b = reg$b[1])
agws::forwardForecast(Q0, AGWRC = Ce)

# other functions:
# agws::fit_agwrc_regression(events)

