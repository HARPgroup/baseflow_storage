# project flow timeseries
n = 75
start_date <- '2023-07-18'
end_date <- as.Date(start_date) + n
gage_no = "01634000"
C = 0.975
gC = 0.985 # calculated from observation of drought flows 2-point method

usgs_daily <- dataRetrieval::readNWISdv(gage_no, parameterCd = '00060', startDate = start_date, endDate = end_date) 

Q0 = usgs_daily[1,]$X_00060_00003

usgs_daily$agwrc_proj975 <- Q0*(bfd_agwrc_project(1, C, 1:(n+1)))
usgs_daily$agwrc_proj985 <- Q0*(bfd_agwrc_project(1, 0.985, 1:(n+1)))

plot(
  usgs_daily$X_00060_00003 ~ usgs_daily$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(usgs_daily$X_00060_00003)),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
points(usgs_daily$agwrc_proj975 ~ usgs_daily$Date, col='red')
points(usgs_daily$agwrc_proj985 ~ usgs_daily$Date, col='green')
axis(1, usgs_daily$Date, format(usgs_daily$Date, "%b %d %Y"), cex.axis = .7)
legend(
  usgs_daily$Date[2], 0.1*(max(usgs_daily$X_00060_00003)),
  c('Min AGWRC', 'Event Estimated'), 
  pch = c(1,1), lty = c(FALSE, FALSE),
  col=c("red", "green")
)

