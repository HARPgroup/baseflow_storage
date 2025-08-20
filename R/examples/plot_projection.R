# project flow timeseries
n1 = 240
n = n1 # initialize then later adjust
start_date <- as.Date('2002-01-26')
recharge_date <- as.Date('2002-01-27')
end_date <- as.Date(start_date) + 50
gage_no = "01634000"
C = 0.993
gC = 0.985 # calculated from observation of drought flows 2-point method

usgs_daily <- dataRetrieval::readNWISdv(gage_no, parameterCd = '00060', startDate = start_date, endDate = end_date) 
if (nrow(usgs_daily) < n1) {
  n = nrow(usgs_daily) - 1
}
lDay <- nrow(usgs_daily)
# Choose custom, lowest flow day, what?
#bfd_start <- start_date 
bfd_start <- usgs_daily$Date[nrow(usgs_daily)]
Q0 = usgs_daily[1,]$X_00060_00003
Ql = usgs_daily[lDay,]$X_00060_00003
Qr = usgs_daily[rDay,]$X_00060_00003
all_projC <- Q0*(bfd_agwrc_project(1, C, 1:(n1+1)))
all_projgC <- Q0*(bfd_agwrc_project(1, gC, 1:(n1+1)))
# Now assemble projection df
all_proj <- as.data.frame(
  cbind(
    all_projC,
    all_projgC
  )
)
names(all_proj) <- c('agwrc_proj_C', 'agwrc_proj_gC')
all_proj$Date <- c(start_date + 0:n1+1)

# project from last day using best estimate of C
lDay <- which(all_proj$Date == max(usgs_daily$Date))
all_projlC <- Ql*(bfd_agwrc_project(1, C, 1:((n1-lDay)+1)))
# Create a spot for USGS observed and set the data
all_proj$X_00060_00003 <- NA
all_proj[1:nrow(usgs_daily),]$X_00060_00003 <- usgs_daily[1:nrow(usgs_daily),'X_00060_00003']
# Create a spot for the projection from last obsered day and set
all_proj$agwrc_proj_lC <- NA
all_proj[lDay:(n1),]$agwrc_proj_lC <- all_projlC
# Create and set spot for recharge projection date
rDay <- which(all_proj$Date == recharge_date)
# from end of "standard" recharge period 
all_projrC <- Qr*(bfd_agwrc_project(1, C, 1:((n1-rDay)+1)))
all_proj$agwrc_proj_rC <- NA
all_proj[rDay:(n1),]$agwrc_proj_rC <- all_projrC 

plot(
  all_proj$X_00060_00003 ~ all_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(all_proj$X_00060_00003)),
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(all_proj$agwrc_proj_C ~ all_proj$Date, col='red')
#points(all_proj$agwrc_proj_gC ~ all_proj$Date, col='green')
points(all_proj$agwrc_proj_rC ~ all_proj$Date, col='blue')
points(all_proj$agwrc_proj_lC ~ all_proj$Date, col='purple')
axis(1, all_proj$Date, format(all_proj$Date, "%b %d %Y"), cex.axis = .7)
legend(
  all_proj$Date[floor(n1 * 0.65)], 0.98*(max(all_proj$X_00060_00003)),
  c('Min AGWRC', 'Event Estimated'), 
  pch = c(1,1), lty = c(FALSE, FALSE),
  col=c("red", "green")
)
text(
  all_proj$Date[floor(n1 * 0.75)],
  0.1*(max(all_proj$X_00060_00003)),
  paste('Min. Q @', end_date, '=', round(all_proj[n1,"agwrc_proj_lC"]))
)

# Log version
plot(
  log(all_proj$X_00060_00003) ~ all_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(log(all_proj$X_00060_00003))),
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(all_proj$agwrc_proj_C ~ all_proj$Date, col='red')
#points(all_proj$agwrc_proj_gC ~ all_proj$Date, col='green')
points(log(all_proj$agwrc_proj_rC) ~ all_proj$Date, col='blue')
points(log(all_proj$agwrc_proj_lC) ~ all_proj$Date, col='purple')
axis(1, all_proj$Date, format(all_proj$Date, "%b %d %Y"), cex.axis = .7)
legend(
  all_proj$Date[floor(n1 * 0.65)], 0.98*(max(all_proj$X_00060_00003)),
  c('Min AGWRC', 'Event Estimated'), 
  pch = c(1,1), lty = c(FALSE, FALSE),
  col=c("red", "green")
)
text(
  all_proj$Date[floor(n1 * 0.75)],
  0.1*(max(all_proj$X_00060_00003)),
  paste('Min. Q @', end_date, '=', round(all_proj[n1,"agwrc_proj_lC"]))
)


#if (n)
qlast <- usgs_daily
q30last <- bfd_agwrc_project(1, C, 1:(n+1))