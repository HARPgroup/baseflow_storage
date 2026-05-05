# project flow timeseries
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/master/R/bfd_agwrc_project.R")

n1 = 240
n = n1 # initialize then later adjust
start_date <- as.Date('2025-05-11')
recharge_date <- as.Date('2025-05-11')
l_detail_num <- 90 # how many days before end of record to show projection in detail plot
#end_date <- as.Date(recharge_date) + 50
end_date <- as.Date('2025-12-31')
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
rDay <- which(usgs_daily$Date == recharge_date)
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
all_projlgC <- Ql*(bfd_agwrc_project(1, gC, 1:((n1-lDay)+1)))
# Create a spot for USGS observed and set the data
all_proj$X_00060_00003 <- NA
all_proj[1:nrow(usgs_daily),]$X_00060_00003 <- usgs_daily[1:nrow(usgs_daily),'X_00060_00003']
# Create a spot for the projection from last obsered day and set
all_proj$agwrc_proj_lC <- NA
all_proj$agwrc_proj_lgC <- NA
all_proj[lDay:(n1),]$agwrc_proj_lC <- all_projlC
all_proj[lDay:(n1),]$agwrc_proj_lgC <- all_projlgC
# Create and set spot for recharge projection date
# from end of "specified" recharge period 
all_projrC <- Qr*(bfd_agwrc_project(1, C, 1:((n1-rDay)+1)))
all_proj$agwrc_proj_rC <- NA
all_proj[rDay:(n1),]$agwrc_proj_rC <- all_projrC 

plot(
  all_proj$X_00060_00003 ~ all_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(all_proj$X_00060_00003, na.rm = TRUE)),
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(all_proj$agwrc_proj_C ~ all_proj$Date, col='red')
#points(all_proj$agwrc_proj_gC ~ all_proj$Date, col='green')
points(all_proj$agwrc_proj_rC ~ all_proj$Date, col='blue')
points(all_proj$agwrc_proj_lC ~ all_proj$Date, col='purple')
points(log(all_proj$agwrc_proj_lgC) ~ all_proj$Date, col='green')
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
  paste('Q @', end_date, '=', round(all_proj[n1,"agwrc_proj_lC"]))
)

# Log version
plot(
  log(all_proj$X_00060_00003) ~ all_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(log(all_proj$X_00060_00003), na.rm = TRUE)),
  ylab = all_proj$X_00060_00003,
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(all_proj$agwrc_proj_C ~ all_proj$Date, col='red')
#points(all_proj$agwrc_proj_gC ~ all_proj$Date, col='green')
points(log(all_proj$agwrc_proj_rC) ~ all_proj$Date, col='blue')
points(log(all_proj$agwrc_proj_lC) ~ all_proj$Date, col='purple')
points(log(all_proj$agwrc_proj_lgC) ~ all_proj$Date, col='green')
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
  paste('Q @', end_date, '=', round(all_proj[n1,"agwrc_proj_lgC"]), 'to', round(all_proj[n1,"agwrc_proj_lC"]))
)

# Plot from specified recharge date
rday_proj <- all_proj[rDay:nrow(all_proj),]
plot(
  rday_proj$X_00060_00003 ~ rday_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(rday_proj$X_00060_00003, na.rm = TRUE)),
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(rday_proj$agwrc_proj_C ~ rday_proj$Date, col='red')
#points(rday_proj$agwrc_proj_gC ~ rday_proj$Date, col='green')
points(rday_proj$agwrc_proj_rC ~ rday_proj$Date, col='blue')
points(rday_proj$agwrc_proj_lC ~ rday_proj$Date, col='purple')
points(rday_proj$agwrc_proj_lgC ~ rday_proj$Date, col='green')
axis(1, rday_proj$Date, format(rday_proj$Date, "%b %d %Y"), cex.axis = .7)
legend(
  rday_proj$Date[floor(n1 * 0.65)], 0.98*(max(rday_proj$X_00060_00003)),
  c('Min AGWRC', 'Event Estimated'), 
  pch = c(1,1), lty = c(FALSE, FALSE),
  col=c("red", "green")
)
text(
  rday_proj$Date[floor(n1 * 0.75)],
  0.1*(max(rday_proj$X_00060_00003)),
  paste('Q @', end_date, '=', round(rday_proj[n1,"agwrc_proj_lgC"]), 'to', round(rday_proj[n1,"agwrc_proj_lC"]))
)


# Plot from near end of period
lday_proj <- all_proj[ (lDay - l_detail_num):nrow(all_proj),]
plot(
  lday_proj$X_00060_00003 ~ lday_proj$Date, 
  xaxt = "n", 
  type = "l",
  ylim=c(0,max(5.0*lday_proj$agwrc_proj_lC[lDay:nrow(lday_proj)], na.rm = TRUE)),
  xlim=c(start_date, end_date),
  main=(
    paste("Observed and projected baseflows for", gage_no, 
          "\n",start_date,"to", end_date))
)
#points(lday_proj$agwrc_proj_C ~ lday_proj$Date, col='red')
#points(lday_proj$agwrc_proj_gC ~ lday_proj$Date, col='green')
points(lday_proj$agwrc_proj_lgC ~ lday_proj$Date, col='blue')
points(lday_proj$agwrc_proj_lC ~ lday_proj$Date, col='purple')
points(lday_proj$agwrc_proj_lgC ~ lday_proj$Date, col='green')
axis(1, lday_proj$Date, format(lday_proj$Date, "%b %d %Y"), cex.axis = .7)
legend(
  lday_proj$Date[floor(n1 * 0.65)], 0.98*(max(lday_proj$X_00060_00003)),
  c('Min AGWRC', 'Event Estimated'), 
  pch = c(1,1), lty = c(FALSE, FALSE),
  col=c("red", "green")
)
text(
  lday_proj$Date[floor(n1 * 0.75)],
  0.1*(max(lday_proj$X_00060_00003)),
  paste('Q @', end_date, '=', round(lday_proj[n1,"agwrc_proj_lgC"]), 'to', round(lday_proj[n1,"agwrc_proj_lC"]))
)



#if (n)
qlast <- usgs_daily
q30last <- bfd_agwrc_project(1, C, 1:(n+1))