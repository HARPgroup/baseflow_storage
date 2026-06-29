qs <- list("0.999 (693-day half-flow)" = 1.0*(bfd_agwrc_project(1, 0.999, 1:90)))
qs[["0.9924 (90-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.9924, 1:90))
qs[["0.985 (45-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.985, 1:90))
qs[["0.977 (30-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.977, 1:90))
qs[["0.965 (20-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.965, 1:90))
qs[["0.95 (14-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.95, 1:90))
qs[["0.90 (6-day)"]] <- 1.0*(bfd_agwrc_project(1, 0.90, 1:90))

pX_days <- function (C, X=0.5, maxdays=900) {
  c_ts <- 1.0*(C^(1:maxdays))
  x = round(median(which(round(c_ts,1) == X)))
  return(x)
}
pX_days(0.975)
pX_days(0.98, 0.5)
pX_days(0.9, 0.5)

n=0
for (i in names(qs)) {
  if (i != "0.999 (693-day half-flow)") {
    if (n == 0) {
      plot(
        100.0*qs[[i]], 
        ylim=c(0,100),
        xlab="Days without recharge",
        ylab="Percent of Starting Flow"
      )
    } else {
      points(100.0*qs[[i]])
    }
    p50_days = round(median(which(round(qs[[i]],1) == 0.5)))
    if ( is.na(p50_days) || (p50_days > 90) ) {
      p50_days = 85
    }
    trot = -90.0*0.9*(1.0-(p50_days / 90))
    text(
      p50_days, 
      100.0 * qs[[i]][[floor(p50_days)]],
      i,
      srt=trot
    )
    n=n+1
    message(trot)
  }
}
