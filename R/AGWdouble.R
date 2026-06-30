#'@title agwo_hspf
#'@name agwo_hspf
#' @details Calculate active groundwater outflow using HSPF methodologies
#' @details Calculate active groundwater outflow using HSPF formula of AGWO = S
#'   * (1 - C^timestep). This is an alternate of \code{agws::single_forecast()}
#'   designed to help test against HSPF output
#' @param S numeric of length 1 representing AGWS - active ground water storage
#'   in inches
#' @param C numeric of length 1 representing the AGWRC - Active groundwater
#'   recession coefficient
#' @param dthr numeric of length 1 representing the model timestep in hours
#' @return A numeric of length 1 that is this
agwo_hspf <- function(S, C, dthr) {
  kgwV = 1.0 - C^(dthr/24.0)
  agwo = kgwV * S
  return(agwo)
}

AGWdouble <- R6::R6Class(
  public = list(
    tmethod="all",
    agws1 = 0.0,
    agws2 = 0.0,
    agwo1 = 0.0,
    agwo2 = 0.0,
    agwin1 = 0.0,
    agwin2 = 0.0,
    agwo = 0.0,
    agws = 0.0,
    c1 = 0.99,
    c2 = 0.99,
    ce = 0.99,
    agwsmax1 = 1.0,
    agwsmax2 = 1.0,
    log = NA,
    dthr = 24.0,
    initialize = function(
        agws1=0.0, agws2=0.0, c1=0.99, c2=0.99,
        agwsmax1=1.0, agwsmax2=1.0, tmethod="all"
      ) {
      self$agws1 = agws1
      self$agws2=agws2
      self$c1=c1
      self$c2=c2
      self$agwsmax1=agwsmax1
      self$agwsmax2=agwsmax2
      self$tmethod=tmethod
      self$agws = self$agws1 + self$agws2
      log = data.frame(
        timestamp = integer(),
        agws1 = numeric(),
        agws2 = numeric(),
        agws = numeric(),
        agwin1 = numeric(),
        agwin2 = numeric(),
        agwo1 = numeric(),
        agwo2 = numeric(),
        agwo = numeric(),
        c1 = numeric(),
        c2 = numeric(),
        ce = numeric()
      )
    },
    solve_double_C = function(agws1, agws2, c1, c2, agwsmax1, agwsmax2, agwin1=0.0, tmethod="all", dthr=24) {
      # calculate agwo1 = flow out of agws1 (top layer of GW)
      # calculate agwin2 = amount of agwo1 that goes into agws2 from agws1
      # - this cannot exceed storage available in agws2
      # - should not exceed transmissivity rate of agws2
      # - *but* we will assume that the agws1 will take all it can from agwo1
      # calculate agwo2 = amount of flow out of agwo2
      # we will ignore agwsmax1 for now since HSPF does so
      agwo1 = agwo_hspf(agws1, c1, dthr)
      # then calculate outflow from bottom layer
      agwo2 = agwo_hspf(agws2, c2, dthr)
      # now computer transfers from top layer to bottom
      agwin2 = 0.0
      # how much free space in agws2?
      free2 = (agwsmax2 - agws2)
      # this method assumes lower layer can take all that upper gives
      if (tmethod == "all") {
        tmax2 = free2
      } else {
        # compute on head/transmissivity?
        tmax2 = agwsmax2 * (1.0 - c2)
      }
      # cannot allow more in than available storage
      if (tmax2 > free2) {
        tmax2 = free2
      }
      if (agwo1 > tmax2) {
        agwin2 = tmax2
      } else {
        agwin2 = agwo1
      }
      # since we consider agwo as that which goes from GW to the stream
      # we subtract the transfer to the bottom GW layer
      agwo1 = agwo1 - agwin2
      # record total outflow from GW to stream
      agwo = agwo1 + agwo2
      # calculate an effective C to compare to other methods
      # TODO: need to accomodate varying timesteps a la 1.0 - C^(dthr/24.0)
      ce = 1.0 - (agwo / (agws1 + agws2))
      # finish by updating storage at end of timestep
      agws1 = agws1 - (agwo1 + agwin2) # remove total from agws1
      agws2 = agws2 + agwin2 - agwo2
      return(
        list(
          agwo=agwo, agws1=agws1, agws2=agws2, agwo2=agwo2,
          agwo1=agwo1, agwin1=agwin1, agwin2=agwin2, ce=ce
        )
      )
    },
    eval = function() {
      outlist = self$solve_double_C(
        agws1=self$agws1, agws2=self$agws2, c1=self$c1, c2=self$c2, agwsmax1=self$agwsmax1,
        agwsmax2=self$agwsmax2, agwin1=self$agwin1, dthr=self$dthr, tmethod=self$tmethod
      )
      self$agws1 = outlist$agws1
      self$agws2 = outlist$agws2
      self$agws = self$agws1 + self$agws2
      self$agwo1 = outlist$agwo1
      self$agwo2 = outlist$agwo2
      self$agwo = outlist$agwo
      self$agwin1 = outlist$agwin1
      self$agwin2 = outlist$agwin2
      self$ce = outlist$ce
    },
    show_state = function(format="markdown") {
      output = as.data.frame(
        list(
          AGWS1 = self$agws1,
          AGWS2 = self$agws2,
          AGWIN1 = self$agwin1,
          AGWO1 = self$agwo1,
          AGWIN2 = self$agwin2,
          AGWO2 = self$agwo2,
          AGWO = self$agwo
        )
      )
      if (format != "dataframe") {
        output = kableExtra::kable(
          output,
          format=format
        )
      }
      return(output)
    },
    log_state = function(ts) {
      self$log = rbind(
        self$log,
        data.frame(
          timestamp = ts,
          agws1 = self$agws1,
          agws2 = self$agws2,
          agws = self$agws,
          agwin1 = self$agwin1,
          agwin2 = self$agwin2,
          agwo1 = self$agwo1,
          agwo2 = self$agwo2,
          agwo = self$agwo,
          c1 = self$c1,
          c2 = self$c2,
          ce = self$ce
        )
      )
    },
    plot = function(type="cfq", warmup=0, ylim=TRUE) {
      wts = self$log[(warmup + 1):nrow(self$log),]
      if (type == "cfq") {
        if (is.logical(ylim)) {
          ylim=c(min(0.9, min(wts$ce)), 1.0)
        }
        plot(
          (wts$agwo1 + wts$agwo2),
          wts$ce,
          xlab='Total Outflow (in/day)',
          ylab='Effective Recession Coefficient(AGWRC)',
          ylim=ylim
        )
      }
      if (type == "cfs") {
        if (is.logical(ylim)) {
          ylim=c(min(0.9, min(wts$ce)), 1.0)
        }
        plot(
          (wts$agws1 + wts$agws2),
          wts$ce,
          xlab='Total Storage (in)',
          ylab='Effective Recession Coefficient(AGWRC)',
          ylim=ylim
        )
      }
      if (type == "qfs") {
        if (is.logical(ylim)) {
          ylim=c(0, max(wts$agwo))
        }
        plot(
          wts$agws,
          wts$agwo,
          xlab='Total Storage (in)',
          ylab='Outflow (in/day)',
          ylim=ylim
        )
      }
      if (type == "storage") {
        plot(
          (wts$agws),
          ylab='Total Storage (in)',
          xlab='Timestep',col="black",
          ylim=c(0,max((wts$agws1 + wts$agws2), na.rm = TRUE))
        )
        points(
          (wts$agws1),col="blue"
        )
        points(
          (wts$agws2),col="green"
        )
        points(
          (wts$agwo),col="orange"
        )
        legend(
          x="right",y="top",
          fill=c("black", "blue", "green","orange"),
          legend=c("Total (in)", "Upper Layer", "Lower Layer", "Flow")
        )
      }
    }
  )
)

