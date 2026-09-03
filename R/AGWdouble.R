
#'@title agwo_hspf
#'@name agwo_hspf
#' @description Calculate active groundwater outflow using HSPF methodologies
#' @details Calculate active groundwater outflow using HSPF formula of AGWO = S
#'   * (1 - C^timestep). This is an alternate of \code{agws::single_forecast()}
#'   designed to help test against HSPF output
#' @param S numeric of length 1 representing AGWS - active ground water storage
#'   in inches
#' @param C numeric of length 1 representing the AGWRC - Active groundwater
#'   recession coefficient
#' @param dthr numeric of length 1 representing the model timestep in hours
#' @return A numeric of length 1 that is this timestep's AGWO
agwo_hspf <- function(S, C, dthr) {
  kgwV = 1.0 - C^(dthr/24.0)
  agwo = kgwV * S
  return(agwo)
}

#' AGWdouble
#' @description Object for calculating flows from a 2-compartment AGWS/hsp style surficial aquifer
#' @details This object creates two storage compartments with user defined
#'   initial vlaues and maximums. Provides all variables and equations to
#'   perform simulation and keep track of mass balance as water flows out of or
#'   between compartments based on user defined decay coefficients
#' @importFrom R6 R6Class
#' @param agws1 Numeric, Initial Storage in AGW compartment 1
#' @param agws2 Numeric, Initial Storage in AGW compartment 1
#' @param c1 Numeric, default of 0.99. The constant AGWRC (recession decay
#'   coefficient) in compartment 1 across all timesteps
#' @param c2 Numeric, default of 0.99. The constant AGWRC (recession decay
#'   coefficient) in compartment 2 across all timesteps
#' @param agwsmax1 Numeric, default 1 (inch). Maximum storage compartment 1
#' @param agwsmax2 Numeric, default 1 (inch). Maximum storage compartment 2
#' @param tmethod Character, either "all" or "trans" representing the transfer
#'   method from compartment 1 to 2. "all" indicates all remaining water from
#'   the upper compartment may flow to the lower, but "trans" indicates that the
#'   rate of flow is related to the decay coefficient of the bin
#' @return R6 object of class AGWdouble.
#' @examples \dontrun{
#' agwhilo = AGWdouble$new(
#'    agws1=5, agwsmax1 = 5,
#'    agws2=2.0, agwsmax2 = 1.0,
#'    c1=0.95, c2=0.98, tmethod = "tmax"
#')
#'agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="tmax", dthr=24)$agwin2
#'agwhilo$solve_double_C(2.5, 0.5, 0.95, 0.99, 5, 2.5, agwin1=0.0, tmethod="all", dthr=24)$agwin2
#'agw2$eval()
#'agw2$show_state()
#'}
#'@export AGWdouble
AGWdouble <- R6::R6Class(
  public = list(
    #' @field agws1 Numeric, Initial Storage in AGW compartment 1
    #' @field agws2 Numeric, Initial Storage in AGW compartment 1
    #' @field c1 Numeric, default of 0.99. The constant AGWRC (recession decay
    #'   coefficient) in compartment 1 across all timesteps
    #' @field c2 Numeric, default of 0.99. The constant AGWRC (recession decay
    #'   coefficient) in compartment 2 across all timesteps
    #' @field ce Numeric, default 0.99. The effective AGWRC of the entire system
    #'   as represented by $1.0 - (agwo / (agws1 + agws2))$
    #' @field agwsmax1 Numeric, default 1 (inch). Maximum storage compartment 1
    #' @field agwsmax2 Numeric, default 1 (inch). Maximum storage compartment 2
    #' @field tmethod Character, either "all" or "trans" representing the transfer
    #'   method from compartment 1 to 2. "all" indicates all remaining water from
    #'   the upper compartment may flow to the lower, but "trans" indicates that the
    #'   rate of flow is related to the decay coefficient of the bin
    #' @field agwo1 Numeric, defaults to 0 inches. Outflow from compartment 1.
    #' @field agwo2 Numeric, defaults to 0 inches. Outflow from compartment 2.
    #' @field agwin1 Numeric, defaults to 0 inches. Inflow from compartment 1.
    #' @field agwin2 Numeric, defaults to 0 inches. Inflow from compartment 2.
    #' @field agwo Numeric, total outflow from all compartments.
    #' @field agws Numeric, total storage across all compartments
    #' @field log data.frame, the current state of all fields
    #' @field dthr Numeric, default to 24.0 (hours). The timestep of the model.
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
    #' @details Initialize an AGWdouble object by providing inital model state
    #'   variables. These will be set to the relevant fields.
    #' @param agws1 Numeric, Initial Storage in AGW compartment 1
    #' @param agws2 Numeric, Initial Storage in AGW compartment 1
    #' @param c1 Numeric, default of 0.99. The AGWRC (recession decay coefficient) in compartment 1
    #' @param c2 Numeric, default of 0.99. The AGWRC (recession decay coefficient) in compartment 2
    #' @param agwsmax1 Numeric, default 1 (inch). Maximum storage compartment 1
    #' @param agwsmax2 Numeric, default 1 (inch). Maximum storage compartment 2
    #' @param tmethod Character, either "all" or "trans" representing the transfer
    #'   method from compartment 1 to 2. "all" indicates all remaining water from
    #'   the upper compartment may flow to the lower, but "trans" indicates that the
    #'   rate of flow is related to the decay coefficient of the bin
    #' @return Nothing, but all fields are now set on the object
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
      self$log = data.frame(
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
    #' @details Initialize an AGWdouble object by providing inital model state
    #'   variables. These will be set to the relevant fields.
    #' @param agws1 Numeric, Initial Storage in AGW compartment 1
    #' @param agws2 Numeric, Initial Storage in AGW compartment 2
    #' @param c1 Numeric. The constant AGWRC (recession decay coefficient) in
    #'   compartment 1 across all timesteps
    #' @param c2 Numeric. The constant AGWRC (recession decay coefficient) in
    #'   compartment 2 across all timesteps
    #' @param agwsmax1 Numeric. Constant maximum storage compartment 1 across
    #'   all timesteps.
    #' @param agwsmax2 Numeric. Constant maximum storage compartment 2 across
    #'   all timesteps.
    #' @param agwin1 Numeric, default 0 inches. Inflow to the first compartment
    #'   for this timestep.
    #' @param dthr Numeric, default 24 (hours). Model timestep.
    #' @param tmethod Character, either "all" or "trans" representing the transfer
    #'   method from compartment 1 to 2. "all" indicates all remaining water from
    #'   the upper compartment may flow to the lower, but "trans" indicates that the
    #'   rate of flow is related to the decay coefficient of the bin
    #' @return A data frame with all relevant state variables to include:
    #' agwo (total outflow), agws1 (storage in compartment 1), agws2 (storage in
    #' compartment 2), agwo2 (outflow from compartment 2),
    #' agwo1 (outflow from compartment 1), agwin1  (inflow to compartment 1),
    #' agwin2 (inflow to compartment 2), ce (effective AGWRC across compartments)
    solve_double_C = function(agws1, agws2, c1, c2, agwsmax1, agwsmax2,
                              agwin1=0.0, tmethod="all", dthr=24) {
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
    #'@description Compute double compartment groundwater step
    #'@details Use \code{self$solve_double_C()} to compute storage and outflow
    #'  from each bin using the data set on the object. This method will set
    #'  agws, agws1, agws2, agwo, agwo1, agwo2, agwin1, agwin2, ce
    #'@return Nothing, but sets all variables on object with updated values
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
    #'@description Show all relevant model data
    #'@details output a data frame with
    #'  agws1, agws2, agwo, agwo1, agwo2, agwin1, agwin2, ce  or a
    #'  \code{kableExtra::kable()} by specifying format as "dataframe" or some
    #'  format to pass to \code{kableExtra::kable()}
    #'@param format Character, default "data.frame". Either "data.frame" or a
    #'  character vector to pass to \code{kableExtra::kable()}
    #'@return Either a data frame of model parameters or a kable to display the
    #'  data
    show_state = function(format = "data.frame") {
      output <- data.frame(
        AGWS1 = self$agws1,
        AGWS2 = self$agws2,
        AGWIN1 = self$agwin1,
        AGWO1 = self$agwo1,
        AGWIN2 = self$agwin2,
        AGWO2 = self$agwo2,
        AGWO = self$agwo
      )
      if (format != "data.frame") {
        output <- kableExtra::kable(
          output,
          format = format
        )
      }
      return(output)
    },
    #'@description Add current model parameters to log
    #'@param ts The timestep that represents the current model parameters
    #'@return Nothing, but updates the log field on this object
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
    #'@description Plot
    #'@param type Character, default cfq. Creates a base R plot based on user input:
    #' * "cfq" plot effective Ce vs total outflow.
    #' * "cfs" plot effective ce vs total storage.
    #' * "cft" plot effective Ce vs timestep.
    #' * "qfs" plot total outflow vs total storage.
    #' * "storage" plot total storage, storage in each compartment, an
    #'  outflow vs timestep.
    #'@param warmup Numeric, default 0. The number of timesteps to ignore a the
    #'  start of \code{self$log}
    #'@param ylim logical, default TRUE. Should a custom y-axis limiter be used
    #'  that is between 0.9/minimu Ce and 1.0?
    #'@return nothing, but writes r plot to device
    plot = function(type="cfq", warmup=0, ylim=TRUE) {
      wts <- self$log[(warmup + 1):nrow(self$log),]
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
      if (type == "cft") {
        if (is.logical(ylim)) {
          ylim=c(min(0.9, min(wts$ce)), 1.0)
        }
        plot(
          wts$ce,
          xlab='Timestep',
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

