#'@title calc_AGWR
#'@name
#'calc_AGWR
#'@description
#'Calculation of AGWR / Ratio of Iterative timeseries values
#'@details
#'Active groundwater recession coefficient is AGWR = Qt / Qt-1 This function
#'divides the the value in the time series by the value in the previous
#'day/index of the series
#'
#'@param x Num vector with chronological values
#'@return Num vector with AGWR in all indices except 1
#'@examples
#'#Get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000")
#'StrasGage$gage_data$AGWR <- calc_AGWR(StrasGage$gage_data$value)
#'@export
#'
calc_AGWR <- function (x) {
  #First element of vector calculations records NA value e.g.
  #All x vector values except the first position
  #Division operator
  #All x vector values except the last position
  out <- c(NA, x[-1] / x[-length(x)])
  return(out)
}

#'@title calc_delta_AGWR
#'@name
#'calc_delta_AGWR
#'@description
#'Wrapper of calc_AGWR to comply with legacy code
#'@details
#'Calls calc_AGWRC(x)
#'@param x Num vector with chronological AGWR values
#'@return Num vector with change in AGWR in all positions except 1 and 2
#'@examples
#'#get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000")
#'StrasGage$gage_data$AGWR <- calc_AGWR(StrasGage$gage_data$value)
#'StrasGage$gage_data$deltaAGWR <- calc_delta_AGWR(StrasGage$gage_data$AGWR)
#'@export
calc_delta_AGWR <- function(x) {
  out <- calc_AGWR(x)
  return(out)
}



