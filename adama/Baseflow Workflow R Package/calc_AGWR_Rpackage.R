#'@title calc_AGWR
#'@name
#'calc_AGWR
#'@description
#'Calculation of AGWR
#'@details
#'AGWR = Qt / Qt-1
#'Calculates active groundwater regression by dividing each flow 
#'value by the previous flow value in the series 
#'
#'@param x An input num vector with chronological flow values
#'@return An output num vector with AGWR in all positions except 1
#'@author ?
#'@refrences ?
#'@example
#'#get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000") 
#'StrasGage$gage_data$AGWR <- calc_AGWR(StrasGage$gage_data$value)
#'@export
calc_AGWR <- function ( 
    #An input vector with chronological flow values
    x) {
    #First position of vector calculations records NA value
    c(NA,
    #All x vector values except the first position
    x[-1] 
    #Division operator
    / 
    #All x vector values except the last position  
    x[-length(x)])
}

