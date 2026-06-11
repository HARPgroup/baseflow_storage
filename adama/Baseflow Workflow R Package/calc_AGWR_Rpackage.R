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
#'@param x Num vector with chronological flow values
#'@return Num vector with AGWR in all indices except 1
#'@author 
#'@example
#'#Get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000") 
#'StrasGage$gage_data$AGWR <- calc_AGWR(StrasGage$gage_data$value)
#'@export
calc_AGWR <- function ( 
    #Num vector with chronological flow values
    x) {
    #First element of vector calculations records NA value
    c(NA,
    #All x vector values except the first position
    x[-1] 
    #Division operator
    / 
    #All x vector values except the last position  
    x[-length(x)])
}

