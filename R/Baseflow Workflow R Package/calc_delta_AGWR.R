#'@title calc_delta_AGWR
#'@name
#'calc_delta_AGWR
#'@description
#'Calculation of change in AGWR
#'@details
#'delta_AGWR = AGWR_t / AGWR_t-1
#'Calculates change in active groundwater regression by dividing each AGWR 
#'value by the previous AGWR value in the series 
#'
#'@param x Num vector with chronological AGWR values
#'@return Num vector with change in AGWR in all positions except 1 and 2
#'@author 
#'@example
#'#get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000") 
#'StrasGage$gage_data$AGWR <- calc_AGWR(StrasGage$gage_data$value)
#'StrasGage$gage_data$deltaAGWR <- calc_delta_AGWR(StrasGage$gage_data$AGWR)
#'@export
calc_delta_AGWR <- function(
    #Num vector with chronological AGWR values
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

