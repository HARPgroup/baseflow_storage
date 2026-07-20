#'@title add_month_season
#'@name
#'add_month_season
#'@description
#'Add month and meteorologic season to a data.frame
#'@details
#'Add numerical month column in data.frame based on Date column, then add char
#'meteorologic season column when month number matches individual season
#'parameters
#'@param df data.frame input with column named "Date" in format "YYYY-mm-dd" as
#'  Date obj
#'@param date_col Character. The field in df containing dates.
#'@return data.frame now appended with columns Month and Season contianing num
#'  month and char season for length of "Date" column
#'@examples
#'\dontrun{
#'#'#get data from N F Shenandoah River via Strasburg, VA - USGS 01634000
#'library(hydrotools)
#'StrasGage <- hydrotools::WaterGageDaily$new(gage_id = "01634000")
#'StrasGage$gage_data <- StrasGage$gage_data  |>
#'  dplyr::rename(Date = time)
#'month_season_data <- add_month_season(StrasGage$gage_data)
#'}
#'@importFrom rlang .data
#'@export
add_month_season <- function(
    #df input with column named "Date" in format "YYYY-mm-dd" as Date obj
    df, date_col = "Date") {
    #Mutate df, adding Month and Season column
    out <- df |> dplyr::mutate(
    #New column: Month = num "%m" from Date column
      Month = format(!!dplyr::sym(date_col), "%m"),
      #New column: Season = char output when num month matches
      Season = dplyr::case_when(
        Month %in% c("12", "01", "02") ~ "Winter",
        Month %in% c("03", "04", "05") ~ "Spring",
        Month %in% c("06", "07", "08") ~ "Summer",
        Month %in% c("09", "10", "11") ~ "Fall",
        #Return NA for non-matches
        TRUE ~ NA_character_
      )
    )
    return(out)
}

