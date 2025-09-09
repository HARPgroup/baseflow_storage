make_model_daily <- function(data, datecol){
  require(sqldf)
  # Packages
  require(lubridate)
  require(sqldf)
  
  rates <- c("AGWET", "AGWI", "AGWLI", "AGWO", "BASET", "CEPE",
             "IFWI", "IFWLI", "IFWO", "IGWI", "INFIL", "LZET",
             "LZI", "LZLI", "PERC", "PERO", "PET", "SUPY", "SURI",
             "SURLI", "SURO", "TAET", "UZET", "UZI", "UZLI")
  
  lengths_etc <- c("AGWS", "CEPS", "GWVS", "IFWS", "INFFAC", 
                   "LZS", "PERS", "PETADJ", "SURS", "TGWS", "UZS",
                   "year", "month", "day")
  
  # create date column to be used
  if (datecol %in% colnames(data)){
    data$date <- as_date(data[[datecol]])
  } else {
    print("Could not find input date column")
  }
  
  # Apply different functions tot he columns to aggregate to daily
  print("Aggregating rates into daily values")
  rates_daily <- aggregate(data[rates], by = list(Date = data$date), FUN = sum)
  
  print("Aggregating lengths and constants into daily values")
  lengths_etc_daily <- aggregate(data[lengths_etc], by = list(Date = data$date), FUN = mean)
  
  # Combine rates and lengths into one dat frame
  print("Creating new daily data frame")
  data_daily <- sqldf(
    "select * from rates_daily as a
    inner join lengths_etc_daily as b 
    on (
      a.date = b.date
    )
    ")
  
  # Put back in alphabetical order with Date as first column
  data_daily <- data_daily[c("Date", sort(setdiff(names(data_daily), "Date")))]
  
  return(data_daily)
}
