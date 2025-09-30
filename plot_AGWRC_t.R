plot_AGWRC_t <- function(event_data_path){
  # load required packages
  require(ggplot2)
  require(ggpmisc)
  
  # Load in data
  event_df <- read.csv(event_data_path)
  event_df$Date <- as.Date(event_df$Date)
  
  # create plot with R^2 value
  result <- ggplot(data = event_df, mapping = aes(x = Date, y = calc_AGWR))+
    geom_point()+
    geom_smooth(method = "lm")+
    stat_poly_eq()+
    theme_bw()+
    ylab("Calculated AGWRC")+
    ggtitle("Event AGWRC Over Time")+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(result)
  
}

# plot_AGWRC_t("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/mount_jackson_event_dataset.csv")
