bfd_cfq_seasonal <- function(analysis_data){
  require(ggplot2)
  require(gridExtra)
  source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/summarize_event.R")
  data <- summarize_event(analysis_data)
  mean_event_flow <- aggregate(Flow~GroupID, data = analysis_data, FUN = mean, na.rm=TRUE)
  analysis_summary <- aggregate(Season ~ GroupID, data = analysis_data, FUN = function(x) tail(x, 1))
  
  data_combined <- sqldf("
  select a.*, b.Flow, c.Season
  from data as a
  inner join mean_event_flow as b
  on a.i = b.GroupID
  left outer join analysis_summary as c
  on a.i = c.GroupID
")
  
  spring <- ggplot(data = subset(data_combined, Season == "Spring"), mapping = aes(x = Flow))+
    geom_point(mapping = aes(y = AGWR))+
    geom_smooth(mapping = aes(y = AGWR), method = "lm")+
    theme_bw()+
    coord_cartesian(xlim = c(min(data_combined$Flow), max(data_combined$Flow)),
                    ylim = c(min(data_combined$AGWR), max(data_combined$AGWR)))+
    xlab("Mean Event Flow (cfs)")+
    ylab("Estimated AGWR")+
    ggtitle("Spring Events")+
    theme(plot.title = element_text(hjust = 0.5))
  
  summer <- ggplot(data = subset(data_combined, Season == "Summer"), mapping = aes(x = Flow))+
    geom_point(mapping = aes(y = AGWR))+
    geom_smooth(mapping = aes(y = AGWR), method = "lm")+
    theme_bw()+
    coord_cartesian(xlim = c(min(data_combined$Flow), max(data_combined$Flow)),
                    ylim = c(min(data_combined$AGWR), max(data_combined$AGWR)))+
    xlab("Mean Event Flow (cfs)")+
    ylab("Estimated AGWR")+
    ggtitle("Summer Events")+
    theme(plot.title = element_text(hjust = 0.5))
  
  fall <- ggplot(data = subset(data_combined, Season == "Fall"), mapping = aes(x = Flow))+
    geom_point(mapping = aes(y = AGWR))+
    geom_smooth(mapping = aes(y = AGWR), method = "lm")+
    theme_bw()+
    coord_cartesian(xlim = c(min(data_combined$Flow), max(data_combined$Flow)),
                    ylim = c(min(data_combined$AGWR), max(data_combined$AGWR)))+
    xlab("Mean Event Flow (cfs)")+
    ylab("Estimated AGWR")+
    ggtitle("Fall Events")+
    theme(plot.title = element_text(hjust = 0.5))
  
  winter <- ggplot(data = subset(data_combined, Season == "Winter"), mapping = aes(x = Flow))+
    geom_point(mapping = aes(y = AGWR))+
    geom_smooth(mapping = aes(y = AGWR), method = "lm")+
    theme_bw()+
    coord_cartesian(xlim = c(min(data_combined$Flow), max(data_combined$Flow)),
                    ylim = c(min(data_combined$AGWR), max(data_combined$AGWR)))+
    xlab("Mean Event Flow (cfs)")+
    ylab("Estimated AGWR")+
    ggtitle("Winter Events")+
    theme(plot.title = element_text(hjust = 0.5))
  
  result <- grid.arrange(spring, summer, fall, winter, top = paste0(analysis_data$site_no[1], " Seasonal Events"))
  
  return(result)
  
}
