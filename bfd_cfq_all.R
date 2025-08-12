# This function creates a scatter plot of event flow and estimated AGWR
bfd_cfq_all <- function(analysis_data){
  source("https://github.com/HARPgroup/baseflow_storage/blob/main/summarize_event.R")
  data <- summarize_event(analysis_data)
  mean_event_flow <- aggregate(Flow~GroupID, data = analysis_data, FUN = mean, na.rm=TRUE)
  analysis_summary <- aggregate(Season ~ GroupID, data = analysis_data, FUN = function(x) tail(x, 1))
  
  data_combined <- sqldf("
  SELECT a.*, b.Flow, c.Season
  FROM data AS a
  INNER JOIN mean_event_flow AS b
    ON a.i = b.GroupID
  LEFT OUTER JOIN analysis_summary AS c
    ON a.i = c.GroupID
")
  
 result <- ggplot(data = data_combined, mapping = aes(x = Flow))+
    geom_point(mapping = aes(y = AGWR))+
    geom_smooth(mapping = aes(y = AGWR), method = "lm")+
    theme_bw()+
    xlab("Mean Event Flow (cfs)")+
    ylab("Estimated AGWR")+
    ggtitle("All Events")+
    theme(plot.title = element_text(hjust = 0.5))
  
 return(result)
 
}
