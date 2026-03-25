# Download coefficient data
coef <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_data/data/QC_coefficients.csv")

# Download event data
event_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_bf_csvs/bf_events_01634000.csv")

#model_params <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/data/P620171001WQf_model_params.csv")
#colnames(model_params) <- model_params[1,]
#model_params <- model_params[-1,]
#colnames(model_params)[1] <- "LANDSEG"

# Get weighted AGWRCS
w_AGWRC <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/data/weighted_AGWRC.csv")

# Add column for calculated flow 
event_data$calc_AGWRC_gage <- (coef$slope[5]*log(event_data$Flow)) +coef$intercept[5]
event_data$calc_AGWRC_model <- (coef$slope[6]*log(event_data$Flow)) +coef$intercept[6]


# Plot new flow vs agwrc plot
# box and whisker to compare observed to newly calcd
ggplot()+
  geom_boxplot(data = event_data,
               aes(x = "Calculated from Observed Flow", y = AGWR),
               fill = "firebrick2", color="firebrick3", alpha = 0.6)+
  geom_boxplot(data = event_data,
               aes(x = "Event Value After Trimming", y = AGWRC),
               fill = "dodgerblue2", color="dodgerblue3", alpha = 0.6)+
  geom_boxplot(data = event_data,
               aes(x = "Calculated using Model Coefs", y = calc_AGWRC_model),
               fill = "darkorange2", color = "darkorange3", alpha = 0.6)+
  geom_boxplot(data = event_data,
               aes(x = "Calculating using Gage Coefs", y = calc_AGWRC_gage),
               fill = "forestgreen", color = "forestgreen", alpha = 0.6)+
  theme_bw() +
  xlab("Variable") +
  ylab("AGWRC") +
  ggtitle("AGWRCs at different Stages in the Model/Process (Strasburg Data)")+
  coord_cartesian(ylim = c(0.85,1.05))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 7))




ggplot(data = event_data, mapping = aes(Flow, calc_AGWRC_model))+
  geom_point(aes(y = AGWRC), color = "firebrick2", alpha=0.5)+
  geom_point(alpha=0.5)+
  geom_abline(intercept = w_AGWRC$w_AGWRC[3], slope = 0, color="dodgerblue2")+
  theme_bw()+
 # coord_cartesian(ylim = c(0.88,1), xlim = c(0,2675))+
  xlab("Flow (cfs)")+
  ylab("AGWRC (model)")+
  ggtitle("Calculated AGWRCs using Model Coefficients (Strasburg)")

ggplot(data = event_data, mapping = aes(Flow, calc_AGWRC_gage))+
  geom_point()+
  geom_abline(intercept = w_AGWRC$w_AGWRC[3], slope = 0, color="dodgerblue2")+
  theme_bw()+
  coord_cartesian(ylim = c(0.925,1), xlim = c(0,2675))+
  xlab("Flow (cfs)")+
  ylab("AGWRC (gage)")+
  ggtitle("Calculated AGWRCs using Gage Coefficients (Strasburg)")


#sqldf("select eventid, median(flow), min(aGWRC)
