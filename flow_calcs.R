# Download coefficient data
coef <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_data/data/QC_coefficients.csv")

# Download event data
event_data <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/bf_events_01634000.csv")

model_params <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ih_model_calcs/data/P620171001WQf_model_params.csv")
colnames(model_params) <- model_params[1,]
model_params <- model_params[-1,]
colnames(model_params)[1] <- "LANDSEG"

# Add column for calculated flow 
event_data$calc_AGWRC_gage <- (coef$slope[5]*log(event_data$Flow)) +coef$intercept[5]
event_data$calc_AGWRC_model <- (coef$slope[6]*log(event_data$Flow)) +coef$intercept[6]


# Plot new flow vs agwrc plot
ggplot(data = event_data, mapping = aes(Flow, calc_AGWRC_model))+
  geom_point()+
  theme_bw()+
  coord_cartesian(ylim = c(0.85,1.05), xlim = c(0,1500))+
  xlab("Flow (cfs)")+
  ylab("AGWRC (model)")+
  ggtitle("Calculated AGWRCs using Model Coefficients (Strasburg)")

ggplot(data = event_data, mapping = aes(Flow, calc_AGWRC_gage))+
  geom_point()+
  theme_bw()+
  coord_cartesian(ylim = c(0.85,1.05), xlim = c(0,1500))+
  xlab("Flow (cfs)")+
  ylab("AGWRC (gage)")+
  ggtitle("Calculated AGWRCs using Gage Coefficients (Strasburg)")
