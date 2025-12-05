df <- flow_csv

df <- add_model_data(df, "N51171", "for", "AGWI")

df <- add_model_data(df, "N51171", "for", "AGWET")



library(ggplot2)

# initial plot ideas ----

# AGWI vs. Flow Plots
ggplot(data = subset(df, RecessionDay==FALSE), mapping = aes(AGWI, Flow))+
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Recharge and Flow During Non-Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset(df, RecessionDay==TRUE), mapping = aes(AGWI, Flow))+
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Recharge and Flow During Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

#AGWRC vs. AGWET plot
df_clean <- df %>%
  drop_na(AGWR, AGWET)


ggplot(data = subset(df_clean, RecessionDay==FALSE), mapping = aes(AGWR, AGWET))+
  geom_point(size = 0.75, na.rm = TRUE)+
  theme_bw()+
  ggtitle("AGWRC and Evapotranspiration During Non-Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset(df_clean, RecessionDay==TRUE), mapping = aes(AGWR, AGWET))+
  geom_point(size = 0.75, na.rm=TRUE)+
  theme_bw()+
  ggtitle("AGWRC and Evapotranspiration During Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

# AGWI vs. AGWET
ggplot(data = subset(df, RecessionDay==FALSE), mapping = aes(AGWI, AGWET))+
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Recharge and AGWET During Non-Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset(df, RecessionDay==TRUE), mapping = aes(AGWI, AGWET))+
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Recharge and AGWET During Recession Days")+
  theme(plot.title = element_text(hjust = 0.5))

# box plots of AGWET
ggplot(df, aes(x = factor(RecessionDay), y = AGWET)) +
  geom_boxplot(na.rm = TRUE, fill = "lightblue", color = "lightblue4") +
  theme_bw() +
  ggtitle("AGWET Distribution by Recession Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Recession Day") +
  ylab("AGWET")

# box plots of AGWI
ggplot(df, aes(x = factor(RecessionDay), y = AGWI)) +
  geom_boxplot(na.rm = TRUE, fill = "orange1", color = "orange4") +
  theme_bw() +
  ggtitle("Recharge Distribution by Recession Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 0.2)+
  xlab("Recession Day") +
  ylab("AGWI")


# Summary table using means
summary_df <- df %>%
  group_by(RecessionDay) %>%
  summarise(
    mean_flow = mean(Flow, na.rm = TRUE),
    mean_AGWR = mean(AGWR, na.rm = TRUE),
    mean_AGWET = mean(AGWET, na.rm = TRUE),
    mean_AGWI = mean(AGWI, na.rm = TRUE)
  )


# Summary Table using medians
summary_df_median <- df %>%
  group_by(RecessionDay) %>%
  summarise(
    median_flow = median(Flow, na.rm = TRUE),
    median_AGWR = median(AGWR, na.rm = TRUE),
    median_AGWET = median(AGWET, na.rm = TRUE),
    median_AGWI = median(AGWI, na.rm = TRUE)
  )


# AGET and AGWO/Flow Plots ----
# Currently, only using event data

ggplot(data = analysis_df, mapping = aes(x = AGWET, y = AGWO))+
  geom_point(size = 0.75)+
  scale_x_continuous(limits = c(0, max(analysis_df$AGWO, na.rm = T)))+
  scale_y_continuous(limits = c(0, max(analysis_df$AGWO, na.rm = T)))+
  theme_bw()+
  ggtitle("AGWET vs. AGWO Magnitude (Zoomed to Same Scale)")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = analysis_df, mapping = aes(x = AGWET, y = AGWO))+
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("AGWET vs. AGWO Magnitude")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = analysis_df, mapping = aes(x = AGWET, y = Flow))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("AGWET vs. Flow")+
  theme(plot.title = element_text(hjust = 0.5))

analysis_df$c_AGWO <- analysis_df$AGWI - analysis_df$AGWET
analysis_df$ratio_OET <- analysis_df$AGWO/analysis_df$AGWET
analysis_df$ratio_OET[is.infinite(analysis_df$ratio_OET)] <- NA


ggplot(data = analysis_df, mapping = aes(x = c_AGWO, y = AGWO))+ 
  geom_point(size = 0.75)+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("AGWI - AGWET")+
  ggtitle("AGWI - AGWET vs. AGWO")+
  theme(plot.title = element_text(hjust = 0.5))

c_AGWO_lm <-lm(analysis_df$AGWO ~ analysis_df$c_AGWO)
summary(c_AGWO_lm)

ratio_summary <- summary(analysis_df$ratio_OET, na.rm=T)

ggplot(data = analysis_df, mapping = aes(y = ratio_OET))+ 
  geom_boxplot()+
  theme_bw()+
  ggtitle("AGWI - AGWET vs. AGWO")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0,100))

ggplot(analysis_df, aes(x = c_AGWO, y = AGWO)) +
  geom_point(size=0.75) +
  theme_bw()+
  xlab("AGWI - AGWET")+
  geom_abline(slope = 4, intercept = 0.01, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggtitle("Visual Trend vs. Linear Model")+
  theme(plot.title = element_text(hjust = 0.5))






# Converting Flow to watershed inches ----
site <- readNWISsite("01633000")
da_sqmi <- site$drain_area_va

conversion <- (86400*12*12*12)/(5280*5280*12*12)
sp_conv <- conversion/da_sqmi

analysis_df$Flow_in <- analysis_df$Flow * sp_conv

ymax <- max(c(max(analysis_df$AGWO), max(analysis_df$Flow_in)), na.rm = T)
xmax <- max(c(max(analysis_df$AGWO), max(analysis_df$Flow_in)), na.rm = T)


ggplot(data = analysis_df, mapping = aes(x = AGWET, y = Flow_in))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("AGWET vs. Flow (in inches)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))

ggplot(data = analysis_df, mapping = aes(x = AGWET, y = AGWO))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("AGWET vs. AGWO")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))



# AGWET vs. AGWS
ggplot(data = analysis_df, mapping = aes(x = AGWET, y = AGWS))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("AGWET vs. AGWS")+
  theme(plot.title = element_text(hjust = 0.5))


# Calculating AGWS from Flow (AGWO) and AGWRC ----
site <- readNWISsite("01633000")
da_sqmi <- site$drain_area_va

conversion <- (86400*12*12*12)/(5280*5280*12*12)
sp_conv <- conversion/da_sqmi

analysis_df$Flow_in <- analysis_df$Flow * sp_conv

analysis_df$calc_AGWS <- (analysis_df$Flow_in)/(1-(analysis_df$calc_AGWR))

ggplot(data = analysis_df, mapping = aes(x = AGWS, y = calc_AGWS))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Model AGWS vs. AGWS Calculated from Flow")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,3)

ggplot(data = analysis_df, mapping = aes(x = AGWO, y = Flow_in))+ 
  geom_point(size = 0.75)+
  theme_bw()+
  ggtitle("Model AGWO vs. Flow in Watershed Inches")+
  theme(plot.title = element_text(hjust = 0.5))


# Quantifying agwet effect on low and high storage days
#calc. agws
analysis_df$calc_AGWS <- (analysis_df$AGWO)/(1-(analysis_df$calc_AGWR))

AGWS_df <- analysis_df[, c("Date", "AGWET", "AGWS")]
AGWS_df$AGWS_model <- analysis_df$calc_AGWS


analysis_df$calc_AGWS <- (analysis_df$Flow_in)/(1-(analysis_df$calc_AGWR))

AGWS_df$AGWS_flow <- analysis_df$calc_AGWS

AGWS_filtered <- AGWS_df[!is.na(AGWS_df$AGWS_model),]

long_AGWS <- sqldf("
  select Date, AGWET, AGWS as AGWS_orig, AGWS_flow as AGWS_calc, 'flow' as source from AGWS_filtered
  union all
  select Date, AGWET, AGWS as AGWS_orig, AGWS_model as AGWS_calc, 'model' as source from AGWS_filtered
")

ggplot(long_AGWS, aes(x = AGWS_orig, y = AGWS_calc, color = source)) +
  geom_point(alpha = 0.4) +
  labs(x = "AGWS (in)", y = "AGWS (in)", color = "Source") +
  theme_bw()+
  scale_color_manual(values = c("dodgerblue2","orange"))+
  coord_cartesian(ylim = c(-1,5))+
  ggtitle("AGWS vs. Two Inputs for Calculating AGWS")+
  theme(plot.title = element_text(hjust = 0.5))
  

# Looking at individual events ----
id_num <- 99

ex <- sqldf(sprintf(
  "select * from analysis_df where GroupID = %f"
, id_num))

ex$AGWS_model <- (ex$AGWO)/(1-(ex$calc_AGWR))
ex$AGWS_flow <- (ex$Flow_in)/(1-(ex$calc_AGWR))

ex_long <- sqldf("
  select Date, AGWET, AGWS as AGWS_orig, AGWS_flow as AGWS_calc, 'flow' as source from ex
  union all
  select Date, AGWET, AGWS as AGWS_orig, AGWS_model as AGWS_calc, 'model' as source from ex
")

ggplot(ex_long, aes(x = AGWS_orig, y = AGWS_calc, color = source)) +
  geom_point(alpha = 0.75) +
  labs(x = "Model AGWS (in)", y = "Calculated AGWS (in)", color = "Source") +
  theme_bw()+
  scale_color_manual(values = c("dodgerblue2","orange"))+
  ggtitle(paste0("Event ", id_num))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ex, aes(x=Date, y= AGWO))+
  geom_line(color = "dodgerblue4")+
  theme_bw()+
  ylab("AGWO (in)")+
  ggtitle(paste0("Event ", id_num))+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim = c(0,0.028))

scale_factor <- 1

ggplot(ex, aes(x = Date)) +
  geom_line(aes(y = Flow_in, color = "Flow")) +
  geom_line(aes(y = AGWS_flow * scale_factor, color = "AGWS")) +
  scale_y_continuous(name = "Flow (in)",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Model AGWS (in)")) +
  scale_color_manual(name = "Variable",
                     values = c("Flow" = "blue", "AGWS" = "red")) +
  labs(x = "Date",
       y = "Flow",
       title = paste0("Flow and AGWS over Time: Event ", id_num)) +
  theme_bw()

  


# Calculate the 50th percentile (median) of storage
median_AGWS <- quantile(analysis_df$AGWS, 0.5, na.rm = TRUE)

# Filter rows where storage is less than or equal to the median
low_storage_data <- analysis_df[analysis_df$AGWS <= median_AGWS, ]

high_storage_data <- analysis_df[analysis_df$AGWS >= median_AGWS, ]

ggplot(low_storage_data, mapping = aes(AGWET, AGWS))+
  geom_point()

ggplot(high_storage_data, mapping = aes(AGWET, AGWS))+
  geom_point()


# Using Ben's Trimmed data
mj_trimmed <- read.csv("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/ben_trimming/MJ_trimmed_analysis.csv")

land_type_code <- "forN51171"

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWI")

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWET")

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWO")

mj_trimmed <- add_model_data(mj_trimmed, land_type_code, "AGWS")

site <- readNWISsite("01633000")
da_sqmi <- site$drain_area_va

conversion <- (86400*12*12*12)/(5280*5280*12*12)
sp_conv <- conversion/da_sqmi

id_num <- 101

ex <- sqldf(sprintf(
  "select * from mj_trimmed where GroupID = %f"
  , id_num))

# For all events
ex <- sqldf(
  "select * from mj_trimmed "
  )

ex$AGWS_model <- (ex$AGWO)/(1-(ex$trimmed_calc_AGWR))
ex$AGWS_flow <- (ex$Flow_in)/(1-(ex$trimmed_calc_AGWR))

ex_long <- sqldf("
  select Date, AGWET, AGWS as AGWS_orig, AGWS_flow as AGWS_calc, 'flow' as source from ex
  union all
  select Date, AGWET, AGWS as AGWS_orig, AGWS_model as AGWS_calc, 'model' as source from ex
")

ggplot(ex_long, aes(x = AGWS_orig, y = AGWS_calc, color = source)) +
  geom_point(alpha = 0.75) +
  labs(x = "Model AGWS (in)", y = "Calculated AGWS (in)", color = "Source") +
  theme_bw()+
  scale_color_manual(values = c("dodgerblue2","orange"))+
  ggtitle(paste0("AGWS Calculated from Two Sources (Trimmed Data)"))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0,1)


scale_factor <- 1

ex$Date <- as.Date(ex$Date)


ggplot(ex, aes(x = Date)) +
  geom_line(aes(y = Flow_in, color = "Flow", group = 1)) +
  geom_line(aes(y = AGWS_flow * scale_factor, color = "AGWS", group = 1)) +
  scale_y_continuous(name = "Flow (in)",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Model AGWS (in)")) +
  scale_color_manual(name = "Variable",
                     values = c("Flow" = "blue", "AGWS" = "red")) +
  labs(x = "Date",
       y = "Flow",
       title = paste0("Flow and AGWS over Time: Event ", id_num)) +
  theme_bw()


