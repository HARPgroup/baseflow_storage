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



