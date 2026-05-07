# Plots from model.R workflow

# Plots for all event data----
# plot all storage vs agwrc and flow vs agwrc Check limits
ggplot(data = all_df, mapping = aes(Flow_in, AGWRC))+
  geom_point(color="dodgerblue2", size = 0.75)+
  theme_bw()+
  coord_cartesian(ylim = c(0.85, 1), xlim = c(0,0.08))+
  xlab("Flow (in)")+
  ylab("AGWRC")+
  ggtitle("All MJ Event Observed Flow and Event AGWRC")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = all_df, mapping = aes(V_flow, V_AGWRC))+
  geom_point(color="firebrick2", size = 0.75)+
  theme_bw()+
  coord_cartesian(ylim = c(0.85, 1), xlim = c(0,0.08))+
  xlab("Flow (in)")+
  ylab("AGWRC")+
  ggtitle("All MJ Calculated Event Flow and Variable AGWRC")+
  theme(plot.title = element_text(hjust = 0.5))

#obs(model) vs calc storage
ggplot(data = all_df, mapping = aes(, V_AGWRC))+
  geom_point(mapping = aes(x = AGWS,color="Model Storage"), size = 0.75, alpha = 0.15)+
  geom_point(mapping = aes(x = V_storage,color="Calculated Storage"), size = 0.75)+
  theme_bw()+
  scale_color_manual(name = "Legend", values = c("Model Storage" = "purple",
                                                 "Calculated Storage" = "darkorange")) +
  coord_cartesian(ylim = c(0.85, 1), xlim = c(0,0.8))+
  xlab("Storage (in)")+
  ylab("AGWRC")+
  ggtitle("All MJ Storage and Variable AGWRC")+
  theme(plot.title = element_text(hjust = 0.5))

# Obs vs calc flow
ggplot(data = all_df, mapping = aes(, V_AGWRC))+
  geom_point(mapping = aes(x = Flow_in,color="Obs Flow"), size = 0.75, alpha = 0.25)+
  geom_point(mapping = aes(x = V_flow,color="Calculated Flow"), size = 0.75, alpha = 0.25)+
  theme_bw()+
  scale_color_manual(name = "Legend", values = c("Obs Flow" = "dodgerblue2",
                                                 "Calculated Flow" = "firebrick2")) +
  coord_cartesian(ylim = c(0.85, 1), xlim = c(0,0.08))+
  xlab("Flow (in)")+
  ylab("AGWRC")+
  ggtitle("All MJ Flow and Variable AGWRC")+
  theme(plot.title = element_text(hjust = 0.5))

# model vs calc stoagre
ggplot(data=all_df, mapping = aes(AGWS, V_storage))+
  geom_point()+
  geom_abline(slope = 1, color = "firebrick2", size = 1)+
  coord_cartesian(ylim = c(0,0.7), xlim = c(0,0.7))+
  theme_bw()+
  xlab("Model AGWS (in)")+
  ylab("Storage calc using uniroot (in)")+
  ggtitle("Model vs. Calculated Storage at MJ")+
  theme(plot.title = element_text(hjust = 0.5))

# obs vs calc flow
ggplot(data=all_df, mapping = aes(Flow_in, V_flow))+
  geom_point()+
  geom_abline(slope = 1, color = "dodgerblue2", size = 1)+
  coord_cartesian(ylim = c(0,0.1), xlim = c(0,0.1))+
  theme_bw()+
  xlab("Obs Flow (in/d)")+
  ylab("Flow calc using uniroot (in/d)")+
  ggtitle("Obs vs. Calculated Flow at MJ")+
  theme(plot.title = element_text(hjust = 0.5))


# Plots for using sample event----
# scale fact for second axis
ex <- sqldf("select * from all_df where GroupID = 96")
S0 <- ex$AGWS[1]
scale_factor <- S0 + 0.01

# No flow on plot
ggplot(data = ex, aes(Date)) +
  geom_line(aes(y = AGWS, color = "Model Data Storage"), size=0.7) +
  geom_line(aes(y = U_storage, color = "Constant C Storage"), size = 0.7) +
  geom_line(aes(y = (AGWRC - 0.7)/0.3 * scale_factor, color = "Constant AGWRC"), linetype="dashed", size=0.7)+
  geom_line(aes(y = V_storage, color = "Variable C Storage"), size=0.7) +
  geom_line(aes(y = (V_AGWRC - 0.7) / 0.3 * scale_factor,color = "Variable AGWRC"), linetype="dashed", size = 0.7) +
  scale_color_manual(name = "Legend", values = c("Model Data Storage" = "purple3",
                                                 "Constant C Storage" = "lightseagreen",
                                                 "Constant AGWRC" = "lightseagreen",
                                                 "Variable C Storage" = "gold2",
                                                 "Variable AGWRC" = "gold2")) +
  scale_y_continuous(name = "Storage(in)",sec.axis = sec_axis(~ 0.7 + 0.3 * (. / scale_factor),name = "AGWRC")) +
  coord_cartesian(ylim = c(0, scale_factor)) +
  theme_bw() +
  ggtitle(paste0("Storage and AGWRC over time, MJ Event ", ex$GroupID[1])) +
  theme(plot.title = element_text(hjust = 0.5))



# With flow on plot
ggplot(data = ex, aes(Date)) +
  geom_line(aes(y = AGWS, color = "Model Data"), size=0.7) +
  geom_line(aes(y = U_storage, color = "Constant C Storage"), size = 0.7) +
  geom_line(aes(y = (AGWRC - 0.7)/0.3 * scale_factor, color = "Constant AGWRC"), linetype="dashed", size=0.7)+
  geom_line(aes(y = V_storage, color = "Variable C Storage"), size=0.7) +
  geom_line(aes(y = V_flow, color = "Variable C Flow"), size = 0.7) +
  geom_line(aes(y = (V_AGWRC - 0.7) / 0.3 * scale_factor,color = "Variable AGWRC"), linetype="dashed", size = 0.7) +
  scale_color_manual(name = "Legend", values = c("Model Data" = "purple3",
                                                 "Constant C Storage" = "lightseagreen",
                                                 "Constant AGWRC" = "lightseagreen",
                                                 "Variable C Storage" = "gold2",
                                                 "Variable AGWRC" = "gold2",
                                                 "Variable C Flow" = "forestgreen")) +
  scale_y_continuous(name = "Storage or Flow (in)",sec.axis = sec_axis(~ 0.7 + 0.3 * (. / scale_factor),
                                                                       name = "AGWRC")) +
  coord_cartesian(ylim = c(0, scale_factor)) +
  theme_bw() +
  ggtitle(paste0("Storage and AGWRC over time, MJ Event ", ex$GroupID[1])) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot with lookup table value
ggplot(data = ex, mapping = aes(Date))+
  geom_line(mapping = aes(,U_storage, color = "Constant AGWRC"))+
  geom_line(mapping = aes(,L_storage, color = "Lookup Table"))+
  geom_line(mapping = aes(,AGWS, color = "Model Data"))+
  scale_color_manual(name="Legend", values = c("Constant AGWRC" = "lightseagreen",
                                               "Lookup Table" = "firebrick2",
                                               "Model Data" = "purple3"))+
  theme_bw()+
  ylab("Storage (in)")+
  ggtitle("Storage Calculated by Different Methods (Cootes Store Event 96)")+
  theme(plot.title = element_text(hjust = 0.5))


# Lookup table values plot
ggplot(data = all_df, mapping = aes(AGWS, L_storage))+
  geom_point()+
  geom_abline(slope = 1, color = "firebrick2", linewidth = 1)+
  theme_bw()+
  coord_cartesian(xlim = c(0,1.25), ylim = c(0,1.25))+
  xlab("Model AGWS (in)")+
  ylab("Storage from lookup Table (in)")+
  ggtitle("Lookup Table Storage Compared to Model (SB)")+
  theme(plot.title = element_text(hjust = 0.5))



# Storage vs. AGWRC Plot
ggplot(data = ex, aes(V_storage, V_AGWRC))+
  geom_point(color="firebrick2")+
  theme_bw()+
  coord_cartesian(ylim = c(0.7,1))+
  xlab("Storage (in)")+
  ylab("AGWRC")+
  ggtitle(paste0("Storage and AGWRC Values for MJ Event ", ex$GroupID[1]))+
  theme(plot.title = element_text(hjust = 0.5))
# Arbitrary S Plots----

ggplot(data=test_df, aes(Storage, AGWRC))+
  geom_point(color="royalblue4", size = 0.75)+
  ggtitle("Storage Values vs. Calculated AGWRC")+
  xlab("Storage (in)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))




