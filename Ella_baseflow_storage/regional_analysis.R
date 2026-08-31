#This script provides a regional analysis of baseflow events

library(patchwork)
library(dplyr)
library(ggplot2)

##NF analysis
#Load in NF dataframes
strasburg <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01634000.csv")
mt_jackson <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01633000.csv")
cootes_store <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01632000.csv")
lynnwood <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01628500.csv")
luray <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01629500.csv")
front_royal <- read.csv("https://deq1.bse.vt.edu//usgs//agws//baseflow_summary_df_01631000.csv")

stras_flow <- read.csv("https://deq1.bse.vt.edu//usgs//agws//01634000.csv")

#Normalize flow by DA
strasburg <- strasburg |>
  mutate(norm_flow = median_flow / 770)
mt_jackson <- mt_jackson |>
  mutate(norm_flow = median_flow / 355)
cootes_store <- cootes_store |>
  mutate(norm_flow = median_flow / 210)
lynnwood <- lynnwood |>
  mutate(norm_flow = median_flow / 1079)
luray <- luray |>
  mutate(norm_flow = median_flow / 1372)
front_royal <- front_royal |>
  mutate(norm_flow = median_flow / 1634)

#Combine all 3 NF datasets
strasburg$Location <- "Strasburg"
mt_jackson$Location <- "Mt_Jackson"
cootes_store$Location <- "Cootes_Store"
NF <- rbind(strasburg, mt_jackson, cootes_store)

#Combine all 3 SF datasets
lynnwood$Location <- "Lynnwood"
luray$Location <- "Luray"
front_royal$Location <- "Front_Royal"
SF <- rbind(lynnwood, luray, front_royal)

#Combine SF and NF datasets
Shen <- rbind(strasburg, mt_jackson, cootes_store, lynnwood, luray, front_royal)

#Plot NF data
model_NF <- lm(event_AGWRC ~ log(median_flow), data = NF)
summary(model_NF)

model_NF_norm <- lm(event_AGWRC ~ log(norm_flow), data = NF)
summary(model_NF_norm)

#AGWRC vs Flow (not normalized)
ggplot(NF, aes(x = log(median_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Characteristic event flow (median, cfs)",
       y = "Event AGWRC",
       title = "AGWRC vs Flow for NF Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

#AGWRC vs Flow (normalized)
ggplot(NF, aes(x = log(norm_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Normalized median flow (cfs per sqr mi)",
       y = "Event AGWRC",
       title = "AGWRC vs Normalized Flow for NF Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

#Plot SF data
model_SF <- lm(event_AGWRC ~ log(median_flow), data = SF)
summary(model_SF)

model_SF_norm <- lm(event_AGWRC ~ log(norm_flow), data = SF)
summary(model_SF_norm)

ggplot(SF, aes(x = log(median_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Characteristic event flow (median, cfs)",
       y = "Event AGWRC",
       title = "AGWRC vs Median Flow for SF Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

ggplot(SF, aes(x = log(norm_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Normalized median flow (cfs per sqr mi)",
       y = "Event AGWRC",
       title = "AGWRC vs Normalized Flow for SF Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

#Plot both NF and SF
model_Shen <- lm(event_AGWRC ~ log(median_flow), data = Shen)
summary(model_Shen)

model_Shen_norm <- lm(event_AGWRC ~ log(norm_flow), data = Shen)
summary(model_Shen_norm)

ggplot(Shen, aes(x = log(median_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Characteristic event flow (median, cfs)",
       y = "Event AGWRC",
       title = "AGWRC vs Median Flow for Total Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

ggplot(Shen, aes(x = log(norm_flow), y = event_AGWRC, color = Location)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Normalized median flow (cfs per sqr mi)",
       y = "Event AGWRC",
       title = "AGWRC vs Normalized Flow for Total Shenandoah Region") +
  geom_smooth(aes(group = 1),
              method = "lm")

#AGWRC vs Flow (normalized) for Strasburg
model_strasburg_norm <- lm(event_AGWRC ~ norm_flow, data = strasburg)
summary(model_strasburg_norm)

strasburg1 <- ggplot(strasburg, aes(x = norm_flow, y = event_AGWRC)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Normalized median flow (cfs per sqr mi)",
       y = "Event AGWRC",
       title = "AGWRC vs Normalized Flow for Strasburg (01634000)") +
  geom_smooth(method = "lm")

strasburg2 <- ggplot(strasburg, aes(x = median_flow, y = event_AGWRC)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Characteristic event flow (median, cfs)",
       y = "Event AGWRC",
       title = "AGWRC vs Median Flow for Strasburg (01634000)") +
  geom_smooth(method = "lm")

strasburg1 | strasburg2

#AGWRC vs Flow (normalized) for Mt Jackson
model_mtjackson_norm <- lm(event_AGWRC ~ norm_flow, data = mt_jackson)
summary(model_mtjackson_norm)

ggplot(mt_jackson, aes(x = norm_flow, y = event_AGWRC)
) +
  geom_point() +
  theme_classic() +
  labs(x = "Normalized median flow (cfs per sqr mi)",
       y = "Event AGWRC",
       title = "AGWRC vs Normalized Flow for Mt Jackson (01633000)") +
  geom_smooth(method = "lm")

