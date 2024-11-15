library("sqldf")
library("data.table")
pwater <- read.csv('http://deq1.bse.vt.edu:81/p6/out/land/hsp2_2022/pwater/forN51165_pwater.csv')
#pwater <- read.csv('/media/model/p6/out/land/hsp2_2022/pwater/forN51165_pwater.csv')
pwater_daily <- sqldf("select year, month, day, avg(LZS) as LZS, avg(AGWS) as AGWS from pwater group by year, month, day order by year, month , day")
regm <- lm(AGWS ~ LZS, data=pwater_daily )
summary(regm)
plot(AGWS ~ LZS, data=pwater_daily )

rodf <- data.frame(
  'model_version' = c('cbp-6.1'),
  'runid' = c('subsheds'),
  'metric' = c('l90_RUnit'),
  'runlabel' = c('l90_RUnit')
)
# ftype options,
# sova: cbp532_lrseg
# others: cbp6_lrseg
ro_data <- om_vahydro_metric_grid(
  metric = metric, runids = rodf, bundle = "landunit", ftype = "cbp6_landseg",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

# RO too small, check for missing lrseg: JU2_7140_7330, JU2_7450_7360
# - in these, a single Landseg was missing, from WV: N54063 
jar_rodata = fn_extract_basin(ro_data,'OR7_8490_0000')

sqldf("select * from jar_rodata where abs((Runit_2 - Runit_0) / Runit_0) > 0.05")


