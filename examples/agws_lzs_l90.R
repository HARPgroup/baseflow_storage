library("sqldf")
library("data.table")
pwater <- read.csv('http://deq1.bse.vt.edu:81/p6/out/land/hsp2_2022/pwater/forN51165_pwater.csv')
#pwater <- read.csv('/media/model/p6/out/land/hsp2_2022/pwater/forN51165_pwater.csv')
pwater_daily <- sqldf("select year, month, day, avg(LZS) as LZS, avg(AGWS) as AGWS from pwater group by year, month, day order by year, month , day")
regm <- lm(AGWS ~ LZS, data=pwater_daily )
summary(regm)
plot(AGWS ~ LZS, data=pwater_daily )

rodf <- data.frame(
  'model_version' = c('cbp-6.0'),
  'runid' = c('CFBASE30Y20180615_vadeq'),
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


param_file = "http://deq1.bse.vt.edu:81/p6/vadeq/input/param/for/P620171001WQf/PWATER.csv"
landseg = "N51135"
table_name = "PWAT-PARM2"
param_name = "LZSN"

param_file_raw <- read.csv(param_file, header=FALSE)
param_tables <- param_file_raw[1,]
param_vars <- param_file_raw[2,]
param_cols <- names(param_vars[which(param_vars == param_name)])
target_table <- (param_file_raw[param_file_raw$V1 == landseg,])[,names(param_vars[,param_tables == table_name])]
if (param_name == "") {
  return_vals <- target_table
} else {
  return_vals <- target_table[,param_cols]
}

print(as.numeric(return_vals))

all_seg_params <- param_file_raw[3:nrow(param_file_raw),c("V1",param_cols)]
names(all_seg_params) <- c("landseg", param_name)
all_seg_params$LZSN <- as.numeric(all_seg_params$LZSN)

l90_seg_lzsn <- sqldf(
  "
    select a.*, b.l90_RUnit 
    from ro_data as b
    left outer join all_seg_params as a
    on (
      a.landseg = b.hydrocode
    )
    where b.l90_RUnit is not null
  "
)
l90_seg_lzsn_nz <- sqldf(
  "
    select a.*, b.l90_RUnit 
    from ro_data as b
    left outer join all_seg_params as a
    on (
      a.landseg = b.hydrocode
    )
    where b.l90_RUnit > 0
  "
)
l90_seg_lzsn_lt10<- sqldf(
  "
    select a.*, b.l90_RUnit 
    from ro_data as b
    left outer join all_seg_params as a
    on (
      a.landseg = b.hydrocode
    )
    where a.LZSN < 10.0
  "
)
l90lm <- lm(l90_RUnit ~ LZSN, data=l90_seg_lzsn)
plot(l90_RUnit ~ LZSN, data=l90_seg_lzsn)
abline(l90lm, col = "red")
summary(l90lm)


l90lmnz <- lm(l90_RUnit ~ LZSN, data=l90_seg_lzsn_nz)
plot(l90_RUnit ~ LZSN, data=l90_seg_lzsn)
abline(l90lmnz, col = "red")
summary(l90lmnz)

l90lmlt10 <- lm(l90_RUnit ~ LZSN, data=l90_seg_lzsn_lt10)
plot(l90_RUnit ~ LZSN, data=l90_seg_lzsn_lt10)
abline(l90lmlt10, col = "red")
summary(l90lmlt10)

