# This script demonstrates a weighted AGWC calculation from 
# the raw model CSV files with parameters
# The script calculates the weighted area above the 
# Strasburg intake for the forest land use, resulting in the 
# fraction of each landsegs forest area flowing to the Strasburg gage
# and the AGWRC parameter from the model for each of those segments.

lufile="http://deq1.bse.vt.edu:81/p6/vadeq/input/scenario/river/land_use/land_use_2013VAHYDRO2018615.csv"
ludat <- read.csv(lufile)
pfile="http://deq1.bse.vt.edu:81/p6/vadeq/input/param/for/P620171001WQf/PWATER.csv"
read.csv(pfile)
params = read.csv(pfile, header=TRUE, skip=1)
pns <- names(params)
pns[1] = "subshed"
names(params) <- pns
frac_params = sqldf(
  "
    select a.landseg, b.riverseg, a.\"for.\" / b.for as frac, a.\"for.\" as lsfor, b.for, c.AGWR
    from ludat as a
    left outer join (
      select riverseg, sum(\"for.\") as for
      from ludat
      group by riverseg
    ) as b
    on (
      a.riverseg = b.riverseg
    )
    left outer join params as c
    on (
      c.subshed = a.landseg
    )
  "
)

params$AGWR[which(params$subshed %in% c("H51165", "N51165", "N51171", "N54031", "N51660", "N51187"))]

stras_comps <- sqldf(
  "
    select * from frac_params 
    where riverseg in ('PS3_5100_5080', 'PS2_5550_5560', 'PS2_5560_5100')
  ")

stras_wgtd <- sqldf(
  " 
    select landseg, sum(a.lsfor)/ b.rfor, a.AGWR
    from stras_comps as a
    left outer join (
      select riverseg,sum(lsfor) as rfor
      from stras_comps
    ) as b
    on (1=1)
    group by landseg, b.rfor
  "
)

frac_params[
  which(frac_params$landseg %in% c("H51165", "N51165", "N51171", "N54031", "N51660", "N51187")),
]

