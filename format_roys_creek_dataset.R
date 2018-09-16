library(stringr)
library(dplyr)
setwd('/home/mike/git/streampulse/other_projects/gassyPants')
load('../coupled_CO2-O2_toy_model/roys_co2_15jul.rda')

ms = str_match(roys_co2_15jul$time, '[0-9]+:[0-9]([0-9]:[0-9]+)')[,2]
ms_log = ms %in% c('0:58', '0:59', '0:00', '0:01')
RoysCr = roys_co2_15jul[ms_log,]
RoysCr$dtime = as.character(RoysCr$dtime)
substr(RoysCr$dtime, 18, 19) = '00'

RoysCr = RoysCr %>%
    select(dateTime=dtime, Temp_C=temp.CO2, pCO2_uatm=CO2, lux=light)
RoysCr[,-1] = apply(RoysCr[,-1], 2, function(x) as.numeric(as.character(x)))

save(RoysCr, file='data/RoysCr.rda')
