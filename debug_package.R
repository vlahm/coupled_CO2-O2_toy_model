rm(list=ls()); cat('\014')

library(zoo)
library(seacarb)

detach('package:gassyPants', unload=TRUE)
setwd('~/git/gassyPants/gassyPants/R/')
fs = list.files()
for(i in fs) source(i)

load('../data/FishtrapCr.rda')

#now go to hax/R/R_packages/package_write.R if ready to test for reals
library(gassyPants)
