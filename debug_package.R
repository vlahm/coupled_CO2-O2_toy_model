rm(list=ls()); cat('\014')

# install.packages('seacarb')
library(devtools)
install_github('gholtgrieve/gassyPants')
library(seacarb)
library(zoo)

detach('package:gassyPants', unload=TRUE)
# setwd('~/git/gassyPants/gassyPants/R/')
setwd('~/git/streampulse/other_projects/gassyPants/R/')
fs = list.files()
for(i in fs) source(i)

load('../data/FishtrapCr.rda')
load('../data/RoysCr.rda')

#now go to hax/R/R_packages/package_write.R if ready to test for reals
# library(gassyPants)
