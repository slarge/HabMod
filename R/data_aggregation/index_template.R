##########################################################################
library(data.table)

data.dir <- 'C:/1_analyses_ne_shelf/habitat analyses/blank data'

load(file.path(data.dir, 'Survdat.RData'))
# Survdat.RData

setkey(survdat, CRUISE6, STRATUM, TOW, STATION)
stations <- unique(survdat)

# variables to retain to distribute to data providers

blank_data_template <- stations[, list(CRUISE6,STRATUM,TOW,STATION,
                                       YEAR, SEASON,LAT,LON,EST_TOWDATE)]

save(blank_data_template, file = file.path(data.dir, "blank_data_template.RData"))
##########################################################################
colnames(survdat)

survdat_stations <- survdat[, list(CRUISE6,STRATUM,TOW,STATION,SVSPP,CATCHSEX,   
SVVESSEL,YEAR ,SEASON,LAT,LON,EST_TOWDATE,DEPTH,SURFTEMP,SURFSALIN,BOTTEMP,
BOTSALIN, ABUNDANCE , BIOMASS)] 

setkey(survdat, CRUISE6, STRATUM, TOW, STATION,SVSPP)
survdat_stations <- unique(survdat_stations)

save(survdat_stations, file = file.path(data.dir, "survdat_stations.rDATA"))

##########################################################################


setwd("C:/1_analyses_ne_shelf/habitat analyses/rf")

load("blank_data_template.rdata")

# divid into spr and fall
blank_data_template.spr <- blank_data_template[(blank_data_template$SEASON=="SPRING"),]
blank_data_template.fal <- blank_data_template[(blank_data_template$SEASON=="FALL"),]

load("surv.hab.wit.spr.rdata")
load("surv.hab.wit.fal.rdata")






remove(surv.hab.wit)



