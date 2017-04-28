#Merge remote sensing data
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
    data.dir <- "L:\\EcoAP\\Data\\survey"
    rem.dir  <- "L:\\EcoAP\\IndexWG"
    out.dir  <- "L:\\EcoAP\\IndexWG"
    memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
    data.dir <- "/home/slucey/slucey/EcoAP/Data/survey"
    rem.dir  <- "/home/slucey/slucey/EcoAP/IndexWG"
    out.dir  <- "/home/slucey/slucey/EcoAP/IndexWG"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Grab survdat.r
load(file.path(data.dir, 'Survdat.RData'))
load(file.choose())

#Grab remote data
load(file.path(rem.dir, "chl_4km_month_data.rdata"))

#remove duplicate columns
extras <- c('YEAR', 'SEASON', 'LAT', 'LON', 'EST_TOWDATE')
chl <- copy(chl_4km_month_data)
chl[, c(extras) := NULL]

#Merge with survdat
setkey(survdat, CRUISE6, STRATUM, TOW, STATION)
survdat.chl <- merge(survdat, chl, by = key(survdat))

#Save new data set
save(survdat.chl, file = file.path(out.dir, 'Survdat_chl_4km.RData'))
