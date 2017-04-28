#-------------------------------------------------------------------------------
#Required packages
library(randomForest)
library(akima)
require(maps)
library(marmap)
library(raster)

#-------------------------------------------------------------------------------

setwd("C:/1_analyses_ne_shelf/habitat_analyses/rf")

#-------------------------------------------------------------------------------

# get bathy data
getNOAA.bathy(lon1 = -76, lon2 = -65, lat1 = 34, lat2 = 45, resolution = 10) -> nesbath


# LOAD  model
load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_spr_bio.rdata")
load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_spr_abun.rdata")
load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_fal_bio.rdata")
load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_fal_abun.rdata")

load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_spr_ap.rdata")
load("C:/1_analyses_ne_shelf/habitat_analyses/rf/rfmodel_fal_ap.rdata")


iyr=2000

out=matrix(NA,16,3)

wts=c(.065,.075,.085)  # spring bio
wts=c(.13,.15,.17)  # fall bio
wts=c(.2,.22,.24)  # spring abun
wts=c(.25,.3,.35) # fall abun

wts=c(.5,1,1.5) # ap


for (iyr in 2000:2015) {
#print(iyr)
#  get pred grid
  pgfn=gsub(" ","",paste(" C:/1_analyses_ne_shelf/habitat_analyses/rf/pred_grid/pred_grid_data_spr_",iyr,".rdata"))
#  pgfn=gsub(" ","",paste(" C:/1_analyses_ne_shelf/habitat_analyses/rf/pred_grid/pred_grid_data_fal_",iyr,".rdata"))
  
  load(pgfn)

  run.rf.pr = predict(run.rf,pred.grid.comp[,3:10])
  
  pred.hab.lg <- interp(pred.grid.comp$LON, pred.grid.comp$LAT, run.rf.pr,
                        linear=T,
                        xo=seq(-72,-66,.05), yo=seq(40,44.5,.05))
  pred.hab.lr = raster(pred.hab.lg)

  ##area of hab >.02
  pred.hab.lr.SEL <- pred.hab.lr
  pred.hab.lr.SEL[pred.hab.lr.SEL ==1] <- NA
  #pred.hab.lr.SEL[pred.hab.lr.SEL >999] <- NA
  #calculate area of regions under 0 m asl
  #get sizes of all cells under 0 m
  cell_size<-area(pred.hab.lr.SEL, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  #print( length(cell_size)*median(cell_size))
  #print(table(run.rf.pr))
  
  pred.hab.lg <- interp(pred.grid.comp$LON, pred.grid.comp$LAT, run.rf.pr,
                        linear=T,
                        xo=seq(-72,-66,.05), yo=seq(40,44.5,.05))
  pred.hab.lr = raster(pred.hab.lg)
  plot(pred.hab.lr)
  map('state', fill = TRUE, add=T,xlim=c(-72,-66),ylim=c(40,45),col = "grey")
  plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)
  box()
  text(-66,44.4,iyr)

  
  
}  # end iyr
















pred.hab.lg <- interp(pred.grid.comp$LON, pred.grid.comp$LAT, run.rf.pr,
                      linear=T,
                      xo=seq(-72,-66,.05), yo=seq(40,44.5,.05))
pred.hab.lr = raster(pred.hab.lg)
plot(pred.hab.lr)
map('state', fill = TRUE, add=T,xlim=c(-72,-66),ylim=c(40,45),col = "grey")
plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)
box()
text(-66,44.4,iyr)




# BIOMASS  BIOMASS  BIOMASS  BIOMASS   BIOMASS  BIOMASS  BIOMASS  BIOMASS 
##area of hab >.02
pred.hab.lr.SEL <- pred.hab.lr
pred.hab.lr.SEL[pred.hab.lr.SEL <=wts[1]] <- NA
#pred.hab.lr.SEL[pred.hab.lr.SEL >999] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(pred.hab.lr.SEL, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]
out[iyr-1999,1] <- length(cell_size)*median(cell_size)
##area of hab >.03
pred.hab.lr.SEL <- pred.hab.lr
pred.hab.lr.SEL[pred.hab.lr.SEL <=wts[2]] <- NA
#pred.hab.lr.SEL[pred.hab.lr.SEL >999] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(pred.hab.lr.SEL, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]
out[iyr-1999,2] <- length(cell_size)*median(cell_size)
##area of hab >.04
pred.hab.lr.SEL <- pred.hab.lr
pred.hab.lr.SEL[pred.hab.lr.SEL <=wts[3]] <- NA
#pred.hab.lr.SEL[pred.hab.lr.SEL >999] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(pred.hab.lr.SEL, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]
out[iyr-1999,3] <- length(cell_size)*median(cell_size)
# BIOMASS  BIOMASS  BIOMASS  BIOMASS   BIOMASS  BIOMASS  BIOMASS  BIOMASS 











































#  extra CODE

# area(pred.hab.lr, na.rm=T	)

#get sizes of all cells in raster [km2]
cell_size<-area(pred.hab.lr, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
raster_area<-length(cell_size)*median(cell_size)
print(raster_area)









