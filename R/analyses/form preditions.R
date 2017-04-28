#-------------------------------------------------------------------------------
#Required packages
# library(data.table)
library(randomForest)
# library(ROCR)
library(miscTools)
library(raster)
library(rgdal)
library(ncdf4)
library(lubridate)
require(maps)
library("akima")
library("maps")
library(marmap)

#-------------------------------------------------------------------------------

setwd("C:/1_analyses_ne_shelf/habitat analyses/rf")

#-------------------------------------------------------------------------------

pred.grid=read.csv(file.choose())

plot(pred.grid$LON,pred.grid$LAT)

run <- subset(surv.hab.spr, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "DEPTH",
                                       "BOTTEMP",
                                       "chl_4km_month_1",
                                       "chl_f_4km_month_1",
                                       "sst_4km_month_1",
                                       "sst_f_4km_month_1",
                                       "ABUNDANCE"))

pred.grid$soft_sed      <- NA
pred.grid$bpi_30_250_layer      <- NA
pred.grid$DEPTH      <- NA
pred.grid$BOTTEMP      <- NA
pred.grid$chl_4km_month_1      <- NA
pred.grid$chl_f_4km_month_1      <- NA
pred.grid$sst_4km_month_1      <- NA
pred.grid$sst_f_4km_month_1      <- NA


run.rf.pr = predict(run.rf,run[,1:vars-1])


##################################################################################
# Softsediment from TNC
masked.raster <- raster("C:/1_analyses_ne_shelf/habitat analyses/Sedimentwgs84/Sedimentwgs84/softsediment")

for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,3]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# BPI broad scale from TNC   bpi_30_250_layer
masked.raster <- raster("C:/1_analyses_ne_shelf/habitat analyses/bpilayers/bpilayers/bpi_30_250")

for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,4]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# depth grid
masked.raster=raster("C:/1_analyses_ne_shelf/habitat analyses/bathy/nes_bath_data.nc", band=1)

for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,5]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
save(pred.grid, file="pred_grid_data.rdata")



##################################################################################
##  BOTTEMP
cmasked.raster=temp.spr.sr
plot(masked.raster)
for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,6]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
##################################################################################
##  chl_4km_month_1
load("C:/temp_data/chl_4km/NESREG/RAST.NESREG.2000.04.01.GB.CHR1.MNTH.000073140.RData")

plot(masked.raster)
for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,7]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
##################################################################################
##  chl_f_4km_month_1
load("C:/temp_data/chl_4km/NESREG/fronts/RAST.NESREG.2000.04.01.GB.CHR1.MNTH.000073140.RData")

plot(masked.raster)
for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,8]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
##################################################################################
##  sst_4km_month_1
load("C:/temp_data/sst_4km/NESREG/RAST.NESREG.2000.04.01.MO.TEMP.MNTH.000073140.RData")
plot(masked.raster)
for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,9]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
##################################################################################
##  sst_f_4km_month_1
load("C:/temp_data/sst_4km/NESREG/fronts/RAST.NESREG.2000.04.01.MO.TEMP.MNTH.000073140.RData")
plot(masked.raster)
for(i in 1:nrow(pred.grid)){
  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,10]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
save(pred.grid.comp, file="pred_grid_data_2000.rdata")




pred.grid.comp <- pred.grid[complete.cases(pred.grid), ]

plot(pred.grid.comp$LON,pred.grid.comp$LAT)


# LOAD  pred_grid_data_2000.rdata and rfmodel_1.rdata
load(file.choose())

run.rf.pr = predict(run.rf,pred.grid.comp[,3:10])

filled.contour(interp(pred.grid.comp$LON, pred.grid.comp$LAT, run.rf.pr),add=T ,
               color.palette=colorRampPalette(c('white','yellow','orange','red')))

map('state', fill = TRUE, add=T,xlim=c(-72,-66),ylim=c(40,45),col = "grey")


plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)

points(pre.data$LON, pre.data$LAT)




pred.hab.lg <- interp(pred.grid.comp$LON, pred.grid.comp$LAT, run.rf.pr,
                      linear=T,
                      xo=seq(-72,-66,.05), yo=seq(40,44.5,.05))



pred.hab.lr = raster(pred.hab.lg)

plot(pred.hab.lr)

# get bathy data
getNOAA.bathy(lon1 = -76, lon2 = -65, lat1 = 34, lat2 = 45,
              resolution = 10) -> nesbath



plot(pred.hab.lr,zlim=c(0, .35))
map('state', fill=T,add=T,col = "grey")
plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)
points(temp.spr$LON, temp.spr$LAT,cex=.5)
box()


area(pred.hab.lr, na.rm=T	)


r <- raster(nrow=18, ncol=36)
a <- area(r)

#get sizes of all cells in raster [km2]
cell_size<-area(pred.hab.lr, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
raster_area<-length(cell_size)*median(cell_size)


##area of hab >.2
pred.hab.lr.SEL <- pred.hab.lr
pred.hab.lr.SEL[pred.hab.lr.SEL <=.2] <- NA
plot(pred.hab.lr.SEL)
#pred.hab.lr.SEL[pred.hab.lr.SEL >999] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(pred.hab.lr.SEL, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]

witch_hab<-length(cell_size)*median(cell_size)















