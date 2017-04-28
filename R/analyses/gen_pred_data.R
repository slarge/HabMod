# code to form a time set of pred independant variables

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

setwd("C:/1_analyses_ne_shelf/habitat_analyses/rf")

#-------------------------------------------------------------------------------

# for spring start with this file
# static vars loaded

load("pred_grid_data.rdata")

iyr=2000

for (iyr in 2000:2015){
  
  load("pred_grid_data.rdata")
  
  
  predfilename=gsub(" ","",paste("pred_grid_data_fal_",iyr,".rdata"))
  print(predfilename)

  print(iyr)


##################################################################################
##  BOTTEMP
#tfn=gsub(" ","",paste("C:/1_analyses_ne_shelf/habitat_analyses/rf/gen_bottemp/bottemp_spline_",iyr,".rdata"))
tfn=gsub(" ","",paste("C:/1_analyses_ne_shelf/habitat_analyses/rf/gen_bottemp/bottemp_spline_fal",iyr,".rdata"))

load(file=tfn)
masked.raster=temp.fal.sr
#plot(masked.raster)
for(i in 1:nrow(pred.grid)){
#  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,6]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
print(iyr)

##################################################################################
##  chl_4km_month_1
#tfn=list.files(path="C:/4_rs_data_chlorophyll/globe_4km/1_mon_dat/NESREG", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)
tfn=list.files(path="C:/temp_data/chl_4km/NESREG", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)


load(file=tfn)

#plot(masked.raster)
for(i in 1:nrow(pred.grid)){
#  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,7]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
print(iyr)

##################################################################################
##  chl_f_4km_month_1
#tfn=list.files(path="C:/4_rs_data_chlorophyll/globe_4km/1_mon_dat/NESREG/fronts", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)
tfn=list.files(path="C:/temp_data/chl_4km/NESREG/fronts", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)

load(file=tfn)

#plot(masked.raster)
for(i in 1:nrow(pred.grid)){
#  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,8]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
print(iyr)
##################################################################################
##  sst_4km_month_1
#tfn=list.files(path="C:/4_rs_data_temperature/modis_t_4km/1_mon_dat/FULL_MO.TEMP.MNTH/NESREG", pattern=gsub(" ","",paste(iyr,".04")),full.names=T)
tfn=list.files(path="C:/temp_data/sst_4km/NESREG", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)

load(file=tfn)

#plot(masked.raster)
for(i in 1:nrow(pred.grid)){
#  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,9]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
print(iyr)
##################################################################################
##  sst_f_4km_month_1
#tfn=list.files(path="C:/4_rs_data_temperature/modis_t_4km/1_mon_dat/FULL_MO.TEMP.MNTH/NESREG/fronts", pattern=gsub(" ","",paste(iyr,".04")),full.names=T)
tfn=list.files(path="C:/temp_data/sst_4km/NESREG/fronts", pattern=gsub(" ","",paste(iyr,".10")),full.names=T)

load(file=tfn)

#plot(masked.raster)
for(i in 1:nrow(pred.grid)){
#  print(i)
  xy <- cbind(pred.grid$LON[i],pred.grid$LAT[i])
  pred.grid[i,10]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
##################################################################################
print(iyr)


pred.grid.comp <- pred.grid[complete.cases(pred.grid), ]

save(pred.grid.comp, file=predfilename)


}  # end iyr
