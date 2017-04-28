library (raster)
library(rgdal)
library(ncdf4)
library(lubridate)
require(maps)



# load file station file rdata
# in C:\1_analyses_ne_shelf\habitat analyses\blank data

load("C:/1_analyses_ne_shelf/habitat analyses/blank data/blank_data_template.RData")

#colnames(blank_data_template)
# [1] "CRUISE6"     "STRATUM"     "TOW"         "STATION"     "YEAR"        "SEASON"     
# [7] "LAT"         "LON"         "EST_TOWDATE"

##################################################################################
# rugostity from Kristin
Rugosity<-readGDAL("C:/1_analyses_ne_shelf/habitat_analyses/tri_log")

masked.raster <- raster(Rugosity)

remove(Rugosity)  # clean up

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$rugosity <- NA   # Use the same value (0) for all rows
varnum=10

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# BPI from Emily namera_bpi.nc
raster.ncdf <- nc_open("C:/1_analyses_ne_shelf/habitat_analyses/bpi_vrm data/namera_bpi.nc")
raster.full <- as.matrix(t(ncvar_get(raster.ncdf, varid=raster.ncdf$var[[1]]$name)))
lons <- ncvar_get(raster.ncdf,varid = "lon")
lats <- ncvar_get(raster.ncdf,varid = "lat")
masked.raster<- raster(raster.full)
bb <- extent(min(lons),max(lons),min(lats),max(lats) )
extent(masked.raster) <- bb
masked.raster <- setExtent(masked.raster, bb, keepres=TRUE)
e <- extent(-78,-64,34,46)
masked.raster <- crop(masked.raster, e)	

plot(masked.raster)
map('state', fill = F,add=T)


# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$namera_bpi <- NA   # Use the same value (0) for all rows
varnum=11

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# vrm from Emily namera_vrm.nc
raster.ncdf <- nc_open("C:/1_analyses_ne_shelf/habitat_analyses/bpi_vrm data/namera_vrm.nc")
raster.full <- as.matrix(t(ncvar_get(raster.ncdf, varid=raster.ncdf$var[[1]]$name)))
lons <- ncvar_get(raster.ncdf,varid = "lon")
lats <- ncvar_get(raster.ncdf,varid = "lat")
masked.raster<- raster(raster.full)
bb <- extent(min(lons),max(lons),min(lats),max(lats) )
extent(masked.raster) <- bb
masked.raster <- setExtent(masked.raster, bb, keepres=TRUE)
e <- extent(-78,-64,34,46)
masked.raster <- crop(masked.raster, e)	

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$namera_vrm <- NA   # Use the same value (0) for all rows
varnum=12

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# BPI from Emily  necrm_bpi.nc
raster.ncdf <- nc_open("C:/1_analyses_ne_shelf/habitat_analyses/bpi_vrm data/necrm_bpi.nc")
raster.full <- as.matrix(t(ncvar_get(raster.ncdf, varid=raster.ncdf$var[[1]]$name)))
lons <- ncvar_get(raster.ncdf,varid = "lon")
lats <- ncvar_get(raster.ncdf,varid = "lat")
masked.raster<- raster(raster.full)
bb <- extent(min(lons),max(lons),min(lats),max(lats) )
extent(masked.raster) <- bb
masked.raster <- setExtent(masked.raster, bb, keepres=TRUE)
e <- extent(-78,-64,34,46)
masked.raster <- crop(masked.raster, e)	

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$necrm_bpi <- NA   # Use the same value (0) for all rows
varnum=13

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

##################################################################################
##################################################################################
# vrm from Emily  necrm_vrm.nc
raster.ncdf <- nc_open("C:/1_analyses_ne_shelf/habitat_analyses/bpi_vrm data/necrm_vrm.nc")
raster.full <- as.matrix(t(ncvar_get(raster.ncdf, varid=raster.ncdf$var[[1]]$name)))
lons <- ncvar_get(raster.ncdf,varid = "lon")
lats <- ncvar_get(raster.ncdf,varid = "lat")
masked.raster<- raster(raster.full)
bb <- extent(min(lons),max(lons),min(lats),max(lats) )
extent(masked.raster) <- bb
masked.raster <- setExtent(masked.raster, bb, keepres=TRUE)
e <- extent(-78,-64,34,46)
masked.raster <- crop(masked.raster, e)	

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$necrm_vrm <- NA   # Use the same value (0) for all rows
varnum=14

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


##################################################################################
##################################################################################
# varnum=15
# tri from Manderson  SAME AS RUGOSITY FROM KRISTIN
raster.ncdf <- nc_open("C:/1_analyses_ne_shelf/habitat_analyses/Terrain_Ruggedness_Index_Manderson/na_15_tri_m.nc")
raster.full <- as.matrix(ncvar_get(raster.ncdf,varid = "tri"))
masked.raster<- raster(raster.full)
masked.raster = t(masked.raster)
bb <- extent(-77.32028, -54.55778, 34.70428, 50.26261 )
extent(masked.raster) <- bb
masked.raster <- setExtent(masked.raster, bb, keepres=TRUE)

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$complexity  <- NA   # Use the same value (0) for all rows
varnum=15

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop



##################################################################################
##################################################################################
# varnum=16
# MAB sed from Manderson

masked.raster = raster("C:/1_analyses_ne_shelf/habitat_analyses/mab sediment/mab_sedimentgs.asc")
masked.raster <- aggregate(masked.raster, fact=2)

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$mab_sed  <- NA   # Use the same value (0) for all rows
varnum=16

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


##################################################################################
##################################################################################
# varnum=17
# Softsediment from TNC
masked.raster <- raster("C:/1_analyses_ne_shelf/habitat_analyses/Sedimentwgs84/Sedimentwgs84/softsediment")

plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$soft_sed  <- NA   # Use the same value (0) for all rows
varnum=17

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop



##################################################################################
##################################################################################
# varnum=18
# BPI fine scale from TNC
masked.raster <- raster("C:/1_analyses_ne_shelf/habitat_analyses/bpilayers/bpilayers/bpi_3_25")
plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$bpi_3_25_layer  <- NA   # Use the same value (0) for all rows
varnum=18

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop



##################################################################################
##################################################################################
# varnum=19
# BPI broad scale from TNC
masked.raster <- raster("C:/1_analyses_ne_shelf/habitat_analyses/bpilayers/bpilayers/bpi_30_250")
plot(masked.raster)
map('state', fill = F,add=T)


# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$bpi_30_250_layer  <- NA   # Use the same value (0) for all rows
varnum=19

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop



##################################################################################
##################################################################################
# varnum=20
# Seabed forms from TNC
masked.raster = raster("C:/1_analyses_ne_shelf/habitat_analyses/seabedforms/seabedforms/seabedforms")
plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$seabedforms  <- NA   # Use the same value (0) for all rows
varnum=20

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop




##################################################################################
##################################################################################
# varnum=21
# depth grid
masked.raster=raster("C:/1_analyses_ne_shelf/habitat_analyses/bathy/nes_bath_data.nc", band=1)
plot(masked.raster)
map('state', fill = F,add=T)

# MOD MOD MOD MOD MOD MOD MOD
blank_data_template$gdepth  <- NA   # Use the same value (0) for all rows
varnum=21

for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop







##################################################################################
##################################################################################

# write the data

setwd("C:/1_analyses_ne_shelf/habitat_analyses/bpi_vrm data")

benthic_hab_data = blank_data_template

save(benthic_hab_data,file="benthic_hab_data.rdata")

cor(benthic_hab_data[,10:21], use = "pairwise")

dim(benthic_hab_data)
for (i in 10:21){
  print(c(colnames(benthic_hab_data)[i],sum(is.na(benthic_hab_data[,i]))))
  
}



