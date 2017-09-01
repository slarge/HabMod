rm(list = ls())
#-------------------------------------------------------------------------------
#Requiredpackages
# library(randomForest)
# library(miscTools)
# library(raster)
# library(automap)
# require(maps)
# library(marmap)

#-------------------------------------------------------------------------------
# setwd("C:/1_habitat_analysis_2017/spring models fits lt")
# setwd("C:/1_habitat_analysis_2017/spring models fits st")
#-------------------------------------------------------------------------------
# # a grid circumscriobe the ne shelf
# nes.grid = read.csv("nes_lon_lat.csv", header=T)
# # make copy of nes.grid
# nes.grid.pred = nes.grid
# # make both spatial data frames
# coordinates(nes.grid.pred) <- ~x+y

#
nes_grid <- read.csv("analysis/data/raw_data/nes_lon_lat.csv",
                     stringsAsFactors = FALSE)
colnames(nes_grid) <- c("LON", "LAT")
nes_grid_pred <- nes_grid

# make both spatial data frames
coordinates(nes_grid_pred) <- ~ LON + LAT

#-------------------------------------------------------------------------------
# get bathy data
nesbath <- getNOAA.bathy(lon1 = -77, lon2 = -65, lat1 = 35, lat2 = 45,
              resolution = 10)
#-------------------------------------------------------------------------------
#often used in code that follows
xy <- with(nes_grid, cbind("LON", "LAT"))
#-------------------------------------------------------------------------------
# get models to predict habitat
files = list.files(pattern=".rdata")
#-------------------------------------------------------------------------------
rast_dir = "C:/1_habitat_analysis_2017/static_vars/"
zoo_dir = "C:/1_analyses_ne_shelf/nes zoo/ecomon 3_1/maps/spring_raster/"

sst_r = "C:/1_habitat_analysis_2017/sst/NESREG/"
sst_r_clim = "C:/1_habitat_analysis_2017/sst/NESREG/clim/"
sst_f = "C:/1_habitat_analysis_2017/sst/NESREG/fronts/"
sst_f_clim = "C:/1_habitat_analysis_2017/sst/NESREG/fronts/clim/"

chl_r = "C:/1_habitat_analysis_2017/chl/NESREG/"
chl_r_clim = "C:/1_habitat_analysis_2017/chl/NESREG/clim/"
chl_f = "C:/1_habitat_analysis_2017/chl/NESREG/fronts/"
chl_f_clim = "C:/1_habitat_analysis_2017/chl/NESREG/fronts/clim/"

ST_SD_dir = "C:/1_habitat_analysis_2017/surftemp/spring_spdf/rasters/"
BT_SD_dir = "C:/1_habitat_analysis_2017/bottemp/spring_spdf/rasters/"
BOTSALIN_dir="C:/1_habitat_analysis_2017/botsal/spring_spdf/rasters/"
SURFSALIN_dir="C:/1_habitat_analysis_2017/surfsal/spring_spdf/rasters/"
#-------------------------------------------------------------------------------

#------------------------------------------------- get source locations raster
load("C:/1_habitat_analysis_2017/particle transport/for_proj_5day_source_x.rdata")
# remane
five_day_x = masked.raster
load("C:/1_habitat_analysis_2017/particle transport/for_proj_5day_source_y.rdata")
# remane
five_day_y = masked.raster

sl_x = extract(five_day_x, xy, method = "bilinear")
sl_y = extract(five_day_y, xy, method = "bilinear")
sxy = cbind(sl_x, sl_y)
#------------------------------------------------- get source loations raster
# loop for models
for(j in 1:length(files)){

  # load random forest model not the AP model
  load(gsub("ap_","",files[j]))

  # create dir for any rasters
  dir.create(file.path(getwd(), gsub("ap_", "", gsub(".rdata", "",files[j]))),
             showWarnings = FALSE)
run.rf <- rf_bm$finalModel
# get variables for the model
im = importance(run.rf)
im = data.frame(im)
pred.vars = row.names(im)
#-------------------------------------------------------------------------------

if(unlist(strsplit(files[j],"_"))[3]=='lts'){
  start_year=1992
}

if(unlist(strsplit(files[j],"_"))[3]=='sts'){
  start_year=2000
}


# loop for years
for (year in start_year:2016){

# create dataframe to hold a year of data at a time, start with blank data
pred.data = data.frame(array(data=NA, dim=c(nrow(nes_grid),length(pred.vars)+2)))
colnames(pred.data) <- c("x","y",pred.vars)

# reload the x and y each time
pred.data$x = nes.grid$x
pred.data$y = nes.grid$y

#-------------------------------------------------------------------------------
# special case get DEPTH
if("DEPTH" %in% pred.vars) {
  load("C:/1_habitat_analysis_2017/static_vars/rast_gdepth.rdata")
  pred.data$DEPTH = extract(masked.raster,xy,method="bilinear")
}

#-------------------------------------------------------------------------------
# get the rast data used
for (i in 1:length(pred.vars)){
  test = pmatch("rast",pred.vars[i],nomatch=0)
  if(test == 1){
    fname= paste(rast_dir,pred.vars[4],".rdata", sep="")
    load(fname)
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
} # end get rast

#-------------------------------------------------------------------------------
# get the zoo clim data used
for (i in 1:length(pred.vars)){
  test = pmatch("zoo_spr_clim",pred.vars[i],nomatch=0)
  if(test == 1){
    load(paste(zoo_dir,gsub("zoo_spr_clim_","",pred.vars[i]), "_all_yr.rdata",sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
} # end get zoo clim

#-------------------------------------------------------------------------------
# get the sst r clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("sst_r_clim",pred.vars[i],nomatch=0)
  test2 = pmatch("sst_r_clim_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("2000.",mo,sep="")
    file = list.files(path=sst_r_clim,pattern=pat)
    load(paste(sst_r_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("2000.",mo,sep="")
    file = list.files(path=sst_r_clim,pattern=pat)
    load(paste(sst_r_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get sst clim

# get the sst f clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("sst_f_clim",pred.vars[i],nomatch=0)
  test2 = pmatch("sst_f_clim_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("2000.",mo,sep="")
    file = list.files(path=sst_f_clim,pattern=pat)
    load(paste(sst_f_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("2000.",mo,sep="")
    file = list.files(path=sst_f_clim,pattern=pat)
    load(paste(sst_f_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get sst clim

#-------------------------------------------------------------------------------
# get the chl r clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("chl_r_clim",pred.vars[i],nomatch=0)
  test2 = pmatch("chl_r_clim_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("1998.",mo,sep="")
    file = list.files(path=chl_r_clim,pattern=pat)
    load(paste(chl_r_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("1998.",mo,sep="")
    file = list.files(path=chl_r_clim,pattern=pat)
    load(paste(chl_r_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get chl clim

# get the chl f clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("chl_f_clim",pred.vars[i],nomatch=0)
  test2 = pmatch("chl_f_clim_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("1998.",mo,sep="")
    file = list.files(path=chl_f_clim,pattern=pat)
    load(paste(chl_f_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste("1998.",mo,sep="")
    file = list.files(path=chl_f_clim,pattern=pat)
    load(paste(chl_f_clim,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get chl clim

#-------------------------------------------------------------------------------
#    END STATIC VARS
# not used
#BOTTEMP
#SURFTEMP
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# special case get BT_SD
if("BT_SD" %in% pred.vars) {
  pat = paste("_",year,sep="")
  file = list.files(path=BT_SD_dir,pattern=pat)
  load(paste(BT_SD_dir,file,sep=""))
  pred.data$BT_SD = extract(masked.raster,xy,method="bilinear")
}

# special case get ST_SD
if("ST_SD" %in% pred.vars) {
  pat = paste("_",year,sep="")
  file = list.files(path=ST_SD_dir,pattern=pat)
  load(paste(ST_SD_dir,file,sep=""))
  pred.data$ST_SD = extract(masked.raster,xy,method="bilinear")
}

# special case get BOTSALIN
if("BOTSALIN" %in% pred.vars) {
  pat = paste("_",year,sep="")
  file = list.files(path=BOTSALIN_dir,pattern=pat)
  load(paste(BOTSALIN_dir,file,sep=""))
  pred.data$BOTSALIN = extract(masked.raster,xy,method="bilinear")
}

# special case get BOTSALIN
if("SURFSALIN" %in% pred.vars) {
  pat = paste("_",year,sep="")
  file = list.files(path=SURFSALIN_dir,pattern=pat)
  load(paste(SURFSALIN_dir,file,sep=""))
  pred.data$SURFSALIN = extract(masked.raster,xy,method="bilinear")
}

#-------------------------------------------------------------------------------
# get the zoo ann data used
for (i in 1:length(pred.vars)){
  test = pmatch("zoo_spr_ann",pred.vars[i],nomatch=0)
  if(test == 1){
    pat = paste("_",year,sep="")
    file = list.files(path=paste(zoo_dir , gsub("zoo_spr_ann_","",pred.vars[i]),"/",sep=""),pattern=pat)
    load(paste(zoo_dir , gsub("zoo_spr_ann_","",pred.vars[i]),"/",file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
} # end get zoo ann

#-------------------------------------------------------------------------------







# get the sst r clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("sst_r_ann",pred.vars[i],nomatch=0)
  test2 = pmatch("sst_r_ann_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=sst_r,pattern=pat)
    load(paste(sst_r,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=sst_r,pattern=pat)
    load(paste(sst_r,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get sst clim

# get the sst f clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("sst_f_ann",pred.vars[i],nomatch=0)
  test2 = pmatch("sst_f_ann_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=sst_f,pattern=pat)
    load(paste(sst_f,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=sst_f,pattern=pat)
    load(paste(sst_f,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get sst clim

#-------------------------------------------------------------------------------
# get the chl r clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("chl_r_ann",pred.vars[i],nomatch=0)
  test2 = pmatch("chl_r_ann_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=chl_r,pattern=pat)
    load(paste(chl_r,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=chl_r,pattern=pat)
    load(paste(chl_r,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get chl clim

# get the chl f clim data used
for (i in 1:length(pred.vars)){
  test1 = pmatch("chl_f_ann",pred.vars[i],nomatch=0)
  test2 = pmatch("chl_f_ann_s",pred.vars[i],nomatch=0)
  test = test1+test2
  if(test == 1){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=chl_f,pattern=pat)
    load(paste(chl_f,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,xy,method="bilinear")
  }
  if(test == 2){
    mo = substr(pred.vars[i], nchar(pred.vars[i])-1, nchar(pred.vars[i]))
    pat = paste(year,".",mo,sep="")
    file = list.files(path=chl_f,pattern=pat)
    load(paste(chl_f,file,sep=""))
    pred.data[,i+2] = extract(masked.raster,sxy,method="bilinear")
  }
} # end get chl clim

# remove incomplete cases
pred.data <- pred.data[complete.cases(pred.data), ]

# predict habitat values based in rf model
run.rf.pr = predict(run.rf,pred.data[,3:ncol(pred.data)])

# over write pred.data since not use again
cont.out = data.frame(cbind(pred.data$x,pred.data$y,run.rf.pr))

# make spdf for kriging
coordinates(cont.out) <- ~V1+V2

# Ordinary kriging to make complete depth grid
kriging_result = autoKrige(run.rf.pr~1, cont.out, nes.grid.pred)

# get krig output
k_out = kriging_result$krige_output

# make project and save raster
masked.raster = rasterFromXYZ(k_out)
crs(masked.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
outfile = paste("RAST_NESREG_",year,".04.03.BT.TEMP.YEAR.000066596.RData",sep="")
outfile = paste(file.path(getwd(), gsub(".rdata","",files[j])),"/",outfile,sep="")
outfile=gsub("ap_","",outfile)
save(masked.raster,file=outfile)

# make pdf
outfile = paste(gsub(".rdata","",files[j]),"_",year,".pdf",sep="")
outfile = paste(file.path(getwd(), gsub(".rdata","",files[j])),"/",outfile,sep="")
outfile=gsub("ap_","",outfile)
pdf (file= outfile)
plot(masked.raster)
map("state",add=T)
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)
dev.off()



} # end year loop

}  # sp model loop

