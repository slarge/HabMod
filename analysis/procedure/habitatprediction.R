rm(list = ls())
#-------------------------------------------------------------------------------
#Requiredpackages
# library(randomForest)
library(xgboost)
library(raster)
library(automap)
library(maps)
library(marmap)
library(dplyr)
library(viridis)
library(sf)
# devtools::install_github('tidyverse/ggplot2')
library(ggplot2)
# library(scales)
library(ggthemes)
# devtools::install_github("dgrtwo/gganimate")
library(gganimate)
# library(RFinfer)
# devtools::install_github("swager/randomForestCI")
# library(randomForestCI)

sp.i = 73
# mod_type = "PA"

set.seed(627)

xmin = -77
xmax = -65
ymin = 35
ymax = 45

xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
crs <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

## North America layer
ne_countries <- rnaturalearth::ne_countries(scale = 10,
                                            continent = "North America",
                                            returnclass = "sf") %>% 
  sf::st_transform(crs = crs)

# 200 m isobath layer
nesbath <- marmap::getNOAA.bathy(lon1 = xmin, lon2 = xmax, 
                                 lat1 = ymin, lat2 = ymax,
                                 resolution = 1,
                                 keep = TRUE)

bathy_df  <- marmap::fortify.bathy(nesbath) 

#-------------------------------------------------------------------------------
raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"
fig_path <- "analysis/figures/"
#-------------------------------------------------------------------------------
# a grid circumscriobe the ne shelf 
nes.grid <- read.csv(paste0(raw_path, "nes_lon_lat.csv"), header=T)

# make copy of nes.grid
nes.grid.pred <- nes.grid

# make both spatial data frames
coordinates(nes.grid.pred) <- ~x+y

#-------------------------------------------------------------------------------
#often used in code that follows
xy <- with(nes.grid, cbind(x,y))

#------------------------------------------------- get source locations raster

load(paste0(raw_path, "particle transport/for_proj_1day_source_x.rdata"))
particle_x = masked.raster
masked.raster <- NULL

load(paste0(raw_path, "particle transport/for_proj_1day_source_y.rdata"))
particle_y = masked.raster
masked.raster <- NULL

sl_x = raster::extract(particle_x, xy, method="bilinear") 
sl_y = raster::extract(particle_y, xy, method="bilinear") 
sxy = cbind(sl_x,sl_y)

# Load species abbreviateions
sp_abbr <- read.csv("analysis/data/raw_data/SVSPP_abbr.csv",
                    stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Start loop for all models ##
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

season_list <- c("fall", "spring")[2]

season <- "fall"
for(season in season_list) {
  
  habmod_file <- paste0("analysis/data/raw_data/",
                        grep(paste0("-biomass_habmod-", season),
                             list.files("analysis/data/raw_data"),
                             value = TRUE))
  
  # Get the most recent file
  habmod_file <- habmod_file[file.info(habmod_file)$mtime == tail(file.info(habmod_file)$mtime, 1)]
  
  sp_svspp <- readRDS(habmod_file) %>%
    dplyr::select(SVSPP,
                  name) %>% 
    distinct(.keep_all = TRUE)
  
  #-------------------------------------------------------------------------------
  rast_dir = paste0(raw_path, "static_vars/")
  zoo_dir = sprintf("%szooplankton/maps/%s_raster/", raw_path, season)
  
  sst_r = paste0(raw_path, "sst/NESREG/")
  sst_r_clim = paste0(raw_path, "sst/NESREG/clim/")
  sst_f = paste0(raw_path, "sst/NESREG/fronts/")
  sst_f_clim = paste0(raw_path, "sst/NESREG/fronts/clim/")
  
  chl_r = paste0(raw_path, "chl/NESREG/")
  chl_r_clim = paste0(raw_path, "chl/NESREG/clim/")
  chl_f = paste0(raw_path, "chl/NESREG/fronts/")
  chl_f_clim = paste0(raw_path, "chl/NESREG/fronts/clim/")
  
  ST_SD_dir = sprintf("%ssurftemp/%s_spdf/rasters/", raw_path, season)
  BT_SD_dir = sprintf("%sbottemp/%s_spdf/rasters/", raw_path, season)
  BOTSALIN_dir = sprintf("%sbotsal/%s_spdf/rasters/", raw_path, season)
  SURFSALIN_dir = sprintf("%ssurfsal/%s_spdf/rasters/", raw_path, season)
  
  PRESPROB_dir <-  sprintf("%s%s/", derived_path, season) 

  # sp_svspp$SVSPP
  for(sp.i in  c(28, 73, 74, 105, 107, 141)){
    
    if(!sp.i %in% sp_abbr$SVSPP) {
      stop("Make sure there is a 6 char abbreviation in the analysis/data/raw_data/SVSPP_abbr.csv file.")
    }
    
    name <- sp_svspp$name[sp_svspp$SVSPP == sp.i]
    name_abbr <- sp_abbr$six_names[sp_abbr$SVSPP == sp.i]
    
    dat_file <- grep(name,
                     list.files(paste0(derived_path, season, "/"), 
                                pattern = "-dat.rds", full.names = TRUE), value = TRUE)
    
    files <- grep(name,
                  list.files(paste0(derived_path, season, "/"), 
                             pattern = "_bst.rds"), value = TRUE)
    
    for(mod_type in c("PA", "BM")) {
      
      run.rf <- readRDS(paste0(derived_path, season, "/", 
                               grep(paste0(name, "-", season, "-", mod_type, "_bst"),
                                    files, 
                                    value = TRUE)))
      
      dat <- readRDS(dat_file)
      # names(dat) <- c("data", "pa_names", "bm_selection", "bm_names", "pa_selection")
      sp_dat_raw <- dat$data
      # keepers <- colnames(sp_data)[!colnames(sp_data) %in% c("YEAR", "name", "SVSPP", "BIOMASS", "ABUNDANCE", "PRESENCE", "LON", "LAT")]
      
      if(mod_type == "PA") {
        pred_vars <- dat$pa_names
        selection <- dat$pa_selection
      }
      if(mod_type == "BM") {
        pred_vars <- dat$bm_names
        selection <- dat$bm_selection
      }
      rm(dat)
      
      sp_data <- sp_dat_raw[colnames(sp_dat_raw) %in% pred_vars]    
      
      # Predict on PA for 1) the PRESPROB raster and 2) the threshold for the occupancy
      pa_bst <- readRDS(paste0(derived_path, season, "/", grep(paste0(name, "-", season, "-", "PA_bst"),
                                                               files, value = TRUE)))
      
      if(mod_type %in% c("BM")){

          sp_data$PRESPROB <- predict(pa_bst, newdata = data.matrix(sp_data))
          rm(pa_bst)
        
      } # Close biomass model RF bit
      if(mod_type %in% c("PA")) {
        
        sp_test <- sp_dat_raw[-selection, colnames(sp_dat_raw) %in% c("PRESENCE", pred_vars)]
        ## ROC-Curve
        predRoc <- predict(pa_bst, newdata = data.matrix(sp_test))
        myroc <- pROC::roc(sp_test$PRESENCE, as.vector(predRoc))
        ##adjust optimal cut-off threshold for class probabilities
        threshold <- data.frame(rbind(pROC::coords(myroc, x = "best", best.method = "youden"))) #get optimal cutoff threshold
        
        roc_plot <- pROC::ggroc(myroc) +
          # ggplot2::geom_point(data = threshold, aes(x = specificity, y = sensitivity), color = "red") +
          # ggplot2::geom_text(data = threshold, aes(x = specificity, y = sensitivity), 
          #                    label = round(threshold$threshold, 2), nudge_y = 0.05) +
          ggplot2::geom_abline(intercept = 1, slope = 1, col = "grey70") +
          ggplot2::labs(title = gsub("_", " ", name),
                        subtitle = paste("AUC =", sprintf("%.3f",myroc$auc))) +
          ggplot2::theme_bw() +
          ggplot2::coord_equal()
        
        ggplot2::ggsave(paste0("analysis/figures/", season, "/", name, "-", season, "-", "roc.png"), 
                        plot = roc_plot)

        predCut <- factor( ifelse(predRoc >= threshold$threshold, "1", "0") )
        curConfusionMatrix <- caret::confusionMatrix(predCut, as.factor(sp_test$PRESENCE), positive = "1")
        
        sp_data <- sp_data[colnames(sp_data) %in% pred_vars]
      }
      
      
      # loop for years
      for(year in 1992:2016){
        
        # create dataframe to hold a year of data at a time, start with blank data
        pred.data = data.frame(array(data=NA, dim=c(nrow(nes.grid),length(pred_vars)+2)))
        colnames(pred.data) <- c("x","y", pred_vars)
        
        # reload the x and y each time
        pred.data$x = nes.grid$x
        pred.data$y = nes.grid$y
        
        #-------------------------------------------------------------------------------
        # special case get DEPTH
        if("DEPTH" %in% pred_vars) {
          
          load(paste0(raw_path, "static_vars/rast_gdepth.rdata"))
          pred.data$DEPTH = raster::extract(masked.raster, xy, method="bilinear")
          masked.raster <- NULL
        }
        
        #-------------------------------------------------------------------------------
        # get the rast data used
        for (i in 1:length(pred_vars)){
          test = pmatch("rast",pred_vars[i],nomatch=0)
          if(test == 1){
            fname= paste(rast_dir,pred_vars[i],".rdata", sep="")
            load(fname)
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get rast
        
        #-------------------------------------------------------------------------------
        # get the PRESPROB data used
        for (i in 1:length(pred_vars)){
          test = pmatch("PRESPROB", pred_vars[i], nomatch=0)
          if(test == 1){
            
            # pat = paste(name, "-", season, "-PA-RAST-", year, sep= "")
            pat <- paste0("RAST_", name_abbr, "_", year, ".*PA")
            file <- list.files(path = PRESPROB_dir, pattern = pat, full.names = TRUE)
            # masked.raster <- readRDS(paste0(PRESPROB_dir, file))
            load(file)
            pred.data[,i+2] = raster::extract(masked.raster, xy, method = "bilinear")
            masked.raster <- NULL
          }
        } # end get PREPROB
        
        #-------------------------------------------------------------------------------
        # get the zoo clim data used
        for (i in 1:length(pred_vars)){
          zoo_match <- ifelse(season == "spring",
                              "zoo_spr_clim",
                              "zoo_fal_clim")
          
          test = pmatch(zoo_match, pred_vars[i], nomatch=0)
          if(test == 1){
            load(paste(zoo_dir, gsub(paste0(zoo_match, "_"), "", pred_vars[i]), "_all_yr.rdata",sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")
            masked.raster <- NULL
          }
        } # end get zoo clim
        
        #-------------------------------------------------------------------------------
        # get the sst r clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("sst_r_clim", pred_vars[i],nomatch=0)
          test2 = pmatch("sst_r_clim_s", pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            mo = gsub(".*_clim_|_.*$", "", pred_vars[i])
            pat = paste("2000.",mo,sep="")
            file = list.files(path=sst_r_clim,pattern=pat)
            load(paste(sst_r_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            mo = gsub(".*_clim_s_|_.*$", "", pred_vars[i])
            pat = paste("2000.",mo,sep="")
            file = list.files(path=sst_r_clim,pattern=pat)
            load(paste(sst_r_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get sst clim
        
        # get the sst f clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("sst_f_clim", pred_vars[i], nomatch=0)
          test2 = pmatch("sst_f_clim_s", pred_vars[i], nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = gsub(".*_clim_|_.*$", "", pred_vars[i])
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste("2000.",mo,sep="")
            file = list.files(path=sst_f_clim,pattern=pat)
            load(paste(sst_f_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = gsub(".*_clim_s_|_.*$", "", pred_vars[i])
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste("2000.",mo,sep="")
            file = list.files(path=sst_f_clim,pattern=pat)
            load(paste(sst_f_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get sst clim
        
        #-------------------------------------------------------------------------------
        # get the chl r clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("chl_r_clim",pred_vars[i],nomatch=0)
          test2 = pmatch("chl_r_clim_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = gsub(".*_clim_|_.*$", "", pred_vars[i])
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste("1998.",mo,sep="")
            file = list.files(path=chl_r_clim,pattern=pat)
            load(paste(chl_r_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = gsub(".*_clim_s_|_.*$", "", pred_vars[i])
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste("1998.",mo,sep="")
            file = list.files(path=chl_r_clim,pattern=pat)
            load(paste(chl_r_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get chl clim
        
        # get the chl f clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("chl_f_clim",pred_vars[i],nomatch=0)
          test2 = pmatch("chl_f_clim_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            mo = gsub(".*_clim_|_.*$", "", pred_vars[i])
            pat = paste("1998.",mo,sep="")
            file = list.files(path=chl_f_clim,pattern=pat)
            load(paste(chl_f_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = gsub(".*_clim_s_|_.*$", "", pred_vars[i])
            # mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste("1998.",mo,sep="")
            file = list.files(path=chl_f_clim,pattern=pat)
            load(paste(chl_f_clim,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get chl clim
        
        #-------------------------------------------------------------------------------
        #    END STATIC VARS
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # special case get BT_SD
        if("BT_SD" %in% pred_vars) {
          pat = paste("_",year, sep="")
          file = list.files(path = BT_SD_dir,pattern=pat)
          load(paste(BT_SD_dir,file,sep=""))
          pred.data$BT_SD = raster::extract(masked.raster,xy,method="bilinear")  
          masked.raster <- NULL
        }
        
        # special case get ST_SD
        if("ST_SD" %in% pred_vars) {
          pat = paste("_",year,sep="")
          file = list.files(path=ST_SD_dir,pattern=pat)
          load(paste(ST_SD_dir,file,sep=""))
          pred.data$ST_SD = raster::extract(masked.raster,xy,method="bilinear")  
          masked.raster <- NULL
        }
        
        # special case get BOTSALIN
        if("BOTSALIN" %in% pred_vars) {
          pat = paste("_",year,sep="")
          file = list.files(path=BOTSALIN_dir,pattern=pat)
          load(paste(BOTSALIN_dir,file,sep=""))
          pred.data$BOTSALIN = raster::extract(masked.raster,xy,method="bilinear")  
          masked.raster <- NULL
        }
        
        # special case get BOTSALIN
        if("SURFSALIN" %in% pred_vars) {
          pat = paste("_",year,sep="")
          file = list.files(path=SURFSALIN_dir,pattern=pat)
          load(paste(SURFSALIN_dir,file,sep=""))
          pred.data$SURFSALIN = raster::extract(masked.raster,xy,method="bilinear")  
          masked.raster <- NULL
        }
        
        #-------------------------------------------------------------------------------
        # get the zoo ann data used
        for (i in 1:length(pred_vars)){
          zoo_match <- ifelse(season == "spring",
                              "zoo_spr_ann",
                              "zoo_fal_ann")
          
          test = pmatch(zoo_match, pred_vars[i], nomatch = 0)
          if(test == 1){
            pat = paste("_", year, sep="")
            file = list.files(path = paste(zoo_dir , gsub(paste0(zoo_match, "_"), "", pred_vars[i]), "/", sep=""), pattern = pat)
            load(paste(zoo_dir , gsub(paste0(zoo_match, "_"), "",pred_vars[i]),"/",file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get zoo ann
        
        #-------------------------------------------------------------------------------
        
        # get the sst r clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("sst_r_ann",pred_vars[i],nomatch=0)
          test2 = pmatch("sst_r_ann_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=sst_r,pattern=pat)
            load(paste(sst_r,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=sst_r,pattern=pat)
            load(paste(sst_r,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")
            masked.raster <- NULL
          }
        } # end get sst clim
        
        # get the sst f clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("sst_f_ann",pred_vars[i],nomatch=0)
          test2 = pmatch("sst_f_ann_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=sst_f,pattern=pat)
            load(paste(sst_f,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")
            masked.raster <- NULL
          }
          if(test == 2){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=sst_f,pattern=pat)
            load(paste(sst_f,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")
            masked.raster <- NULL
          }
        } # end get sst clim
        
        #-------------------------------------------------------------------------------
        # get the chl r clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("chl_r_ann",pred_vars[i],nomatch=0)
          test2 = pmatch("chl_r_ann_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=chl_r,pattern=pat)
            load(paste(chl_r,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=chl_r,pattern=pat)
            load(paste(chl_r,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get chl clim
        
        # get the chl f clim data used
        for (i in 1:length(pred_vars)){
          test1 = pmatch("chl_f_ann",pred_vars[i],nomatch=0)
          test2 = pmatch("chl_f_ann_s",pred_vars[i],nomatch=0)
          test = test1+test2
          if(test == 1){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=chl_f,pattern=pat)
            load(paste(chl_f,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,xy,method="bilinear")  
            masked.raster <- NULL
          }
          if(test == 2){
            mo = substr(pred_vars[i], nchar(pred_vars[i])-1, nchar(pred_vars[i]))
            pat = paste(year,".",mo,sep="")
            file = list.files(path=chl_f,pattern=pat)
            load(paste(chl_f,file,sep=""))
            pred.data[,i+2] = raster::extract(masked.raster,sxy,method="bilinear")  
            masked.raster <- NULL
          }
        } # end get chl clim
        
        # predict habitat values based in rf model
        bst_pr <- predict(run.rf, data.matrix(pred.data[, !names(pred.data) %in% c("x", "y")]))
        
        cont.out = data.frame(V1 = pred.data$x,
                              V2 = pred.data$y,
                              value = bst_pr)
        # make spdf for kriging
        coordinates(cont.out) <- ~V1+V2
        
        # Ordinary kriging to make complete depth grid
        kriging_result <- autoKrige(value ~ 1, 
                                    input_data = cont.out,
                                    new_data = nes.grid.pred)
        
        # get krig output
        k_out <- kriging_result$krige_output
        
        # make project and save raster
        masked.raster = rasterFromXYZ(k_out) 
        crs(masked.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
        
        outfile <- sprintf("RAST_%s_%s.00.00.%s.%s.%s.000000000.RData",
                           name_abbr,
                           year,
                           mod_type,
                           substr(season, start = 1, stop = 4),
                           sprintf("%04d", sp.i))
        
        # write locally or to google drive?
        save(masked.raster, file = paste0(derived_path, season, "/", outfile))
      } # end year loop 
    } # end mod_type loop
    
    mod_type = "BM"
      
      get_stack <- function(path = derived_path, name_abbr, mod_type){ 
        
        raster_files <- list.files(path, pattern = "^RAST_.*.RData$", full.names = TRUE)
        sp_files <- grep(pattern = name_abbr, raster_files, value = TRUE)
        mod_files <- grep(pattern = mod_type, sp_files, value = TRUE)
        
        s <- raster::stack()
        for(i in 1:length(mod_files)){
          print(mod_files[i])
          masked.raster <- NULL
          year <- as.numeric(stringr::str_extract(mod_files[i], "[0-9]{4}"))
          
          load(mod_files[i])
          names(masked.raster) <- paste0(mod_type, "-", year)
          s <- stack(s, masked.raster)
        }
        
        return(s)
      }
      
      s <- get_stack(path = file.path(derived_path, season), name_abbr, mod_type)
   
      time <- 1:nlayers(s)
      regression_coefficient <- function(x) {
        if(is.na(x[1])){
          NA
        } else {
          lm(x ~ time)$coefficients[2] * length(time)
        }
      }
      
      signficant_value = function(x) {
        if(is.na(x[1])) {
          NA 
        } else {
          anova(lm(x ~ time))$'Pr(>F)'[1]
        }
      }
      
      if(mod_type == "BM"){
        pa <- get_stack(path = file.path(derived_path, season), name_abbr, mod_type = "PA")
        
        thresh_m <- c(0, threshold$threshold, 0, threshold$threshold, 1, 1)
        thresh_mat <- matrix(thresh_m, ncol = 3, byrow = TRUE)
        thresh_mask <- reclassify(pa, thresh_mat)
        hab_pa <- thresh_mask %>%
          calc(fun = sum)
        
        sum_m <- c(0, .99, 0, 1, Inf, 1)
        sum_mat <- matrix(sum_m, ncol = 3, byrow = TRUE)
        sum_mask <- reclassify(hab_pa, sum_mat)
        
        # present_mask = function(x) { x[x < 1] <- NA; return(x)}
        present_mask <- calc(sum_mask,  function(x) { x[x < 1] <- NA; return(x)})
        s_mask <- mask(s, present_mask)
        
      }

      
      get_raster <- function(path = derived_path, name_abbr, mod_type){ 
        
        raster_files <- list.files(path, pattern = "^RAST_.*.RData$", full.names = TRUE)
        sp_files <- grep(pattern = name_abbr, raster_files, value = TRUE)
        mod_files <- grep(pattern = mod_type, sp_files, value = TRUE)
        
        each_year <- data.frame()
        for(i in 1:length(mod_files)){
          masked.raster <- NULL
          load(mod_files[i])
          
          if(mod_type == "BM"){
            masked.raster <- mask(masked.raster, present_mask)
          }
          
          each_year <-  masked.raster %>%  
            rasterToPoints(spatial = TRUE) %>% 
            as.data.frame %>% 
            mutate(YEAR = as.numeric(stringr::str_extract(mod_files[i], "[0-9]{4}"))) %>% 
            rename(pred = layer) %>% 
            bind_rows(each_year)
        }
        return(each_year)
      }
      
      all_pred <- get_raster(path = file.path(derived_path, season), name_abbr, mod_type)
      
      map_title <- case_when(mod_type == "BO" ~ "Biomass only model",
                             mod_type == "BM" ~ "Biomass with occupancy model",
                             mod_type == "PA" ~ "Occupancy model")
      
      map_legend <- case_when(mod_type %in% c("BO", "BM") ~ "Biomass (log) in tons",
                              mod_type == "PA" ~ "Probability of presence")
      
      pp1 <- ggplot() +
        geom_tile(data = all_pred, aes(x = x, y = y, fill = pred, frame = YEAR), alpha = 0.8) +
        geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
        stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey90", size = 0.25) +
        scale_fill_viridis(direction = -1) +
        coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
        theme_map() +
        labs(subtitle = paste0(gsub("_", " ", name), " - ", map_title),
             fill = map_legend) +
        theme(legend.position = "bottom") +
        theme(legend.key.width = unit(2, "cm"))
      
      pp2 <- ggplot() +
        geom_tile(data = all_pred, aes(x = x, y = y, fill = pred), alpha = 0.8) +
        geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
        stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey90", size = 0.25) +
        scale_fill_viridis(direction = -1) +
        coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
        theme_map() +
        labs(subtitle = paste0(gsub("_", " ", name), " - ", map_title),
             fill = map_legend) +
        theme(legend.position = "bottom",
              strip.background = element_blank(),
              legend.key.width = unit(2, "cm")) +
        facet_wrap(~YEAR)
      
      gganimate(pp1, filename = paste0(fig_path, season, "/", name, "-", season, "-", mod_type, ".gif"))
      
      ggsave(filename = paste0(fig_path, season, "/", name, "-", season, "-", mod_type, ".png"), 
             plot = pp2, 
             width = 210, 
             height = 297, 
             units = "mm")
      
      hab_slope <- s_mask %>% 
        calc(fun = regression_coefficient)
      
      hab_sig <- s_mask %>% 
        calc(fun = signficant_value)
      
      ## reclassify everything above 0.05 as 0 for *hab_sig rasters
      sig_m <- c(0, 0.05, 1, 0.05, 1, 0)
      sig_mat <- matrix(sig_m, ncol = 3, byrow = TRUE)
      sig_mask <- reclassify(hab_sig, sig_mat)
      pvalue_mask = function(x) { x[x < 1] <- NA; return(x)}
      
      hab_mask <- calc(sig_mask, pvalue_mask)
      
      hab <- mask(hab_slope, hab_mask)

      hab_final <- hab %>%  
        rasterToPoints(spatial = TRUE) %>% 
        as.data.frame %>% 
        rename(pred = layer) %>% 
        mutate(pred = pred)
      
      sig_subtitle <- case_when(mod_type %in% c("PA") ~ bquote(Significant~(p<=0.05)~change~"in"~.(season)~occurance~from~1992-2016),
                                mod_type %in% c("BM", "BO") ~ bquote(Significant~(p<=0.05)~change~"in"~.(season)~biomass~from~1992-2016))
      
      raw_subtitle <- case_when(mod_type %in% c("PA") ~ paste0("Change in ", season, " occurance from 1992-2016"),
                                mod_type %in% c("BM", "BO") ~ paste0("Change in ", season, " biomass from 1992-2016"))
      
      pp3 <- ggplot() +
        geom_tile(data = hab_final, aes(x = x, y = y, fill = pred), alpha = 0.8) +
        geom_sf(data = ne_countries, color = "grey70", fill = "grey60", size = 0.25) +
        stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey60", size = 0.25) +
        scale_fill_gradient2(midpoint = 0, low = "#0000FF", mid = "#FFFFFF", high ="#FF0000") +
        coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
        theme_map() +
        labs(title = paste0(gsub("_", " ", name)), 
             subtitle = sig_subtitle,
             fill = "regression coefficient") +
        theme(legend.position = "bottom",
              strip.background = element_blank())
      # pp3

      ggsave(filename = paste0(fig_path, season, "/", name, "-", season,  "-", mod_type, "-slope.png"), 
             plot = pp3) 
      
      hab_raw <- hab_slope %>%  
        rasterToPoints(spatial = TRUE) %>% 
        as.data.frame %>% 
        rename(pred = layer) %>% 
        mutate(pred = pred)
      
      pp4 <- ggplot() +
        geom_tile(data = hab_raw, aes(x = x, y = y, fill = pred), alpha = 0.8) +
        geom_sf(data = ne_countries, color = "grey70", fill = "grey60", size = 0.25) +
        stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey60", size = 0.25) +
        scale_fill_gradient2(midpoint = 0, low = "#0000FF", mid = "#FFFFFF", high ="#FF0000") +
        coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
        theme_map() +
        labs(title = paste0(gsub("_", " ", name)), 
             subtitle = raw_subtitle,
             fill = "regression coefficient") +
        theme(legend.position = "bottom",
              strip.background = element_blank())
      # pp4

      ggsave(filename = paste0(fig_path, season, "/", name, "-", season,  "-", mod_type, "-raw_slope.png"), 
             plot = pp4)

  } # close species loop
} # close season_list loop
