rm(list = ls())
#-------------------------------------------------------------------------------
#Requiredpackages
library(randomForest)
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
library(randomForestCI)

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

# all_dat_op <- readRDS(paste0(raw_path, 
#                              grep(paste0("biomass_habmod-", season, ".*.rds"), 
#                                   list.files(raw_path),
#                                   value = TRUE)))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Start loop for all models ##
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# sp.names <- gsub("|-.*|-RFmodel.rds$", "", list.files(paste0(derived_path, season, "/"), pattern = "RFmodel.rds"))
# sp.svspp <- unique(all_dat_op$SVSPP[all_dat_op$name %in% sp.names])

season_list <- c("fall", "spring")
sp.svspp <- c(22, 28, 73, 74, 105, 107, 141)
# season = "fall"
# sp.i = 28
for(season in season_list) {
  
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
  
  
  for(sp.i in sp.svspp){

    sp_data <- readRDS(paste0(raw_path, 
                              grep(paste0("biomass_habmod-", season, ".*.rds"), 
                                   list.files(raw_path),
                                   value = TRUE))) %>% 
      dplyr::filter(SVSPP == sp.i) %>% 
      na.omit
    
  name <- unique(sp_data$name)
  
  files <- grep(paste(unique(sp_data$name), collapse = "|"),
                list.files(paste0(derived_path, season, "/"), 
                           pattern = "model.rds"), value = TRUE)

  # mod_type = "PA"
  
  for(mod_type in c("PA", "BM")) {
    run.rf <- readRDS(paste0(derived_path, season, "/", grep(paste0(name, "-", mod_type, "-RF"),
                                                files, value = TRUE)))
    pred_vars <- run.rf$finalModel$xNames
    
    if(mod_type %in% c("BO", "BM")){
      
      if(mod_type == "BM"){
        PA_rf <- readRDS(paste0(derived_path, season, "/", grep(paste0(name, "-", "PA-RF"),
                                                   files, value = TRUE)))
        
        sp_data$PRESPROB <- predict(PA_rf, newdata = sp_data, type = "prob")[, 2]
        rm(PA_rf)
      }
      
      res_var <- dplyr::case_when(mod_type %in% c("BM", "BO") ~ "BIOMASS",
                                  mod_type == "PA" ~ "PRESENCE",
                                  TRUE ~ NA_character_)
      
      rf_data <- sp_data[, c(res_var, pred_vars, "LAT", "LON")]
      
      # if(mod_type %in% c("BM", "BO")){
      rf_data <- rf_data[rf_data$BIOMASS != 0,]
      # }
      
      rf_data <- Filter(var, rf_data)
      
      rf_selection <- caret::createDataPartition(y = rf_data[, res_var],
                                                 p = 0.75,
                                                 list = FALSE)
      
      rf_train <- rf_data[rf_selection, ]
      rf_test <- rf_data[-rf_selection, ]
      
      rf_trainX <- rf_train[, names(rf_train) %in% pred_vars]
      rf_trainY <- rf_train[, res_var]
      
      # if(mod_type == "PA"){
      #   rf_trainY <- as.factor(rf_trainY)
      # }
      
      strt_time <- Sys.time()
      cluster <- parallel::makeCluster(parallel::detectCores() - 1)
      doParallel::registerDoParallel(cluster)

      rf_run <- randomForest::randomForest(x = rf_trainX, y = rf_trainY,
                                           mtry = run.rf$finalModel$mtry,
                                           ntree = 5000,
                                           keep.inbag = TRUE,
                                           importance = TRUE,
                                           replace = TRUE)
      
      parallel::stopCluster(cluster)
      foreach::registerDoSEQ()
      
      stp_time <- Sys.time()
      stp_time - strt_time
      
      # 
      rf_test$PREDICTION <- predict(rf_run,  rf_test)
      rf_test$RESID <- rf_test$BIOMASS - rf_test$PREDICTION
      # 
      
      RMSE_test <- sqrt(sum((rf_test$BIOMASS - rf_test$PREDICTION)^2)/length(rf_test$PREDICTION))
      rRMSE_test <- round(RMSE_test/(max(rf_test$BIOMASS) - min(rf_test$BIOMASS)), 2)
      
      var_hat <- randomForestCI::randomForestInfJack(rf_run, rf_test[, -1], calibrate = TRUE)
      
      # 
      df <- data.frame(BIOMASS = rf_test$BIOMASS, PREDICTED = var_hat$y.hat, error = var_hat$var.hat)
      df <- df %>%
        dplyr::mutate(ci_upper = PREDICTED + (1.96 * sqrt(error)),
                      ci_lower = PREDICTED - (1.96 * sqrt(error)))

      ## Predicted/observed plot
      p1 <- ggplot(df, aes(x = BIOMASS, y = PREDICTED))
      p1 <- p1 +
        geom_linerange(aes(ymin = ci_lower,
                           ymax = ci_upper),
                       color = "black", alpha = 0.2) +
        geom_point(color = "black", alpha = 0.8, shape = 16) +
        geom_abline(intercept=0, slope = 1, linetype = 2) +
        labs(x = "Observed biomass",
             y = "Predicted biomass",
             title = sprintf("%s (%s)", gsub("_", " ", name), season),
             subtitle = paste0("The error (relative RMSE) is about ", rRMSE_test * 100, "% \nas large as the mean biomass.")) +
        coord_equal() +
        theme_bw()
      ggsave(paste0(fig_path, name,"-", season, "-fit_plot.png"), p1, width = 5, height = 5)
      
      ## Importance plot
      im <- caret::varImp(run.rf)
      
      stackedData <- caret:::sortImp(im, top =  dim(im$importance)[1])
      
      featureNames <- rownames(stackedData)
      outcomeNames <- colnames(stackedData)
      featureNames <- gsub("_0d$", "", featureNames)
      
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)), 
                                    levels = rev(featureNames))
      names(stackedData) <- c("Importance", "Feature")
      
      p4 <- ggplot(stackedData, aes(x = Feature, y = Importance)) +
        geom_segment(aes(x = Feature, xend = Feature,
                         y = 0, yend = Importance)) +
        geom_point(stat = "identity", aes(y = Importance), color = "grey50") +
        coord_flip() +
        labs(x = "",
             y = "Importance (scaled)",
             title = gsub("_", " ", name)) +
        theme_bw()
      
      ggsave(paste0(fig_path, name, "-", season, "-importance_plot.png"), p4, width = 5, height = 5)
      
    } # Close biomass model RF bit
    
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
          pat = paste(name, "-", season, "-PA-RAST-", year, sep= "")
          file = list.files(path = PRESPROB_dir, pattern=pat)
          masked.raster <- readRDS(paste0(PRESPROB_dir, file))
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
      
      # remove incomplete cases
      pred.data <- pred.data[complete.cases(pred.data), ]
      
      # predict habitat values based in rf model
      if(mod_type == "PA"){
        run.rf.pr = predict(run.rf$finalModel, pred.data[, !names(pred.data) %in% c("x", "y", "PRESENCE")], type = "prob")[,2]
        
        df <- data.frame(key = "PREDICTED", 
                         value = run.rf.pr)
      }
      
      if(!mod_type == "PA"){
        
        df <- randomForestCI::randomForestInfJack(rf_run, pred.data[, !names(pred.data) %in% c("x", "y")], calibrate = TRUE) %>% 
          rename(PREDICTED = y.hat,
                 error = var.hat) %>% 
          dplyr::mutate(ci_upper = PREDICTED + (1.96 * sqrt(error)),
                        ci_lower = PREDICTED - (1.96 * sqrt(error))) %>% 
          select(-error) %>% 
          tidyr::gather() %>% 
          as.data.frame
      }
      
      for(key.i in unique(df$key)) {
        
        cont.out = data.frame(V1 = pred.data$x,
                              V2 = pred.data$y,
                              value = df$value[df$key == key.i])
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
        
        outfile <- paste0(name, "-", season, "-", mod_type, "-RAST-", year, "-", key.i, ".rds")
        
        saveRDS(masked.raster, file = paste0(derived_path, season, "/", outfile))
      } # end CI loop
    } # end year loop 
    
  } # end mod_type loop
  
  
  get_raster <- function(path = derived_path, name, season, mod_type, pred_type){ 
    
    file_path <- list.files(path, paste0(name, "-", season, "-", mod_type, "-RAST.*",
                                         pred_type, ".rds$"), full.names = TRUE) 
    
    each_year <- data.frame()
    for(i in file_path){
      each_year <-  i %>% 
        readRDS %>% 
        rasterToPoints(spatial = TRUE) %>% 
        as.data.frame %>% 
        mutate(YEAR = as.numeric(gsub(paste0(".*-RAST-|-", pred_type, ".rds$"), "", i))) %>% 
        rename(pred = layer) %>% 
        bind_rows(each_year)
    }
    return(each_year)
  }
  
  mod_type = "BM"
  all_pred <- get_raster(file.path(derived_path, season), name, season, mod_type, pred_type = "PREDICTED")
  ci_upper_pred <- get_raster(file.path(derived_path, season), name, season, mod_type, pred_type = "ci_upper")
  ci_lower_pred <- get_raster(file.path(derived_path, season), name, season, mod_type, pred_type = "ci_lower")
  
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
  
  gganimate(pp1, filename = paste0(fig_path, name, "-", season, "-", mod_type, ".gif"))
  
  ggsave(filename = paste0(fig_path, name, "-", season, "-", mod_type, ".png"), 
         plot = pp2, 
         width = 210, 
         height = 297, 
         units = "mm")
  
  get_stack <- function(path = derived_path, name, season, mod_type, pred_type){ 
    
    file_path <- list.files(path, paste0(name, "-", season, "-", mod_type, "-RAST.*",
                                         pred_type, ".rds$"), full.names = TRUE) 
    s <- raster::stack()
    for(i in file_path){
      print(i)
      nm <- gsub(paste0(".*-RAST-|-", pred_type, ".rds$"), "", i)
      tm <- readRDS(i) 
      names(tm) <- paste0(mod_type, "-", nm)
      s <- stack(s, tm)
    }
    
    return(s)
  }
  mod_type = "BM"
  s <- get_stack(path = file.path(derived_path, season), name = name, season = season, mod_type = mod_type, pred_type = "PREDICTED")
  
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

  hab_slope <- s %>% 
    calc(fun = regression_coefficient)

  hab_sig <- s %>% 
    calc(fun = signficant_value)

  ## reclassify everything above 0.05 as 0 for *hab_sig rasters
  sig_m <- c(0, 0.05, 1, 0.05, 1, 0)
  sig_mat <- matrix(sig_m, ncol = 3, byrow = TRUE)
  sig_mask <- reclassify(hab_sig, sig_mat)
  pvalue_mask = function(x) { x[x < 1] <- NA; return(x)}
  
  hab_mask <- calc(sig_mask, pvalue_mask)
  hab <- mask(hab_slope, hab_mask)

  if(mod_type == "BM") {
    upper_s <- get_stack(path = file.path(derived_path, season), name = name, season = season, mod_type = "BM", pred_type = "ci_upper")
    lower_s <- get_stack(path = file.path(derived_path, season), name = name, season = season, mod_type = "BM", pred_type = "ci_lower")
    
    upper_hab_slope <- upper_s %>% 
      calc(fun = regression_coefficient) 
    
    upper_hab_sig <- upper_s %>% 
      calc(fun = signficant_value) 
    
    lower_hab_slope <- lower_s %>% 
      calc(fun = regression_coefficient) 
    
    lower_hab_sig <- lower_s %>% 
      calc(fun = signficant_value) 
    
    ## reclassify upper raster with significant limits and if the upper slope crosses zero
    upper_hab_mask <- reclassify(upper_hab_sig, sig_mat)
    upper_hab_mask <- calc(upper_hab_mask, pvalue_mask)
    upper_hab_slope[upper_hab_slope >= 0] <- NA
    upper_hab <- mask(upper_hab_slope, upper_hab_mask)
    
    ## reclassify lower raster with significant limits and if the lower slope crosses zero
    lower_hab_mask <- reclassify(lower_hab_sig, sig_mat)
    lower_hab_mask <- calc(lower_hab_mask, pvalue_mask)
    lower_hab_slope[lower_hab_slope <= 0] <- NA
    lower_hab <- mask(lower_hab_slope, lower_hab_mask)
    
    ci_slope <- merge(upper_hab, lower_hab)
    hab <- mask(hab, ci_slope)
  }

  hab_final <- hab %>%  
    rasterToPoints(spatial = TRUE) %>% 
    as.data.frame %>% 
    rename(pred = layer) %>% 
    mutate(pred = pred)
  
  pp3 <- ggplot() +
    geom_tile(data = hab_final, aes(x = x, y = y, fill = pred), alpha = 0.8) +
    geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
    stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey60", size = 0.25) +
    scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
                         high="red") +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    theme_map() +
    labs(title = paste0(gsub("_", " ", name)), 
         subtitle = bquote(Significant~(p<=0.05)~change~"in"~.(season)~biomass~from~1992-2016),
         fill = "regression coefficient") +
    theme(legend.position = "bottom",
          strip.background = element_blank())
  # pp3
  
  ggsave(filename = paste0(fig_path, name, "-", season, "-slope.png"), 
         plot = pp3) 
  
  hab_raw <- hab_slope %>%  
    rasterToPoints(spatial = TRUE) %>% 
    as.data.frame %>% 
    rename(pred = layer) %>% 
    mutate(pred = pred)
  
  pp4 <- ggplot() +
    geom_tile(data = hab_raw, aes(x = x, y = y, fill = pred), alpha = 0.8) +
    geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
    stat_contour(data = bathy_df, aes(x = x, y = y , z = z), breaks = -200, color = "grey60", size = 0.25) +
    scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
                         high="red") +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    theme_map() +
    labs(title = paste0(gsub("_", " ", name)), 
         subtitle = paste0("Change in ", season, " biomass from 1992-2016"),
         fill = "regression coefficient") +
    theme(legend.position = "bottom",
          strip.background = element_blank())
  
  ggsave(filename = paste0(fig_path, name, "-", season, "-raw_slope.png"), 
         plot = pp4)

  } # close species loop
} # close season_list loop
