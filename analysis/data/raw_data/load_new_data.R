# Load new data 
rm(list = ls())

## 1. make sure googledrive is loaded
library(googledrive)

## 2. Authenticate the connection 
## (this will open up a browser window. Sign in to your relevant google account, 
## copy and paste the code into the console when it asks to "Enter authorization code:")
options(httr_oob_default=TRUE)
googledrive::drive_auth()


## 3. Make sure HabMod package is loaded with ctrl+shift+l

## 4. Download spring.data.RData and fall.data.Rdata from appropriate GoogleDrive locations
spring_drive_path <- "~/temp_for_scott/1_habitat_analysis_2017/spring models lt only/"
data_path <- "analysis/data/raw_data/"

spring_drive_files <-  googledrive::drive_ls(spring_drive_path, pattern = "spring.data.RData")
googledrive::drive_download(file = spring_drive_files,
               path = file.path(data_path, spring_drive_files$name),
               overwrite = TRUE)

fall_drive_path <- "~/temp_for_scott/1_habitat_analysis_2017/fall models lt only/"
fall_drive_files <-  googledrive::drive_ls(fall_drive_path, pattern = "fall.data.RData")

googledrive::drive_download(file = fall_drive_files,
                            path = file.path(data_path, fall_drive_files$name),
                            overwrite = TRUE)

## 5. Now you are all set to run the /analysis/procedure/habitatmodel.R and /analysis/procedure/xgboost_habitat.R analyses.


## 6 To run the habitatprediction.R scripts, you will need some raster data... the subsequent code will load that, but it will take some time.

## ~~~~~~~~~~~ ##
## Raster data ##
## ~~~~~~~~~~~ ##

subDirs <- c("static_vars", 
             "particle transport", 
             "zooplankton/maps/spring_raster/", "zooplankton/maps/fall_raster/",
             "sst/NESREG/",
             "sst/NESREG/clim/",
             "sst/NESREG/fronts/",
             "sst/NESREG/fronts/clim/",
             "chl/NESREG/",
             "chl/NESREG/clim/",
             "chl/NESREG/fronts/",
             "chl/NESREG/fronts/clim/",
             "surftemp/spring_spdf/rasters/", "surftemp/fall_spdf/rasters/",
             "bottemp/spring_spdf/rasters/","bottemp/fall_spdf/rasters/",
             "botsal/spring_spdf/rasters/","botsal/fall_spdf/rasters/",
             "surfsal/spring_spdf/rasters/", "surfsal/fall_spdf/rasters/")

lapply(subDirs, function(x) gd_loader(gdDir = "temp_for_scott/1_habitat_analysis_2017/",
                                      subDir = x, 
                                      mainDir = "analysis/data/raw_data"))

climDirs <- do.call(paste0, expand.grid("zooplankton/maps/", c("spring_raster/", "fall_raster/"),
                   c("acarspp", "calfin", "chaeto", "cham", "cirr", "clim", "ctyp", "echino", "evadnespp", "gas",
                     "hyper", "larvaceans", "mlucens", "oithspp", "para", "penilia", "pseudo", "salps", "tlong", "volume"),
                   "/"))

climDirs <- climDirs[!grepl("fall_raster/clim", climDirs)]

lapply(climDirs, function(x) gd_loader(gdDir = "temp_for_scott/1_habitat_analysis_2017/",
                                      subDir = x, 
                                      mainDir = "analysis/data/raw_data"))



SVSPP <- read.csv("analysis/data/raw_data/SVSPP.csv")
species_list <- c(101,102,103,104,105,
                  106,107,108,109,112,
                  121,13,131,135,139,
                  14,141,143,145,15,
                  151,155,156,163,164,
                  168,171,172,176,177,
                  22,23,24,25,26,
                  27,28,32,33,34,
                  35,36,69,72,73,
                  74,75,76,77,78,84)
SVSPP %>% 
  filter(SVSPP %in% species_list) %>%
  mutate(COMNAME = tolower(COMNAME)) %>% 
  tidyr::extract(COMNAME, into=c('partA', 'partB'), '(.*)\\s+([^ ]+)$', remove = FALSE) %>% 
  mutate(partA = substr(partA, start = 1, stop = 3),
         partB = substr(partB, start = 1, stop = 3),
         partC = ifelse(is.na(partA) | is.na(partB),
                    substr(COMNAME, start = 1, stop = 6),
                    paste0(partA, partB)),
         six_names = stringr::str_pad(partC, 6, pad = "z", side = "right")) %>% 
  dplyr::select(-partA,
         -partB,
         -partC) %>% 
  write.csv("analysis/data/raw_data/SVSPP_abbr.csv", row.names = FALSE)

