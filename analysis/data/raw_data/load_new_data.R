# Load new data 

# need to do this: https://github.com/tidyverse/googledrive/issues/79
options(httr_oob_default=TRUE)
googledrive::drive_auth()

drive_path <- "~/temp_for_scott/1_habitat_analysis_2017/spring models lt only/"
data_path <- "analysis/data/raw_data/"

drive_files <-  googledrive::drive_ls(drive_path, pattern = "spring.data.RData")

googledrive::drive_download(file = drive_files,
               path = paste0(data_path, drive_files$name),
               overwrite = TRUE)

drive_path_fall <- "~/temp_for_scott/1_habitat_analysis_2017/fall models lt only/"
drive_files_fall <-  googledrive::drive_ls(drive_path_fall, pattern = "fall.data.RData")

googledrive::drive_download(file = drive_path_fall,
                            path = paste0(drive_path_fall, drive_file_falls$name),
                            overwrite = TRUE)


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
