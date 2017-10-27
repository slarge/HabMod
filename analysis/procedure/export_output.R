
# need to do this: https://github.com/tidyverse/googledrive/issues/79
options(httr_oob_default=TRUE)
googledrive::drive_auth()

raw_path <- "analysis/data/raw_data"
figure_path <- "analysis/figures"

output_folder <- googledrive::drive_mkdir("analysis/output", parent = "temp_for_scott")

output_folder_spring <- googledrive::drive_mkdir("spring", parent = "temp_for_scott/analysis/output")
output_folder_fall <- googledrive::drive_mkdir("fall", parent = "temp_for_scott/analysis/output")

googledrive::drive_mkdir("data", parent = "temp_for_scott/analysis/")
raw_data_folder <- googledrive::drive_mkdir("raw_data", parent = "temp_for_scott/analysis/data")
googledrive::drive_mkdir("fall", parent = "temp_for_scott/analysis/data/raw_data")
googledrive::drive_mkdir("spring", parent = "temp_for_scott/analysis/data/raw_data")

googledrive::drive_mkdir("derived_data", parent = "temp_for_scott/analysis/data")
googledrive::drive_mkdir("fall", parent = "temp_for_scott/analysis/data/derived_data")
googledrive::drive_mkdir("spring", parent = "temp_for_scott/analysis/data/derived_data")

raw_files <- file.path(raw_path, 
               grep("biomass_habmod-.*.rds", 
                    list.files(raw_path),
                    value = TRUE))
rw_dat <- googledrive::as_dribble("temp_for_scott/analysis/output/fall/")

files <- purrr::map(raw_files, googledrive::drive_upload, path = raw_data_folder, verbose = TRUE)


spring_figures <- list.files(file.path(figure_path, "spring"), full.names = TRUE)

sfigures <- purrr::map(spring_figures, googledrive::drive_upload, path = output_folder_spring, verbose = TRUE)


fall_figures <- file.path(figure_path, 
                            grep("fall.*",
                                  list.files(figure_path),
                                  value = TRUE))
output_folder_fall <- googledrive::as_dribble("temp_for_scott/analysis/output/fall/")
ffigures <- purrr::map(fall_figures, googledrive::drive_upload, path = output_folder_fall, verbose = TRUE)


derived_fall <- file.path(paste0(derived_path, "fall"), list.files(file.path(derived_path, "fall")))
derived_folder_fall <- googledrive::as_dribble("temp_for_scott/analysis/data/derived_data/fall/")
fderived <- purrr::map(derived_fall, googledrive::drive_upload, path = derived_folder_fall, verbose = TRUE)


derived_spring <- file.path(paste0(derived_path, "spring"), list.files(file.path(derived_path, "spring")))
derived_folder_spring <- googledrive::as_dribble("temp_for_scott/analysis/data/derived_data/spring/")
sderived <- purrr::map(derived_spring, googledrive::drive_upload, path = derived_folder_spring, verbose = TRUE)

  
fall_dat <- list.files("analysis/data/derived_data/fall", full.names = TRUE)
fall_dat <- fall_dat[!grepl("-fall", fall_dat)]

new_fall_dat <- sub("-", "-fall-", fall_dat)
file.rename(fall_dat, new_fall_dat)



spring_figs <- file.path(figure_path, 
          list.files(figure_path)[grepl("spring.*", 
                                         list.files(figure_path))])
new_spring_figs <- file.path(figure_path, "spring",
                             grep("spring.*", 
                                   list.files(figure_path),
                                   value = TRUE))
# new_spring_figs <- sub("_roc", "-spring-roc", spring_figs)
file.rename(spring_figs, new_spring_figs)


fall_figs <- file.path(figure_path, 
                         list.files(figure_path)[grepl("fall.*", 
                                                       list.files(figure_path))])
new_fall_figs <- file.path(figure_path, "fall",
                             grep("fall.*", 
                                  list.files(figure_path),
                                  value = TRUE))
# new_spring_figs <- sub("_roc", "-spring-roc", spring_figs)
file.rename(fall_figs, new_fall_figs)



googledrive::drive_ls(rw_dat)
googledrive::drive_rm(rw_dat)

## ~~~~~~~~~~~ ##
## Raster data ##
## ~~~~~~~~~~~ ##
subDirs <- c("static_vars")
             


gd_loader <- function(gdDir, subDir, mainDir) {
  library(magrittr)
  # gdDir = google drive directory: "temp_for_scott/1_habitat_analysis_2017/"
  # subDir = subdirectory: "particle transport"
  # mainDir = where the file shoudl go: "analysis/data/raw_data/particle transport"
  
  drive_files <- googledrive::drive_ls(paste0(gdDir, subDir), pattern = ".rdata|.RData")
  cat("Found ", nrow(drive_files), " files in ", subDir, ".\n", sep = "")
  
  ifelse(!dir.exists(file.path(mainDir, subDir)), 
         dir.create(file.path(mainDir, subDir), recursive = TRUE), FALSE)
  
  cat("\nAttempting to download.\n")
  
  
  
  drive_d <- function(d, 
                      mainDir,
                      subDir,
                      verbose,
                      overwrite) {
    
    path_name <- file.path(mainDir, subDir, d["name"])
    file_name <- d
    
    googledrive::drive_download(file = file_name,
                                path = path_name,
                                verbose = verbose,
                                overwrite = overwrite)
  }
  
  
  drive_files %>% 
    split(.$name) %>% 
    purrr::map(drive_d, 
               mainDir = mainDir,
               subDir = subDir,
               verbose = FALSE, overwrite = TRUE)
  
}