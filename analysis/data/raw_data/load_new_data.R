# Load new data

# need to do this: https://github.com/tidyverse/googledrive/issues/79
options(httr_oob_default=TRUE)
drive_auth()

drive_path <- "~/hab_mod/data/"
data_path <- "analysis/data/raw_data/"

drive_files <-  drive_ls(drive_path, pattern = "spring.data.RData")

drive_download(file = drive_files,
               path = paste0(data_path, drive_files$name))
