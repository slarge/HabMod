# Load new data

devtools::install_github("tidyverse/googledrive")
library(googledrive)
library(dplyr)

# need to do this: https://github.com/tidyverse/googledrive/issues/79
options(httr_oob_default=TRUE) 
drive_auth(new_user = TRUE) 

x <- drive_path("~/hab_mod/data/")

lapply(drive_ls(x)$name, function(x) drive_download(file = x, path = paste0("data/", x)))


drive_download(file = drive_ls(x)[1,], path = "data/spring.data.RData")

