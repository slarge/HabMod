#' Google drive loader
#' 
#' For users with access to the Google Drive holding the data, this function will download the necessary data using the directory structure used in the /analysis/procedures/ files. 
#'
#' @param gdDir 
#' @param mainDir 
#' @param subDir 
#'
#' @return None
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' gd_loader()
#' }
#' @export

gd_loader <- function(gdDir, subDir, mainDir) {
  
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



#' Add a log to the file
#'
#' @param log_name 
#' @param text 
#'
#' @return None
#' @export
#'
#' @examples
#' seed <- 627
#' name <- "Atlantic cod"
#' sample_text <- paste0("Starting ", name, " at ",  Sys.time(), ".\n To recreate, make sure: set.seed(", seed, ")")
#' add_log("~/test.log", text = sample_text)

add_log <- function(log_name, text) {
  log_con <- file(log_name, open = "a")
  cat(text, file = log_con)
  close(log_con)
}

