library(Matrix)
library(xgboost)
library(xgboostExplainer)

# rm(list = ls())
seed <- 627
set.seed(seed)

raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"

# Using Data Miner with R
# source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")

#SETTING USERNAME AND TOKEN - NOT NEEDED WHEN USING RSTUDIO ON THE PORTAL
if(file.exists(paste0(raw_path, "keys.csv"))){
  keys <- read.csv("analysis/data/raw_data/keys.csv", stringsAsFactors = FALSE)
  username <<- keys$username
  token <<- keys$token
  rm(keys)
}

if(!file.exists(paste0(raw_path, "keys.csv"))) {
  cat("To use the vsurf_bb functionality, go to: https://i-marine.d4science.org/group/stockassessment \nand enter your username and personal token")
  set_keys(save_key = TRUE)
}

## ~~~~~~~~~~~~~ ##
## Load the data ##
## ~~~~~~~~~~~~~ ##
svspp_dat <- read.csv(paste0(raw_path, "SVSPP.csv"),
                      stringsAsFactors = FALSE)

svspp_dat <- svspp_dat %>%
  dplyr::mutate(COMNAME = tolower(COMNAME)) %>%
  dplyr::select(COMNAME, SVSPP) %>%
  dplyr::mutate(COMNAME = gsub("atlantic", "Atlantic", COMNAME),
                COMNAME = gsub("american", "American", COMNAME),
                COMNAME = gsub("acadian", "Acadian", COMNAME)) %>%
  dplyr::distinct(.keep_all = TRUE)

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

join_names <- c("CRUISE6", "STRATUM", "STATION", "SVVESSEL", "YEAR", "SEASON", "LAT",
                "LON", "EST_TOWDATE", "DEPTH", "DOY", "SVSPP")

bad_dat <- c("rast_necrm_bpi", "rast_necrm_vrm" ,
             "rast_bpi_3_25_layer", "rast_bpi_30_250_layer",
             "rast_mab_sed", "rast_gdepth",
             "rast_gravel_fraction", "rast_mud_fraction",
             "rast_phi_fraction", "rast_sand_fraction", 
             "rast_plcurv20km", "rast_plcurv2km",
             "rast_plcurv10km", "SURFTEMP", "BOTTEMP")

season <- "fall"

if(length(grep(paste0("-biomass_habmod-", season), 
               list.files("analysis/data/raw_data"))) != 0) {
  
  habmod_file <- paste0("analysis/data/raw_data/",
                        grep(paste0("-biomass_habmod-", season),
                             list.files("analysis/data/raw_data"),
                             value = TRUE))
  all_dat_op <- readRDS(habmod_file)
  
  log_name <- gsub(".rds", ".log", habmod_file)
  log_name <- gsub("raw_data", "derived_data", log_name)
  rm(habmod_file)
}

if(length(grep(paste0("-biomass_habmod-", season, ".rds"),
               list.files("analysis/data/raw_data"))) == 0) {
  load(paste0("analysis/data/raw_data/", season, ".data.RData"))
  if(season == "fall"){
    season.data <- fall.data
    rm(list = c("fall.data"))
  }
  
  if(season == "spring"){
    season.data <- spring.data
    rm(list = c("spring.data"))
  }
  
  ## ~~~~~~~~~~~~~~~~~ ##
  ## Clean up the data ##
  ## ~~~~~~~~~~~~~~~~~ ##
  names(season.data) <- sub(" ", "", names(season.data))
  lag_dat <- grep("_[1-9]d", colnames(season.data), value = TRUE)
  zoo_static_dat <- grep("zoo_spr_clim_", colnames(season.data), value = TRUE)
  
  season_dat <- season.data %>%
    dplyr::filter(SVSPP %in% species_list) %>%
    dplyr::select(-TOW,
                  -CATCHSEX,
                  -dplyr::one_of(lag_dat),
                  -dplyr::one_of(zoo_static_dat),
                  -dplyr::one_of(bad_dat)) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  rm(list = c("season.data"))
  
  ## ~~~~~~~~~~~~~~ ##
  ## Add zero sites ##
  ## ~~~~~~~~~~~~~~ ##
  
  # Each row is an observation, so count the number of each species per year, strata, and station sampled.
  # Spread by each species, so NAs are created where species aren't present for a year, stratum, and station.
  # Replace NAs with 0 and reorganize into a long data frame by year, stratum, and station.
  # Sum the number of observations per year, species, and stratum. If a species is not found in a year, stratum, station
  # but is found in that year stratum, they are considered "absent" or 0. If a species is not found in a year, stratum, station
  # nor is found in that year stratum, they are NA and removed.
  
  pa_table <- season_dat %>%
    dplyr::group_by(YEAR, SVSPP, STRATUM, STATION) %>%
    dplyr::summarize(count = n()) %>%
    tidyr::spread(SVSPP, value = count) %>%
    dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))) %>%
    tidyr::gather(SVSPP, value = count, -YEAR, -STRATUM, -STATION) %>%
    dplyr::group_by(YEAR, SVSPP, STRATUM) %>%
    dplyr::mutate(stratum_sum = sum(count), #
                  PRESENCE = dplyr::case_when(count == 0 & stratum_sum >= 1 ~ 0, # Not at station but in stratum
                                              count == 1 & stratum_sum >= 0 ~ 1, # Present
                                              count == 0 & stratum_sum == 0 ~ NA_real_, # Not at station or in stratum
                                              TRUE ~ NA_real_)) %>%
    dplyr::filter(!is.na(PRESENCE))
  
  # Create a data.frame of just the station data
  station_dat <- season_dat %>%
    dplyr::select(-SVSPP,
                  -ABUNDANCE,
                  -BIOMASS) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # join the p/a data with the station data
  pa_dat <- pa_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SVSPP = as.numeric(SVSPP)) %>%
    dplyr::left_join(station_dat, by = c("YEAR", "STRATUM", "STATION")) %>%
    dplyr::select(-stratum_sum, -count)
  
  # join the p/a data with abundance and biomass data
  bio_dat <- season_dat %>%
    dplyr::select(ABUNDANCE,
                  BIOMASS,
                  join_names) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  all_dat_op <- pa_dat %>%
    dplyr::left_join(bio_dat, by = join_names) %>%
    dplyr::left_join(svspp_dat, by = "SVSPP") %>%
    dplyr::mutate(ABUNDANCE = ifelse(is.na(ABUNDANCE),
                                     0,
                                     ABUNDANCE),
                  ABUNDANCE = as.numeric(ABUNDANCE),
                  BIOMASS = ifelse(is.na(BIOMASS),
                                   0,
                                   BIOMASS),
                  BIOMASS = log10(as.numeric(BIOMASS) + 1),
                  SVSPP = as.numeric(SVSPP),
                  name = as.character(gsub(" ", "_", COMNAME))) %>% 
    dplyr::select(-dplyr::one_of(join_names),
                  -COMNAME,
                  SVSPP, LON, LAT, YEAR) %>%
    as.data.frame
  
  # na.omit %>%
  
  ## Here, instead of creating SAC, we will do it later if SAC is present. The dat_maker
  ## function finds the max row/col of data for each species.
  # all_dat_op <- lapply(unique(all_dat$SVSPP), dat_maker) %>%
  # dplyr::bind_rows()
  
  # ## Split the datasets into PA and Biomass/Abundance partitions
  # data_part_list <- lapply(unique(all_dat_op$SVSPP), function(x)
  #   createDataPartition(y = all_dat_op %>%
  #                         dplyr::filter(SVSPP == x) %>%
  #                         dplyr::select(PRESENCE) %>%
  #                         pull,
  #                       p = 0.50,
  #                       list = FALSE))
  #
  # data_partition <- list(selection = data_part_list)
  # class(data_partition) <- c("tbl_df", "data.frame")
  # attr(data_partition, "row.names") <- .set_row_names(length(data_part_list))
  # data_partition$SVSPP <- unique(all_dat_op$SVSPP)
  # data_partition <- unnest(data_partition)
  
  log_file <- paste(gsub("-", "", Sys.Date()), "-biomass_habmod-", season,".log", sep="")
  log_name <- paste0(derived_path, season, "/", log_file)
  saveRDS(all_dat_op, 
          file = paste0(raw_path, gsub(".log", ".rds", log_file)))
}

# sp.list <- c(22, 28, 73, 74, 105, 107, 141)
sp.list <- c(28, 73, 74, 105, 107, 141)
# sp.list <- c(141, 32, 72, 112, 163, 197)
# sp.list <- c(112, 163, 197)
# sp.list <- 22
# sp.i <- 22
for(sp.i in sp.list) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Split data into P/A model and Biomass model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pa_data <- all_dat_op %>% 
    dplyr::filter(SVSPP == sp.i) %>% 
    na.omit
  
  name <- unique(pa_data$name)
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Starting ", name, " at ",  Sys.time(), ".\n To recreate, make sure: set.seed(", seed, ")"), file = log_con)
  close(log_con)
  
  ## ~~~~~~~~~~~~~~~~~~~~ ##
  ## Fit P/A/ VSURF model ##
  ## ~~~~~~~~~~~~~~~~~~~~ ##
  pa_y <- pa_data[, "PRESENCE"]
  pa_x <- pa_data[, !colnames(pa_data) %in% c("LAT", "LON", "BIOMASS",
                                              "PRESENCE", "ABUNDANCE", 
                                              "name", "SVSPP", "YEAR")]
  ## A few columns are all zeros... might as well remove columns with identical values, too
  pa_x <- Filter(var, pa_x)
  
  log_con <- file(log_name, open = "a")
  cat(paste0("\n\nFirst we'll fit the occupancy model with ",  nrow(pa_x),
             " observations and ", ncol(pa_x), " explanatory variables.\n"), file = log_con)
  close(log_con)
  