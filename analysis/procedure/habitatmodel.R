rm(list = ls())
seed <- 627
set.seed(seed)

raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"

# Using Data Miner with R
source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")

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
  
  # Get the most recent file
  habmod_file <- habmod_file[file.info(habmod_file)$mtime == tail(file.info(habmod_file)$mtime, 1)]
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
  zoo_static_dat <- grep("zoo_spr_clim_|zoo_fal_clim_", colnames(season.data), value = TRUE)

  # Get rid of all the unwanted columns
  season_dat <- season.data %>%
    dplyr::select(-TOW,
                  -CATCHSEX,
                  -dplyr::one_of(lag_dat),
                  -dplyr::one_of(zoo_static_dat),
                  -dplyr::one_of(bad_dat)) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # Create a data.frame of just the station data for merging to make biomass per species by station
  station_dat <- season_dat %>%
    dplyr::select(-SVSPP,
                  -ABUNDANCE,
                  -BIOMASS) %>%
    dplyr::distinct(.keep_all = TRUE) 
  
  # Quick function to filter by species and join with station data, adding zero biomass and abundance if NA
  extend_data <- function(svspp) {
    season_dat %>%
      filter(SVSPP == svspp) %>% 
      right_join(station_dat) %>% 
      mutate(BIOMASS = ifelse(is.na(BIOMASS),
                              0, 
                              BIOMASS),
             ABUNDANCE = ifelse(is.na(ABUNDANCE),
                                0,
                                ABUNDANCE),
             SVSPP = svspp)
  }
  
  # Make the big data 
  all_dat <- suppressMessages(bind_rows(species_list %>% purrr::map(., extend_data)))
  rm(list = c("season.data"))

  # Make data modifications (PA, log10(BIOMASS))
  all_dat_op <- all_dat %>%  
    dplyr::left_join(svspp_dat, by = "SVSPP") %>%
    dplyr::mutate(PRESENCE = ifelse(BIOMASS == 0,
                                    0,
                                    1),
      BIOMASS = log10(as.numeric(BIOMASS) + 1),
           SVSPP = as.numeric(SVSPP),
           name = as.character(gsub(" ", "_", COMNAME))) %>% 
    dplyr::select(-dplyr::one_of(join_names),
                  -COMNAME,
                  SVSPP, LON, LAT, YEAR) %>%
    as.data.frame

  log_file <- paste(gsub("-", "", Sys.Date()), "-biomass_habmod-", season,".log", sep="")
  log_name <- paste0(derived_path, season, "/", log_file)
  saveRDS(all_dat_op, 
          file = paste0(raw_path, gsub(".log", ".rds", log_file)))
}

sp.list <- c(22, 28, 73, 74, 105, 107, 141)

sp.i <- 73
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

  ## VSURF is a data-driven approach to select important variables from high dimensional datasets
  vsurf_url <- vsurf_bb(file_x = pa_x,
                        file_y = pa_y,
                        ntree = 500,
                        mtry =  floor(sqrt(ncol(pa_x))),
                        nfor_thres = 50,
                        nmin = 5,
                        nfor_interp = 25,
                        nsd = 1,
                        nfor_pred = 25,
                        parallel = "true",
                        ncores = 20,
                        clusterType = "FORK",
                        seed = seed,
                        name = paste0(name, "_PA"),
                        save_template = FALSE,
                        username = username,
                        token = token)

  stop_condition_fail <- stop_condition_success <- FALSE

  sleepy <- ifelse(length(pa_y) <= 600,
                   length(pa_y), 600)

  log_con <- file(log_name, open = "a")
  cat(paste0("Sent to BB and will monitor: ", vsurf_url,
             " \nevery ", round(sleepy/60, 2), " minutes.\n"), file = log_con)
  close(log_con)

  ## CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION ##
  while (!stop_condition_fail && !stop_condition_success){

    log_con <- file(log_name, open = "a")
    cat(paste0("Checking in at ", Sys.time(), ".\n"), file = log_con)
    close(log_con)

    ## CHECK THE STATUS URL ##
    out1 <- GET(url = vsurf_url, config = c(authenticate(username, token, type = "basic")), handle = NULL, timeout(3600))
    # cat(as.character(content(out1, encoding = "UTF-8")))
    stop_condition_success <- grepl("ProcessSucceeded", as.character(content(out1, encoding = "UTF-8")))
    stop_condition_fail <- grepl("Exception", as.character(content(out1, encoding = "UTF-8")))

    ## SLEEP FOR 10 MINUTES BEFORE THE NEXT CHECK ##
    Sys.sleep(sleepy)
  }

  if(stop_condition_success) {
    output_url <- xml2::xml_text(xml2::xml_find_all(xml2::read_xml(out1), ".//d4science:Data"))
    output_df <- data.frame(NAME = paste0(name, "_PA"),
                            LOG = output_url[1],
                            OUTPUT = output_url[2],
                            stringsAsFactors = FALSE)

    stop_condition_success <- FALSE

    log_con <- file(log_name, open = "a")
    cat("Success! ", name, "is all finished and output files can be found: ", output_url, "\n\n", file = log_con)
    close(log_con)
  }
  if(stop_condition_fail) {
    log_con <- file(log_name, open = "a")
    cat("Shoot! ", name, " failed.", "\n\n", file = log_con)
    close(log_con)
    next
  }

  download.file(output_df$LOG,
                paste0(derived_path, season, "/", name, "-PA-VSURFlog.txt"),
                mode = "wb",
                cacheOK = FALSE)
  download.file(output_df$OUTPUT,
                paste0(derived_path, season, "/", name, "-PA-VSURFoutput.rds"),
                mode = "wb",
                cacheOK = FALSE)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Extract important variables ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  temp_vsurf <- readRDS(paste0(derived_path, season, "/", name, "-PA-VSURFoutput.rds"))
  pa_rf_vars <- names(pa_x)[temp_vsurf$varselect.pred]

  log_con <- file(log_name, open = "a")
  cat("P/A VSURF model identified ", pa_rf_vars, " as the best variables for prediction.\n", file = log_con)
  close(log_con)

  pa_rf <- pa_data[, c("PRESENCE", pa_rf_vars)]
  pa_rf <- Filter(var, pa_rf)
  pa_rf[, "PRESENCE"] <- as.factor(pa_rf[, "PRESENCE"])
  pa_rf[, "PRESENCE"] <- dplyr::recode_factor(pa_rf[, "PRESENCE"], `0` = "ABSENT", `1` = "PRESENT")

  ## ~~~~~~~~~~~~~~~~ ##
  ## Fit P/A RF model ##
  ## ~~~~~~~~~~~~~~~~ ##
  #Randomly select 75% of the data to enable 10 fold cross validation
  selection <- caret::createDataPartition(y = pa_rf[, "PRESENCE"],
                                   p = 0.75,
                                   list = FALSE)

  pa_rf_train <- pa_rf[selection, ]
  pa_rf_test <- pa_rf[-selection, ]

  trainX <- pa_rf_train[, names(pa_rf_train) %in% pa_rf_vars]
  trainY <- pa_rf_train[, "PRESENCE"]

  # create weights
  h <- ifelse(pa_rf_train[, "PRESENCE"] == "ABSENT", 1/(unname(table(pa_rf_train[, "PRESENCE"])["ABSENT"])/length(pa_rf_train[, "PRESENCE"])),
              ifelse(pa_rf_train[, "PRESENCE"] == "PRESENT", 1/(unname(table(pa_rf_train[, "PRESENCE"])["PRESENT"])/length(pa_rf_train[, "PRESENCE"])),
                     0))
  
  strt_time <- Sys.time()
  train_control <- caret::trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       number = 10,
                                       search = "random",
                                       classProbs = TRUE,
                                       summaryFunction = caret::twoClassSummary,
                                       allowParallel = TRUE,
                                       verboseIter = TRUE)

  cluster <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
  doParallel::registerDoParallel(cluster)

  wf_pa <- caret::train(x = trainX,
                        y = trainY,
                        method = "rf",
                        trControl = train_control,
                        weights = h,
                        keep.inbag = TRUE,
                        replace = TRUE,
                        importance = TRUE,
                        metric = "ROC",
                        preProc = c("center", "scale"))

  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()

  stp_time <- Sys.time()
  stp_time - strt_time

  ## ROC-Curve
  predRoc <- predict(wf_pa, pa_rf_test, type = "prob")
  myroc <- pROC::roc(pa_rf_test$PRESENCE, as.vector(predRoc[,2]))

  roc_plot <- pROC::ggroc(myroc) +
    ggplot2::geom_abline(intercept = 1, slope = 1, col = "grey70") +
    ggplot2::labs(title = gsub("_", " ", name),
         subtitle = paste("AUC =", sprintf("%.3f",myroc$auc))) +
    ggplot2::theme_bw() +
    ggplot2::coord_equal()

  ggplot2::ggsave(paste0("analysis/figures/", name, "-", season, "-", "roc.png"), plot = roc_plot)

  ##adjust optimal cut-off threshold for class probabilities
  threshold <- pROC::coords(myroc, x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  predCut <- factor( ifelse(predRoc[, "PRESENT"] > threshold, "PRESENT", "ABSENT") )

  curConfusionMatrix <- caret::confusionMatrix(predCut, pa_rf_test$PRESENCE, positive = "PRESENT")

  log_con <- file(log_name, open = "a")
  cat(paste0("PA rf completed.\n", capture.output(stp_time - strt_time),
             "\nAccuracy = ",
             round(curConfusionMatrix$overall[1], 2), ", Kappa = ",
             round(curConfusionMatrix$overall[2], 2), ", and threshold = ", threshold, ".\n"),
      file = log_con)
  close(log_con)

  rf_name <- paste0(name, "-PA-RFmodel")
  saveRDS(assign(value = wf_pa, x = rf_name), 
          paste0(derived_path, season, "/", rf_name, ".rds"))

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Split data for Biomass model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  # bm_data <- all_dat_op[all_dat_op$SVSPP == sp.i,]

  bm_data <- all_dat_op %>% 
    dplyr::filter(SVSPP == sp.i) %>% 
    na.omit
  
  # Add the probability of occurance to the Biomass data
  bm_data$PRESPROB <- predict(wf_pa, newdata = bm_data, type = "prob")[, 2]

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Fit BIOMASS VSURF model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~ ##

  bm_y <- bm_data[, "BIOMASS"]
  bm_x <- bm_data[, !colnames(bm_data) %in% c("LAT", "LON", "BIOMASS",
                                              "PRESENCE", "ABUNDANCE",
                                              "name", "SVSPP", "YEAR")]
  
  ## A few columns are all zeros... might as well remove columns with identical values, too
  bm_x <- Filter(var, bm_x)

  log_con <- file(log_name, open = "a")
  cat(paste0("Now we fit the biomass VSURF model with ",
             nrow(bm_x), " observations and ",
             ncol(bm_x), " explanatory variables.\n"),
      file = log_con)
  close(log_con)

  ## VSURF is a data-driven approach to select important variables from high dimensional datasets
  bm_url <- vsurf_bb(file_x = bm_x,
                     file_y = bm_y,
                     ntree = 500,
                     mtry = max(floor(ncol(bm_x)/3), 1),
                     nfor_thres = 50,
                     nmin = 5,
                     nfor_interp = 25,
                     nsd = 1,
                     nfor_pred = 25,
                     parallel = "true",
                     ncores = 20,
                     clusterType = "FORK",
                     seed = seed,
                     name = paste0(name, "_BM"),
                     save_template = FALSE,
                     username = username,
                     token = token)

  stop_condition_fail <- stop_condition_success <- FALSE

  sleepy <- ifelse(length(bm_y) <= 600,
                   length(bm_y), 600)

  log_con <- file(log_name, open = "a")
  cat(paste0("Sent to BB and will monitor: ", bm_url,
             " \nevery ", round(sleepy/60, 2), " minutes.\n"), file = log_con)
  close(log_con)

  ## CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION ##
  while (!stop_condition_fail && !stop_condition_success){

    log_con <- file(log_name, open = "a")
    cat(paste0("Checking in at ", Sys.time(), ".\n"), file = log_con)
    close(log_con)

    ## CHECK THE STATUS URL ##
    out1 <- GET(url = bm_url, config = c(authenticate(username, token, type = "basic")), handle = NULL, timeout(3600))
    # cat(as.character(content(out1, encoding = "UTF-8")))
    stop_condition_success <- grepl("ProcessSucceeded", as.character(content(out1, encoding = "UTF-8")))
    stop_condition_fail <- grepl("Exception", as.character(content(out1, encoding = "UTF-8")))

    ## SLEEP FOR 10 MINUTES BEFORE THE NEXT CHECK ##
    Sys.sleep(sleepy)
  }

  if(stop_condition_success) {
    output_bm_url <- xml2::xml_text(xml2::xml_find_all(xml2::read_xml(out1), ".//d4science:Data"))
    output_bm_df <- data.frame(NAME = paste0(name, "_BM"),
                               LOG = output_bm_url[1],
                               OUTPUT = output_bm_url[2],
                               stringsAsFactors = FALSE)

    stop_condition_success <- FALSE

    log_con <- file(log_name, open = "a")
    cat("Success! ", name, "is all finished and output files can be found: ", output_bm_url, "\n\n", file = log_con)
    close(log_con)
  }
  if(stop_condition_fail) {
    log_con <- file(log_name, open = "a")
    cat("Shoot! ", name, " failed.", "\n\n", file = log_con)
    close(log_con)
    next
  }

  download.file(output_bm_df$LOG,
                paste0("analysis/data/derived_data/", season, "/", name, "-BM-VSURFlog.txt"),
                mode = "wb",
                cacheOK = FALSE)
  download.file(output_bm_df$OUTPUT,
                paste0("analysis/data/derived_data/", season, "/", name, "-BM-VSURFoutput.rds"),
                mode = "wb",
                cacheOK = FALSE)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Extract important variables ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  temp_vsurf <- readRDS(paste0(derived_path, season, "/", name, "-BM-VSURFoutput.rds"))
  bm_rf_vars <- names(bm_x)[temp_vsurf$varselect.pred]

  log_con <- file(log_name, open = "a")
  cat("Biomass VSURF model identified ", bm_rf_vars, " as the best variables for prediction.\n", file = log_con)
  close(log_con)

  bm_rf <- bm_data[bm_data$BIOMASS != 0, c("BIOMASS", bm_rf_vars)]
  bm_rf <- Filter(var, bm_rf)

  #Randomly select 75% of the data to enable 10 fold cross validation
  bm_selection <- caret::createDataPartition(y = bm_rf[, "BIOMASS"],
                                      p = 0.75,
                                      list = FALSE)

  bm_train <- bm_rf[bm_selection,]
  bm_test<- bm_rf[-bm_selection,]

  bm_trainX <- bm_train[, names(bm_train) %in% bm_rf_vars]
  bm_trainY <- bm_train[, "BIOMASS"]

  strt_time <- Sys.time()
  train_control <- caret::trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       number = 10,
                                       search = "random",
                                       allowParallel = TRUE,
                                       verboseIter = TRUE)

  cluster <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cluster)

  rf_bm <- caret::train(x = bm_trainX,
                        y = bm_trainY,
                        method = "rf",
                        trControl = train_control,
                        importance = TRUE,
                        keep.inbag = TRUE,
                        replace = TRUE,
                        metric = "RMSE")

  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()

  stp_time <- Sys.time()
  stp_time - strt_time

  log_con <- file(log_name, open = "a")
  cat(paste0("Biomass rf completed.\n", capture.output(stp_time - strt_time),
             ".\nR2 = ",
             round(tail(rf_bm$finalModel$rsq, 1), 2), ", and RMSE = ",
             round(tail(sqrt(rf_bm$finalModel$mse), 1), 2), "\n"),
      file = log_con)
  close(log_con)

  rf_name <- paste0(name, "-BM-RFmodel")
  saveRDS(assign(value = rf_bm, x = rf_name), 
          paste0(derived_path, season, "/", rf_name, ".rds"))

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Fit BIOMASS VSURF model (w/o PA) ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  bo_data <- all_dat_op %>% 
    dplyr::filter(SVSPP == sp.i,
                  BIOMASS != 0) %>% 
    na.omit
  
  # bo_data <- all_dat_op[all_dat_op$SVSPP == sp.i &
  #                       all_dat_op$BIOMASS != 0, ]

  bo_y <- bo_data[, "BIOMASS"]
  bo_x <- bo_data[, !colnames(bo_data) %in% c("LAT", "LON", "BIOMASS",
                                              "PRESENCE", "ABUNDANCE",
                                              "name", "SVSPP", "YEAR")]
  ## A few columns are all zeros... might as well remove columns with identical values, too
  bo_x <- Filter(var, bo_x)

  log_con <- file(log_name, open = "a")
  cat(paste0("Now we fit the biomass VSURF model (w/o PA) with ",
             nrow(bo_x), " observations and ",
             ncol(bo_x), " explanatory variables."), "\n",
      file = log_con)
  close(log_con)

  ## VSURF is a data-driven approach to select important variables from high dimensional datasets
  bo_url <- vsurf_bb(file_x = bo_x,
                     file_y = bo_y,
                     ntree = 500,
                     mtry = max(floor(ncol(bo_x)/3), 1),
                     nfor_thres = 50,
                     nmin = 5,
                     nfor_interp = 25,
                     nsd = 1,
                     nfor_pred = 25,
                     parallel = "true",
                     ncores = 20,
                     clusterType = "FORK",
                     seed = seed,
                     name = paste0(name, "_BO"),
                     save_template = FALSE,
                     username = username,
                     token = token)

  stop_condition_fail <- stop_condition_success <- FALSE

  sleepy <- ifelse(length(bo_y) <= 600,
                   length(bo_y), 600)

  log_con <- file(log_name, open = "a")
  cat(paste0("Sent to BB and will monitor: ", bo_url,
             " \nevery ", round(sleepy/60, 2), " minutes.\n"), file = log_con)
  close(log_con)

  ## CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION ##
  while (!stop_condition_fail && !stop_condition_success){

    log_con <- file(log_name, open = "a")
    cat(paste0("Checking in at ", Sys.time(), ".\n"), file = log_con)
    close(log_con)

    ## CHECK THE STATUS URL ##
    out1 <- GET(url = bo_url, config = c(authenticate(username, token, type = "basic")), handle = NULL, timeout(3600))
    # cat(as.character(content(out1, encoding = "UTF-8")))
    stop_condition_success <- grepl("ProcessSucceeded", as.character(content(out1, encoding = "UTF-8")))
    stop_condition_fail <- grepl("Exception", as.character(content(out1, encoding = "UTF-8")))

    ## SLEEP FOR 10 MINUTES BEFORE THE NEXT CHECK ##
    Sys.sleep(sleepy)
  }

  if(stop_condition_success) {
    output_bo_url <- xml2::xml_text(xml2::xml_find_all(xml2::read_xml(out1), ".//d4science:Data"))
    output_bo_df <- data.frame(NAME = paste0(name, "_BM"),
                               LOG = output_bo_url[1],
                               OUTPUT = output_bo_url[2],
                               stringsAsFactors = FALSE)

    stop_condition_success <- FALSE

    log_con <- file(log_name, open = "a")
    cat("Success! ", name, "is all finished and output files can be found: ", output_bo_url, "\n\n", file = log_con)
    close(log_con)
  }
  if(stop_condition_fail) {
    log_con <- file(log_name, open = "a")
    cat("Shoot! ", name, " failed.", "\n\n", file = log_con)
    close(log_con)
    next
  }

  download.file(output_bo_df$LOG,
                paste0("analysis/data/derived_data/", season, "/", name,  "-BO-VSURFlog.txt"),
                mode = "wb",
                cacheOK = FALSE)
  download.file(output_bo_df$OUTPUT,
                paste0("analysis/data/derived_data/", season, "/", name,  "-BO-VSURFoutput.rds"),
                mode = "wb",
                cacheOK = FALSE)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Extract important variables ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  temp_vsurf <- readRDS(paste0(derived_path, season, "/", name, "-BO-VSURFoutput.rds"))
  bo_rf_vars <- names(bo_x)[temp_vsurf$varselect.pred]

  log_con <- file(log_name, open = "a")
  cat("Biomass VSURF (w/o PA) model identified ", bo_rf_vars, " as the best variables for prediction.\n", file = log_con)
  close(log_con)

  bo_rf <- bo_data[, c("BIOMASS", bo_rf_vars)]
  bo_rf <- Filter(var, bo_rf)

  #Randomly select 75% of the data to enable 10 fold cross validation
  bo_selection <- caret::createDataPartition(y = bo_rf[, "BIOMASS"],
                                             p = 0.75,
                                             list = FALSE)

  bo_train <- bo_rf[bo_selection,]
  bo_test<- bo_rf[-bo_selection,]

  bo_trainX <- bo_train[, names(bo_train) %in% bo_rf_vars]
  bo_trainY <- bo_train[, "BIOMASS"]

  strt_time <- Sys.time()
  train_control <- caret::trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       number = 10,
                                       search = "random",
                                       allowParallel = TRUE,
                                       verboseIter = TRUE)

  cluster <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cluster)

  rf_bo <- caret::train(x = bo_trainX,
                        y = bo_trainY,
                        method = "rf",
                        trControl = train_control,
                        importance = TRUE,
                        keep.inbag = TRUE,
                        replace = TRUE)

  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()

  stp_time <- Sys.time()
  stp_time - strt_time

  log_con <- file(log_name, open = "a")
  cat(paste0("Biomass rf completed.\n", capture.output(stp_time - strt_time), "\nR2 = ",
             round(tail(rf_bo$finalModel$rsq, 1), 2), ", and RMSE = ",
             round(tail(sqrt(rf_bo$finalModel$mse), 1), 2), "\n\n~~~~~~~~~~~~~~~~~~\n"),
      file = log_con)
  close(log_con)

  rf_name <- paste0(name, "-BO-RFmodel")
  saveRDS(assign(value = rf_bo, x = rf_name),
          paste0(derived_path, season, "/", rf_name, ".rds"))
}
