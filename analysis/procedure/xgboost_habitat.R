## In lieu of rm(list = ls()), please restart R to clean your R environment. in RStudio, ctrl+shift+F10 ##

seed <- 627
set.seed(seed)

library(Matrix)
library(xgboost)
library(dplyr)
library(rBayesianOptimization)
library(ggplot2)

raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"

## ~~~~~~~~~~~~~~~~~ ##
#### Load the data ####
## ~~~~~~~~~~~~~~~~~ ##

# sp.list <- c(22, 28, 73, 74, 105, 107, 141)
# lapply(sp.list, run_xgboost_model, season = "fall")

run_xgboost_model <- function(season, SVSPP){
  
  svspp_dat <- read.csv(paste0(raw_path, "SVSPP.csv"),
                        stringsAsFactors = FALSE)
  
  svspp_dat <- svspp_dat %>%
    dplyr::mutate(COMNAME = tolower(COMNAME)) %>%
    dplyr::select(COMNAME, SVSPP) %>%
    dplyr::mutate(COMNAME = gsub("atlantic", "Atlantic", COMNAME),
                  COMNAME = gsub("american", "American", COMNAME),
                  COMNAME = gsub("acadian", "Acadian", COMNAME)) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  ## Short list -- will need to be updated ##
  species_list <- c(101, 102, 103, 104, 105, 
                    106, 107, 108, 109, 112, 
                    121, 13, 131, 135, 139, 
                    14, 141, 143, 145, 15, 
                    151, 155, 156, 163, 164, 
                    168, 171, 172, 176, 177, 
                    22, 23, 24, 25, 26, 
                    27, 28, 32, 33, 34, 
                    35, 36, 69, 72, 73, 
                    74, 75, 76, 77, 78, 84)
  
  if(!SVSPP %in% species_list){
    stop("SVSPP not in the available species list")
  }
  
  join_names <- c("CRUISE6", "STRATUM", "STATION", "SVVESSEL", "YEAR", "SEASON", "LAT",
                  "LON", "EST_TOWDATE", "DEPTH", "DOY", "SVSPP")
  
  bad_dat <- c("rast_necrm_bpi", "rast_necrm_vrm" ,
               "rast_bpi_3_25_layer", "rast_bpi_30_250_layer",
               "rast_mab_sed", "rast_gdepth",
               "rast_gravel_fraction", "rast_mud_fraction",
               "rast_phi_fraction", "rast_sand_fraction", 
               "rast_plcurv20km", "rast_plcurv2km",
               "rast_plcurv10km", "SURFTEMP", "BOTTEMP")
  
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
    
    ## Get rid of all the unwanted columns
    season_dat <- season.data %>%
      dplyr::select(-TOW,
                    -CATCHSEX,
                    -dplyr::one_of(lag_dat),
                    -dplyr::one_of(zoo_static_dat),
                    -dplyr::one_of(bad_dat)) %>%
      dplyr::distinct(.keep_all = TRUE)
    
    ## Create a data.frame of just the station data for merging to make biomass per species by station
    station_dat <- season_dat %>%
      dplyr::select(-SVSPP,
                    -ABUNDANCE,
                    -BIOMASS) %>%
      dplyr::distinct(.keep_all = TRUE) 
    
    ## Quick function to filter by species and join with station data, adding zero biomass and abundance if NA
    extend_data <- function(svspp) {
      season_dat %>%
        filter(SVSPP == svspp) %>% 
        right_join(station_dat) %>% 
        mutate(PRESENCE = ifelse(is.na(BIOMASS),
                                 0,
                                 1),
               BIOMASS = ifelse(is.na(BIOMASS),
                                NA,
                                BIOMASS),
               ABUNDANCE = ifelse(is.na(ABUNDANCE),
                                  0,
                                  ABUNDANCE),
               SVSPP = svspp)
    }
    
    ## Make the big data 
    all_dat <- suppressMessages(bind_rows(species_list %>% purrr::map(., extend_data)))
    rm(list = c("season.data"))
    
    ## Make data modifications (PA, log10(BIOMASS))
    all_dat_op <- all_dat %>%  
      dplyr::left_join(svspp_dat, by = "SVSPP") %>%
      dplyr::mutate(BIOMASS = log10(as.numeric(BIOMASS) + 1),
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
  
  ## ~~~~~~~~~ ##
  ## P/A model ##
  ## ~~~~~~~~~ ##
  
  pa_data <- all_dat_op %>% 
    dplyr::filter(SVSPP == rlang::UQ(SVSPP))
  name <- unique(pa_data$name)
  keepers <- colnames(pa_data)[!colnames(pa_data) %in% c("YEAR", "name", "SVSPP", "BIOMASS", "ABUNDANCE", 
                                                         "PRESENCE", "LON", "LAT")]
  
  ## P/A data
  pa_rf <- pa_data[, c("PRESENCE", keepers)]
  
  pa_selection <- caret::createDataPartition(y = pa_rf[, "PRESENCE"],
                                             p = 0.75,
                                             list = FALSE)
  
  pa_train <- xgboost::xgb.DMatrix(data = as.matrix(pa_rf[pa_selection, -1]),
                          label = pa_rf[pa_selection, 1], 
                          missing = NA)
  
  pa_test <- xgboost::xgb.DMatrix(data = as.matrix(pa_rf[-pa_selection, -1]),
                         label = pa_rf[-pa_selection, 1], 
                         missing = NA)
  
  xgb.cv.bayes.logistic <- function(max_depth, subsample, colsample_bytree, eta){
    
    cv <- xgboost::xgb.cv(params = list(booster = 'gbtree',
                                        eta = eta,
                                        max_depth = max_depth,
                                        subsample = subsample,
                                        colsample_bytree = colsample_bytree,
                                        lambda = 1,
                                        alpha = 0,
                                        objective = 'binary:logistic',
                                        eval_metric = 'auc',
                                        nthread = parallel::detectCores()-1),
                          data = data.matrix(pa_df_train[,-target.var]),
                          label = as.matrix(pa_df_train[, target.var]),
                          nround = 1200, # Keep this smaller to be more efficient in finding best hyperparameters
                          folds = pa_cv_folds, 
                          watchlist =  list(train = pa_train,
                                            test = pa_test),
                          prediction = FALSE,
                          showsd = TRUE, 
                          early_stop_round = 5, 
                          maximize = TRUE,
                          verbose = 0
    )
    list(Score = max(cv$evaluation_log$test_auc_mean), #cv$evaluation_log$test_auc_mean[cv$best_iteration]
         Pred = 0)
  }
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Tune the hyperparameters ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  target.var <- 1
  pa_df_train <- pa_rf[pa_selection, ]
  
  pa_cv_folds <- rBayesianOptimization::KFold(as.matrix(pa_df_train[, target.var]),
                                              nfolds = 5, 
                                              stratified = TRUE, 
                                              seed = seed)
  
  pa_xgb_bayes <- rBayesianOptimization::BayesianOptimization(
    xgb.cv.bayes.logistic,
    bounds = list(eta = c(0.01, 0.3),
                  max_depth = c(2L, 8L),
                  subsample = c(0.5, 1),
                  colsample_bytree = c(0.1, 0.4)
    ),
    init_grid_dt = NULL,
    init_points = 20,  # number of random points to start search
    n_iter = 30, # number of iterations after initial random points are set
    acq = 'ucb', kappa = 2.576, eps = 0.0, verbose = TRUE
  )
  
  pa_params <- list(
    objective = "binary:logistic",
    seed = seed,
    eta = pa_xgb_bayes$Best_Par[["eta"]],
    max.depth = pa_xgb_bayes$Best_Par[["max_depth"]],
    subsample = pa_xgb_bayes$Best_Par[["subsample"]],
    colsample_bytree = pa_xgb_bayes$Best_Par[["colsample_bytree"]],
    eval_metric = "auc",
    nthread = parallel::detectCores()-1)
  
  ## ~~~~~~~~~~~~~~ ##
  ## Tune the model ##
  ## ~~~~~~~~~~~~~~ ##
  pa_watchlist <- list(train = pa_train,
                       test = pa_test)
  
  bst_plain <- xgb.cv(data = pa_train,
                      watchlist = pa_watchlist,
                      params = pa_params, 
                      maximize = TRUE,
                      nrounds = 1200, # Keep this large to find best model
                      folds = pa_cv_folds,
                      print_every_n = 10,
                      early_stop_round = 5)
  
  ## Identify the index the model that maximizes AUC
  pa_bst_idx <- bst_plain$evaluation_log$iter[bst_plain$evaluation_log$test_auc_mean == max(bst_plain$evaluation_log$test_auc_mean)]
  
  pa_bst <-  xgboost(data = pa_train,
                     params = pa_params, 
                     maximize = TRUE,
                     nrounds = pa_bst_idx, 
                     verbose = 0) 
  
  saveRDS(pa_bst, file = file.path(derived_path, season, paste0(name,"-", season, "-PA_bst.rds")))
  
  pa_names = colnames(pa_rf[, -1])
  
  ## ~~~~~~~~~~~~~ ##
  ## BIOMASS MODEL ##
  ## ~~~~~~~~~~~~~ ##
  bm_rf <- pa_data[, c("BIOMASS", keepers)]
  
  # Add the probability of occurance to the Biomass data
  bm_rf$PRESPROB <- predict(pa_bst, newdata = data.matrix(bm_rf[, -1]))
  bm_rf <- bm_rf[!is.na(bm_rf$BIOMASS),]
  
  bm_selection <- caret::createDataPartition(y = bm_rf[, "BIOMASS"],
                                             p = 0.75,
                                             list = FALSE)
  
  bm_train <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[bm_selection, -1]),
                                   label = bm_rf[bm_selection, 1], 
                                   missing = NA)
  
  bm_test <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[-bm_selection, -1]),
                                  label = bm_rf[-bm_selection, 1], 
                                  missing = NA)
  
  
  xgb.cv.bayes.linear <- function(max_depth, subsample, colsample_bytree, eta){
    
    cv <- xgboost::xgb.cv(params = list(booster = 'gbtree',
                                        eta = eta,
                                        max_depth = max_depth,
                                        subsample = subsample,
                                        colsample_bytree = colsample_bytree,
                                        lambda = 1,
                                        alpha = 0,
                                        objective = 'reg:linear',
                                        eval_metric = 'rmse',
                                        nthread = parallel::detectCores()-1),
                          data = data.matrix(bm_df_train[,-target.var]),
                          label = as.matrix(bm_df_train[, target.var]),
                          nround = 1200, # Keep this smaller to be more efficient in finding best hyperparameters
                          watchlist = list(train = bm_train,
                                           test = bm_test),
                          folds = bm_cv_folds,
                          prediction = FALSE,
                          showsd = TRUE, 
                          early_stop_round = 5, 
                          maximize = TRUE,
                          verbose = 0
    )
    list(Score = -min(cv$evaluation_log$test_rmse_mean), #cv$evaluation_log$test_auc_mean[cv$best_iteration]
         Pred = 0)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Tune the hyperparameters ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  target.var <- 1
  bm_df_train <- bm_rf[bm_selection, ]
  
  bm_cv_folds <- rBayesianOptimization::KFold(as.matrix(bm_df_train[, target.var]),
                                              nfolds = 5,
                                              stratified = FALSE,
                                              seed = seed)
  strt_time <- Sys.time()
  bm_xgb_bayes <- BayesianOptimization(
    xgb.cv.bayes.linear,
    bounds = list(eta = c(0.01, 0.3),
                  max_depth = c(2L, 8L),
                  subsample = c(0.5, 1),
                  colsample_bytree = c(0.1, 0.4)
    ),
    init_grid_dt = NULL,
    init_points = 20,  # number of random points to start search
    n_iter = 30, # number of iterations after initial random points are set
    acq = 'ucb', kappa = 2.576, eps = 0.0, verbose = TRUE
  )
  
  stp_time <- Sys.time()
  stp_time - strt_time
  
  bm_params <- list(booster = 'gbtree',
                    eta = bm_xgb_bayes$Best_Par[["eta"]],
                    max_depth = bm_xgb_bayes$Best_Par[["max_depth"]],
                    subsample =  bm_xgb_bayes$Best_Par[["subsample"]],
                    colsample_bytree =  bm_xgb_bayes$Best_Par[["colsample_bytree"]],
                    lambda = 1,
                    alpha = 0,
                    objective = 'reg:linear',
                    eval_metric = 'rmse',
                    nthread = parallel::detectCores()-1)
  
  ## ~~~~~~~~~~~~~~ ##
  ## Tune the model ##
  ## ~~~~~~~~~~~~~~ ##
  
  bm_bst_plain <- xgboost::xgb.cv(params = bm_params,
                                  data = bm_train,
                                  nround = 1200, # Keep this smaller to be more efficient in finding best hyperparameters
                                  watchlist = list(train = bm_train,
                                                   test = bm_test),
                                  folds = bm_cv_folds,
                                  prediction = FALSE,
                                  missing = NA,
                                  seed = seed,
                                  showsd = TRUE, 
                                  early_stop_round = 5, 
                                  print_every_n = 10,
                                  maximize = FALSE,
                                  verbose = 1
  )
  
  ## Identify the index of that maximizes AUC
  bm_bst_idx <- bm_bst_plain$evaluation_log$iter[bm_bst_plain$evaluation_log$test_rmse_mean == min(bm_bst_plain$evaluation_log$test_rmse_mean)]
  
  bm_bst <-  xgboost(data = bm_train,
                     params = bm_params, 
                     maximize = FALSE,
                     nrounds = bm_bst_idx,
                     verbose = 0) 
  
  bm_names <- colnames(bm_rf[, -1])
  
  saveRDS(bm_bst, file = file.path(derived_path, season, paste0(name,"-", season, "-BM_bst.rds")))
  
  final_dat <- list(data = pa_data,
                    pa_names =  pa_names,
                    bm_selection = bm_selection,
                    bm_names = bm_names,
                    pa_selection = pa_selection)
  saveRDS(final_dat, file = file.path(derived_path, season, paste0(name,"-", season, "-dat.rds")))
  
} ## Close function


## Plots to explore residual patterns

  ## All 
  bm_all <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[, -1]),
                                 label = bm_rf[, 1], 
                                 missing = NA)
  
  
  pred_all <- predict(bm_bst, bm_all)
  bm_rf_all <- bm_rf
  bm_rf_all$COLOR <- "train"
  bm_rf_all$COLOR[-bm_selection] <- "test"
  bm_rf_all$PREDICTION <- NA
  bm_rf_all$PREDICTION <- pred_all
  
  bm_rf_all$RESID <- NA
  bm_rf_all$RESID <- bm_rf_all$BIOMASS - bm_rf_all$PREDICTION

  RMSE_all <- sqrt(sum((bm_rf_all$BIOMASS - bm_rf_all$PREDICTION)^2)/length(bm_rf_all$PREDICTION))
  rRMSE_all <- round(RMSE_all/(max(bm_rf_all$BIOMASS) - min(bm_rf_all$BIOMASS)), 2) #normalized
  
  ## test
  pred_test <- predict(bm_bst, bm_test)
  bm_rf_test <- bm_rf[-bm_selection, ]
  bm_rf_test$COLOR <- "test"
  bm_rf_test$PREDICTION <- NA
  bm_rf_test$PREDICTION <- pred_test

  bm_rf_test$RESID <- NA
  bm_rf_test$RESID <- bm_rf_test$BIOMASS - bm_rf_test$PREDICTION
  
  RMSE_test <- sqrt(sum((bm_rf_test$BIOMASS - bm_rf_test$PREDICTION)^2)/length(bm_rf_test$PREDICTION))
  rRMSE_test <- round(RMSE_test/(max(bm_rf_test$BIOMASS) - min(bm_rf_test$BIOMASS)), 2) #normalized
  
  ## train
  pred_train <- predict(bm_bst, bm_train)
  bm_rf_train <- bm_rf[bm_selection, ]
  bm_rf_train$COLOR <- "train"
  bm_rf_train$PREDICTION <- NA
  bm_rf_train$PREDICTION <- pred_train
  
  bm_rf_train$RESID <- NA
  bm_rf_train$RESID <- bm_rf_train$BIOMASS - bm_rf_train$PREDICTION
  
  RMSE_train <- sqrt(sum((bm_rf_train$BIOMASS - bm_rf_train$PREDICTION)^2)/length(bm_rf_train$PREDICTION))
  rRMSE_train <- round(RMSE_train/(max(bm_rf_train$BIOMASS) - min(bm_rf_train$BIOMASS)), 2) #normalized
  
  df <- bind_rows(bm_rf_all %>% 
                    dplyr::select(BIOMASS,
                                  PREDICTION,
                                  RESID,
                                  COLOR) %>% 
                    mutate(TYPE = "all"),
                  bm_rf_train %>% 
                    dplyr::select(BIOMASS,
                                  PREDICTION,
                                  RESID,
                                  COLOR) %>% 
                    mutate(TYPE = "train"),
                  bm_rf_test %>% 
                    dplyr::select(BIOMASS,
                                  PREDICTION,
                                  RESID,
                                  COLOR) %>% 
                    mutate(TYPE = "test"))
                  
  ## Predicted/observed plot
  p1 <- ggplot(df, aes(x = BIOMASS, y = PREDICTION, color = COLOR))
  p1 <- p1 +
    geom_point(alpha = 0.2, shape = 16) +
    geom_abline(intercept=0, slope = 1, linetype = 2) +
    labs(y = expression(Model~predicted~biomass~(log[10])),
         x = expression(Observed~biomass~(log[10]))) +
    scale_color_brewer(palette = "Set1")+
    coord_equal() +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~TYPE)
  p1
  
  ggsave(filename = "analysis/figures/Atlantic_cod-missing_as_zero.png", plot = p1, width = 8, height = 4, dpi = 144)

  #title = sprintf("%s (%s)", gsub("_", " ", name), season),
  #subtitle = paste0("The error (relative RMSE) is about ", rRMSE_test * 100, "% \nas large as the mean biomass.")) +
  
  ggplot(df, aes(x = PREDICTION, y = RESID, color = COLOR)) +
    geom_hline(yintercept = 0) +
    geom_point(alpha = 0.2, shape = 16) +
    labs(x = "Predicted value",
         y = "Residual error") +
    scale_color_brewer(palette = "Set1")+
    coord_equal() +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~TYPE)
