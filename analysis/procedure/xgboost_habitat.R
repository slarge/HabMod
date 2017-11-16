rm(list = ls())
seed <- 627
set.seed(seed)

## Installs the development xgboost ##
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(Matrix)
library(xgboost)
library(dplyr)
library(rBayesianOptimization)
# devtools::install_github("AppliedDataSciencePartners/xgboostExplainer")
# library(xgboostExplainer)

raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"

# Using Data Miner with R
# source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")

#SETTING USERNAME AND TOKEN - NOT NEEDED WHEN USING RSTUDIO ON THE PORTAL
# if(file.exists(paste0(raw_path, "keys.csv"))){
#   keys <- read.csv("analysis/data/raw_data/keys.csv", stringsAsFactors = FALSE)
#   username <<- keys$username
#   token <<- keys$token
#   rm(keys)
# }
# 
# if(!file.exists(paste0(raw_path, "keys.csv"))) {
#   cat("To use the vsurf_bb functionality, go to: https://i-marine.d4science.org/group/stockassessment \nand enter your username and personal token")
#   set_keys(save_key = TRUE)
# }

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
# for(sp.i in sp.list) {
  
  ## ~~~~~~~~~ ##
  ## P/A model ##
  ## ~~~~~~~~~ ##

  pa_data <- all_dat_op %>% 
    dplyr::filter(SVSPP == sp.i)
  name <- unique(pa_data$name)
  keepers <- colnames(pa_data)[!colnames(pa_data) %in% c("YEAR", "name", "SVSPP", "BIOMASS", "ABUNDANCE", "PRESENCE")]
    
  ## P/A data
  pa_rf <- pa_data[, c("PRESENCE", keepers)]
  
  
  ## set the seed to make your partition reproductible
  pa_selection <- caret::createDataPartition(y = pa_rf[, "PRESENCE"],
                                          p = 0.75,
                                          list = FALSE)
  
  pa_train <- xgb.DMatrix(data = as.matrix(pa_rf[pa_selection, -1]),
                       label = pa_rf[pa_selection, 1], 
                       missing = NA)
  
  pa_test <- xgb.DMatrix(data = as.matrix(pa_rf[-pa_selection, -1]),
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
                          nround = 500, # Keep this smaller to be more efficient in finding best hyperparameters
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

  target.var <- 1
  pa_df_train <- pa_rf[pa_selection, ]
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Tune the hyperparameters ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pa_cv_folds <- rBayesianOptimization::KFold(as.matrix(pa_df_train[, target.var]),
                                              nfolds = 5, 
                                              stratified = TRUE, 
                                              seed = seed)
  
  pa_xgb_bayes <- rBayesianOptimization::BayesianOptimization(
    xgb.cv.bayes.logistic,
    bounds = list(eta = c(0.01, 0.3),
                  max_depth = c(2L, 12L),
                  subsample = c(0.5, 1),
                  colsample_bytree = c(0.1, 0.4)
    ),
    init_grid_dt = NULL,
    init_points = 10,  # number of random points to start search
    n_iter = 20, # number of iterations after initial random points are set
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
                      nrounds = 500, # Keep this large to find best model
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
  
  xgb.save(pa_bst, fname = file.path(derived_path, season, paste0(name, "-", season, "-pa_bst")))
  pa_bst <- xgb.load(file.path(derived_path, season, paste0(name, "-", season, "-pa_bst")))
  
  predROC <- predict(pa_bst, pa_test)
  # pa_test <- xgb.DMatrix(data = as.matrix(pa_rf[-pa_selection, -1]),
  #                        label = pa_rf[-pa_selection, 1], 
  #                        missing = NA)
  # 
  
  myroc <- pROC::roc(pa_rf[-pa_selection, ]$PRESENCE, as.vector(predROC))
  # 
  roc_plot <- pROC::ggroc(myroc) +
    ggplot2::geom_abline(intercept = 1, slope = 1, col = "grey70") +
    ggplot2::labs(title = paste0(gsub("_", " ", name), " (", season, ")"),
                  subtitle = paste("AUC =", sprintf("%.3f",myroc$auc))) +
    ggplot2::theme_bw() +
    ggplot2::coord_equal()
  
  # 
  ##adjust optimal cut-off threshold for class probabilities
  threshold <- pROC::coords(myroc, x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  predCut <- factor( ifelse(predROC > threshold, 1, 0) )
  
  curConfusionMatrix <- caret::confusionMatrix(predCut, pa_rf[-pa_selection, ]$PRESENCE, positive = "1")
  
  ## ~~~~~~~~~~~~~ ##
  ## BIOMASS MODEL ##
  ## ~~~~~~~~~~~~~ ##

  bm_rf <- pa_data[, c("BIOMASS", keepers)]
  
  # Add the probability of occurance to the Biomass data
  bm_rf$PRESPROB <- predict(pa_bst, newdata = data.matrix(bm_rf[, -1]))
  bm_rf$PRESPROB <- ifelse(bm_rf$PRESPROB > threshold, 1, 0)

  bm_selection <- caret::createDataPartition(y = bm_rf[, "BIOMASS"],
                                             p = 0.75,
                                             list = FALSE)

  bm_train <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[bm_selection, -1]),
                          label = bm_rf[bm_selection, 1], 
                          missing = NA)
  
  bm_test <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[-bm_selection, -1]),
                         label = bm_rf[-bm_selection, 1], 
                         missing = NA)
  
  target.var <- 1
  bm_df_train <- bm_rf[bm_selection, ]
  xgb.cv.bayes.linear <- function(max_depth, subsample, colsample_bytree, eta){#, tweedie_variance_power){
    
    cv <- xgboost::xgb.cv(params = list(booster = 'gbtree',
                                        eta = eta,
                                        max_depth = max_depth,
                                        subsample = subsample,
                                        colsample_bytree = colsample_bytree,
                                        lambda = 1,
                                        alpha = 0,
                                        obective = 'reg:tweedie',
                                        tweedie_variance_power = 1,
                                        # tweedie_variance_power = tweedie_variance_power,
                                        # objective = 'reg:linear',
                                        eval_metric = 'rmse',
                                        nthread = parallel::detectCores()-1),
                          data = data.matrix(bm_df_train[,-target.var]),
                          label = as.matrix(bm_df_train[, target.var]),
                          nround = 500, # Keep this smaller to be more efficient in finding best hyperparameters
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
  bm_cv_folds <- rBayesianOptimization::KFold(as.matrix(bm_df_train[, target.var]),
                                           nfolds = 5,
                                           stratified = FALSE,
                                           seed = seed)
  strt_time <- Sys.time()
  bm_xgb_bayes <- BayesianOptimization(
    xgb.cv.bayes.linear,
    bounds = list(eta = c(0.01, 0.3),
                  max_depth = c(2L, 12L),
                  subsample = c(0.5, 1),
                  colsample_bytree = c(0.1, 0.4)#,
                  # tweedie_variance_power = c(1, 1.6)
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
                    tweedie_variance_power = 1,
                    # tweedie_variance_power = bm_xgb_bayes$Best_Par[["tweedie_variance_power"]],
                    lambda = 1,
                    alpha = 0,
                    objective = 'reg:tweedie',
                    eval_metric = 'rmse',
                    nthread = parallel::detectCores()-1)
  
  # bm_params <- list(booster = 'gbtree',
  #                   # eta = bm_xgb_bayes$Best_Par[["eta"]],
  #                   # max_depth = bm_xgb_bayes$Best_Par[["max_depth"]],
  #                   # subsample =  bm_xgb_bayes$Best_Par[["subsample"]],
  #                   # colsample_bytree =  bm_xgb_bayes$Best_Par[["colsample_bytree"]],
  #                   # lambda = 1,
  #                   # alpha = 0,
  #                   objective = 'reg:linear',
  #                   eval_metric = 'rmse',
  #                   nthread = parallel::detectCores()-1)
  ## ~~~~~~~~~~~~~~ ##
  ## Tune the model ##
  ## ~~~~~~~~~~~~~~ ##
  
  bm_bst_plain <- xgboost::xgb.cv(params = bm_params,
                                  data = bm_train,
                                  nround = 500, # Keep this smaller to be more efficient in finding best hyperparameters
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
  
  pred <- predict(bm_bst, bm_test)
  
  bm_all <- xgboost::xgb.DMatrix(data = as.matrix(bm_rf[, -1]),
                                   label = bm_rf[, 1], 
                                   missing = NA)
  
  # pred <- predict(bm_bst, bm_all)
  
  bm_rf$PREDICTION <- NA
  bm_rf[-bm_selection, ]$PREDICTION <- pred

  bm_rf$RESID <- NA
  bm_rf[-bm_selection, ]$RESID <- bm_rf[-bm_selection, ]$BIOMASS - bm_rf[-bm_selection, ]$PREDICTION
  #
  RMSE_test <- sqrt(sum((bm_rf[-bm_selection, ]$BIOMASS - bm_rf[-bm_selection, ]$PREDICTION)^2)/length(bm_rf[-bm_selection, ]$PREDICTION))
  rRMSE_test <- round(RMSE_test/(max(bm_rf[-bm_selection, ]$BIOMASS) - min(bm_rf[-bm_selection, ]$BIOMASS)), 2) #normalized
  
  df <- data.frame(BIOMASS = bm_rf[-bm_selection, ]$BIOMASS, PREDICTED = bm_rf[-bm_selection, ]$PREDICTION)

  # 
  # bm_rf$PREDICTION <- NA
  # bm_rf[, ]$PREDICTION <- pred
  # 
  # bm_rf$RESID <- NA
  # bm_rf[, ]$RESID <- bm_rf[, ]$BIOMASS - bm_rf[, ]$PREDICTION
  # #
  # RMSE_test <- sqrt(sum((bm_rf[, ]$BIOMASS - bm_rf[, ]$PREDICTION)^2)/length(bm_rf[, ]$PREDICTION))
  # rRMSE_test <- round(RMSE_test/(max(bm_rf[, ]$BIOMASS) - min(bm_rf[, ]$BIOMASS)), 2)
  
  # df <- data.frame(BIOMASS = bm_rf[, ]$BIOMASS, PREDICTED = bm_rf[, ]$PREDICTION)
  
  ## Predicted/observed plot
  p1 <- ggplot(df, aes(x = BIOMASS, y = PREDICTED))
  p1 <- p1 +
    # geom_linerange(aes(ymin = ci_lower,
    #                    ymax = ci_upper),
    #                color = "black", alpha = 0.2) +
    geom_point(color = "black", alpha = 0.8, shape = 16) +
    geom_abline(intercept=0, slope = 1, linetype = 2) +
    labs(x = "Observed biomass",
         y = "Predicted biomass",
         title = sprintf("%s (%s)", gsub("_", " ", name), season),
         subtitle = paste0("The error (relative RMSE) is about ", rRMSE_test * 100, "% \nas large as the mean biomass.")) +
    coord_equal() +
    theme_bw()
  p1
  # ggplot(bm_test, aes(x = PREDICTED, y = BIOMASS)) +
  #   geom_point() +
  #   geom_smooth(method='lm', formula = y ~ x, col = "grey40", se = FALSE) +
  #   geom_abline(slope = 1, intercept = 0, linetype = 2, col = "grey40") +
  #   theme_minimal() +
  #   labs(title = "",
  #        subtitle = paste0("RMSE = ", rf_rmse),
  #        x = expression(Model~predicted~biomass~(log[10])),
  #        y = expression(Observed~biomass~(log[10]))) +
  #   theme(aspect.ratio = 1)
  # 
  
  
  ggplot(bm_rf, aes(x = PREDICTION, y = RESID)) +
  # p1 <- p1 +
    # geom_linerange(aes(ymin = ci_lower,
    #                    ymax = ci_upper),
    #                color = "black", alpha = 0.2) +
    geom_point(color = "black", alpha = 0.8, shape = 16)
  
  
  xgb.save(bm_bst, fname = file.path(derived_data, season, paste0(name, "bm_bst") ))
  
  final_dat <- list(pa_data,
                    pa_colnames,
                    bm_selection,
                    bm_colnames,
                    pa_selection)
  
  
  # get the trained model
  model = xgb.dump(bst, with_stats=TRUE)
  # get the feature real names
  names = colnames(bm_rf[,-1])
  # compute feature importance matrix
  importance_matrix = xgb.importance(names, model=bm_bst)
  
  # plot
  gp = xgb.ggplot.importance(importance_matrix, top_n = 10)
   
  
  # params <- list(
  #   objective = "binary:logistic",
  #   seed = seed,
  #   eta = 0.01,
  #   max.depth = 3,
  #   eval_metric = "auc", 
  #   nthread = 11)
  # 
  # watchlist <- list(train = train,
  #                   test = test)
  
  # xgb_grid = expand.grid(
  #   nrounds = 1000, # Fixed value
  #   eta = c(0.3, 0.01, 0.001, 0.0001)[1], # Fine tune
  #   subsample = c(0.5, 0.75, 1), # Grid search
  #   colsample_bytree = c(.4, .6, .8, 1), # Grid search
  #   max_depth = c(2, 4, 6, 8, 10), # Grid search
  #   min_child_weight = c(1), # Fine tune
  #   gamma = 0 # Leave fixed
  # )
  # 
  # # pack the training control parameters
  # xgb_trcontrol_1 = trainControl(
  #   method = "repeatedcv",
  #   repeats = 1,
  #   number = 5,
  #   verboseIter = TRUE,
  #   returnData = FALSE,
  #   returnResamp = "all",                                                        # save losses across all models
  #   classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  #   summaryFunction = twoClassSummary,
  #   allowParallel = TRUE
  # )
  # 
  
  
  bst_plain <- xgboost(data = train,
                       # watchlist = watchlist, 
                       params = params, 
                       nrounds = 100,
                       print_every_n = 10,
                       early_stop_round = 10)
  
  bst_train <- xgb.train(data = train,
                         watchlist = watchlist, 
                         params = params, 
                         nrounds = 100,
                         print_every_n = 10,
                         early_stop_round = 10)
  
  
  bst_cv <- xgb.cv(data = train,
                   watchlist = watchlist, 
                   params = params, 
                   nrounds = 1000,
                   nfold = 5,            # number of folds in K-fold
                   prediction = TRUE,    # return the prediction using the final model 
                   showsd = TRUE,        # standard deviation of loss across folds
                   stratified = TRUE,    # sample is unbalanced; use stratified sampling
                   print_every_n = 10,
                   early_stop_round = 10)
  
  
  
  bst_cv$evaluation_log %>%
    dplyr::select(-contains("std")) %>%
    mutate(iter = 1:n()) %>%
    tidyr::gather(TestOrTrain, AUC, -iter) %>%
    ggplot(aes(x = iter, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
    geom_line() + 
    theme_bw()
  ## RF 
  # bst <- xgboost(data = train,
  #                max_depth = 4, num_parallel_tree = 1000,
  #                subsample = 0.5, colsample_bytree = 0.5, nrounds = 1, objective = "binary:logistic",
  #                nthread = 11)
  
  # train the model for each parameter combination in the grid, 
  #   using CV to evaluate
  xgb_train_1 = train(
    x = as.matrix(df_train %>%
                    select(-SeriousDlqin2yrs)),
    y = as.factor(df_train$SeriousDlqin2yrs),
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
  )
  

  pred <- predict(bst, newdata = test)
  prediction <- as.numeric(pred > 0.5)
  mean(as.numeric(pred > 0.5) != pa_rf[-selection, 1])
  importance_matrix <- xgb.importance(feature_names = colnames(train), model = bst)

  xgb.plot.importance(importance_matrix)
  xgb.plot.tree(bst)
  
  
  myroc <- pROC::roc(pa_rf[-selection, "PRESENCE"], as.vector(predRoc))
  
  roc_plot <- pROC::ggroc(myroc) +
    ggplot2::geom_abline(intercept = 1, slope = 1, col = "grey70") +
    ggplot2::labs(title = gsub("_", " ", name),
                  subtitle = paste("AUC =", sprintf("%.3f",myroc$auc))) +
    ggplot2::theme_bw() +
    ggplot2::coord_equal()
  
  ggplot2::ggsave(paste0("analysis/figures/", name, "-", season, "-", "roc.png"), plot = roc_plot)
  
  ##adjust optimal cut-off threshold for class probabilities
  threshold <- pROC::coords(myroc, x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  predCut <- factor( ifelse(predRoc > threshold, "PRESENT", "ABSENT") )
  pa_rf[-selection, "PRESENCE"] <- as.factor(pa_rf[-selection, "PRESENCE"])
  td <- dplyr::recode_factor(pa_rf[-selection, "PRESENCE"], `1` = "ABSENT", `2` = "PRESENT")
  
                    
  curConfusionMatrix <- caret::confusionMatrix(predCut, td, positive = "PRESENT")
  
  
  mean(p1); mean(p2)
  
  
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
  

  
  
  
  
  data(agaricus.train, package = "xgboost")
  data(agaricus.test, package = "xgboost")

train <- agaricus.train
test <- agaricus.test
str(train)


bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
bstDense  <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 2)


pred <- predict(bstDMatrix, test$data)
prediction <- as.numeric(pred > 0.5)
plot(prediction)
err <- mean(as.numeric(pred > 0.5) != test$label)

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
dtest <- xgb.DMatrix(data = test$data, label = test$label)
watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


seed <- 627
set.seed(seed)
season <- "spring"
sp.i <- 28
mod_type <- "BO"

raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"

## ~~~~~~~~~~~~~ ##
## Load the data ##
## ~~~~~~~~~~~~~ ##
sp_data <- readRDS(paste0(raw_path, 
                          grep(paste0("biomass_habmod-", season, ".*.rds"), 
                               list.files(raw_path),
                               value = TRUE))) %>% 
  dplyr::filter(SVSPP == sp.i,
                BIOMASS != 0) %>%
  na.omit
  
name <- unique(sp_data$name)

out_vec <- sp_data[["BIOMASS"]]

out_vec %>% 
  data_frame(BIOMASS = .) %>% 
  ggplot(aes(x = BIOMASS)) + 
  geom_density(fill = viridis::viridis(1), alpha = 0.6) + 
  labs(title = sprintf("Distribution of the %s biomass", name))


res_var <- dplyr::case_when(mod_type %in% c("BM", "BO") ~ "BIOMASS",
                            mod_type == "PA" ~ "PRESENCE",
                            TRUE ~ NA_character_)
pred_vars <- colnames(sp_data)[!colnames(sp_data) %in% c("PRESENCE", "ABUNDANCE", "BIOMASS", "name", "SVSPP", "LON", "LAT", "YEAR")]

rf_data <- sp_data[, c(res_var, pred_vars, "LAT", "LON")]

# rf_data <- Filter(var, rf_data)

formula <- BIOMASS~.
# dtrain$G3<-as.factor(dtrain$G3)
control <- trainControl(method = "cv", number=10)
metric <- "Accuracy"    
fit.xgbl <- train(formula, data=dtrain, method="xgbLinear", metric=metric, trControl=control, nthread = parallel::detectCores()-1)
fit.xgbl


#-------------Basic Training using XGBoost in caret Library-----------------
# Set up control parameters for caret::train
# Here we use 10-fold cross-validation, repeating twice, and using random search for tuning hyper-parameters.
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "random")
# train a xgbTree model using caret::train
model <- train(factor(Improved)~., data = df, method = "xgbTree", trControl = fitControl)

# Instead of tree for our boosters, you can also fit a linear regression or logistic regression model using xgbLinear
# model <- train(factor(Improved)~., data = df, method = "xgbLinear", trControl = fitControl)




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




