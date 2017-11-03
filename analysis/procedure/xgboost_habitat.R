rm(list = ls())
seed <- 627
set.seed(seed)

library(Matrix)
library(xgboost)
library(dplyr)
library(rBayesianOptimization)
# devtools::install_github("AppliedDataSciencePartners/xgboostExplainer")
# library(xgboostExplainer)

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
                          nround = 100, # Keep this smaller to be more efficient in finding best hyperparameters
                          folds = pa_cv_folds, 
                          prediction = TRUE,
                          showsd = TRUE, 
                          early_stop_round = 5, 
                          maximize = TRUE,
                          verbose = 0
    )
    list(Score = max(cv$evaluation_log$train_auc_mean), #cv$evaluation_log$test_auc_mean[cv$best_iteration]
         Pred = cv$pred)
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
    bounds = list(eta = c(0.001, 0.3),
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
  
  ## Identify the index of that maximizes AUC
  pa_bst_idx <- bst_plain$evaluation_log$iter[bst_plain$evaluation_log$test_auc_mean == max(bst_plain$evaluation_log$test_auc_mean)]
  
  pa_bst <-  xgboost(data = pa_train,
                     params = pa_params, 
                     maximize = TRUE,
                     nrounds = pa_bst_idx, 
                     verbose = 1) 
  
  pred <- predict(pa_bst, pa_test)
  
  ## ~~~~~~~~~~~~~ ##
  ## BIOMASS MODEL ##
  ## ~~~~~~~~~~~~~ ##
  bm_rf <- pa_data[, c("BIOMASS", keepers)]
  
  # Add the probability of occurance to the Biomass data
  bm_rf$PRESPROB <- predict(pa_bst, newdata = data.matrix(bm_rf[, -1]))
  
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
                          nround = 100, # Keep this smaller to be more efficient in finding best hyperparameters
                          folds = bm_cv_folds, 
                          prediction = TRUE,
                          showsd = TRUE, 
                          early_stop_round = 5, 
                          maximize = FALSE,
                          verbose = 0
    )
    list(Score = min(cv$evaluation_log$train_rmse_mean), #cv$evaluation_log$test_auc_mean[cv$best_iteration]
         Pred = cv$pred)
  }
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Tune the hyperparameters ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
  bm_cv_folds <- rBayesianOptimization::KFold(as.matrix(bm_df_train[, target.var]),
                                           nfolds = 5, 
                                           stratified = TRUE, 
                                           seed = seed)
  
  bm_xgb_bayes <- BayesianOptimization(
    xgb.cv.bayes.linear,
    bounds = list(eta = c(0.001, 0.3),
                  max_depth = c(2L, 12L),
                  subsample = c(0.5, 1),
                  colsample_bytree = c(0.1, 0.4)
    ),
    init_grid_dt = NULL,
    init_points = 1,  # number of random points to start search
    n_iter = 2, # number of iterations after initial random points are set
    acq = 'ucb', kappa = 2.576, eps = 0.0, verbose = TRUE
  )
  
  bm_params <- list(
    objective = "reg:linear",
    seed = seed,
    eta = bm_xgb_bayes$Best_Par[["eta"]],
    max.depth = bm_xgb_bayes$Best_Par[["max_depth"]],
    subsample = bm_xgb_bayes$Best_Par[["subsample"]],
    colsample_bytree = bm_xgb_bayes$Best_Par[["colsample_bytree"]],
    eval_metric = "rmse",
    nthread = parallel::detectCores()-1)
  
  ## ~~~~~~~~~~~~~~ ##
  ## Tune the model ##
  ## ~~~~~~~~~~~~~~ ##
  
  bm_watchlist <- list(train = bm_train,
                       test = bm_test)
  
  bm_bst_plain <- xgb.cv(data = bm_train,
                         watchlist = bm_watchlist,
                         params = bm_params, 
                         maximize = FALSE,
                         nrounds = 500,
                         folds = bm_cv_folds,
                         print_every_n = 10,
                         early_stop_round = 5)
  
  ## Identify the index of that maximizes AUC
  bm_bst_idx <- bm_bst_plain$evaluation_log$iter[bm_bst_plain$evaluation_log$test_rmse_mean == min(bm_bst_plain$evaluation_log$test_rmse_mean)]
  
  bm_bst <-  xgboost(data = bm_train,
                  params = bm_params, 
                  maximize = TRUE,
                  nrounds = bm_bst_idx, 
                  verbose = 0) 
  
  pred <- predict(bst, test)
  
  # get the trained model
  model = xgb.dump(bst, with_stats=TRUE)
  # get the feature real names
  names = colnames(pa_rf[,-1])
  # compute feature importance matrix
  importance_matrix = xgb.importance(names, model=bst)
  
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
  