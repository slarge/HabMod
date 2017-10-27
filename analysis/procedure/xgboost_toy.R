library(caret)
library(xgboost)
library(dplyr)


set.seed(627)

pa_data <- all_dat_op %>% 
  dplyr::filter(SVSPP == sp.i) %>% 
  na.omit


pa_data[, "PRESENCE"] <- as.factor(pa_data[, "PRESENCE"])
pa_data[, "PRESENCE"] <- dplyr::recode_factor(pa_data[, "PRESENCE"], `0` = "ABSENT", `1` = "PRESENT")

selection <- caret::createDataPartition(y = pa_data[, "PRESENCE"],
                                        p = 0.75,
                                        list = FALSE)
pa_train <- pa_data[selection, ]
pa_test <- pa_data[-selection, ]

pa_y <- pa_train[, "PRESENCE"]

pa_x <- pa_train[, !colnames(pa_train) %in% c("LAT", "LON", "BIOMASS",
                                            "PRESENCE", "ABUNDANCE", 
                                            "name", "SVSPP", "YEAR")]

cv.ctrl <- trainControl(method = "repeatedcv",
                        repeats = 1,
                        number = 3, 
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        early_stopping_rounds = 10,
                        allowParallel = T)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.01, 0.001, 0.0001),
                        gamma = c(0, 1),
                        max_depth = c(2, 4, 6, 
                                      8, 10, 14),
                        subsample = c(0.5, 0.75, 1), 
                        colsample_bytree = c(0.6, 0.8, 1),
                        min_child_weight = 1)

xgb_tune <- train(x = pa_x,
                 y = pa_y,
                 method = "xgbTree",
                 trControl = cv.ctrl,
                 tuneGrid = xgb.grid,
                 verbose = T,
                 # metric = "ROC",
                 nthread = 11)

names <- dimnames(data.matrix(X[,-1]))[[2]]

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")