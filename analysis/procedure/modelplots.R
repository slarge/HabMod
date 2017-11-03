rm(list = ls())
#-------------------------------------------------------------------------------
#Requiredpackages
library(randomForest)
library(dplyr)
library(viridis)
# devtools::install_github('tidyverse/ggplot2')
library(ggplot2)
# library(scales)
library(ggthemes)
# devtools::install_github("dgrtwo/gganimate")
library(gganimate)
# library(RFinfer)
# devtools::install_github("swager/randomForestCI")
# library(randomForestCI)
library(pdp)
set.seed(627)

#-------------------------------------------------------------------------------
raw_path <- "analysis/data/raw_data/"
derived_path <- "analysis/data/derived_data/"
fig_path <- "analysis/figures/"
#-------------------------------------------------------------------------------

# couple of functions
partial_overview <- function(object, pred.var) {
  
  partial_output <- pdp::partial(object, pred.var, chull = FALSE)
  colnames(partial_output) <- c("x", "y")
  partial_output$pred.var <- pred.var
  return(partial_output)
}


gggsave <- function(filename, plot, ...) {
  ggsave(filename, plot, ...)
  
  if(grepl("fall", filename)) sns <- "fall"
  if(grepl("spring", filename)) sns <- "spring"
  
  gd_path <- googledrive::as_dribble(file.path("temp_for_scott/analysis/output", sns))
  googledrive::drive_upload(media = filename, path = gd_path, verbose = TRUE)
  
}



season_list <- c("fall", "spring")
sp.svspp <- c(22, 28, 73, 74, 105, 107, 141)
season = "fall"
# sp.i = 73
mod_type = "BM"

for(season in season_list) {

  for(sp.i in sp.svspp){
    
    sp_data <- readRDS(paste0(raw_path,
                              grep(paste0("biomass_habmod-", season, ".*.rds"),
                                   list.files(raw_path),
                                   value = TRUE))) %>%
      dplyr::filter(SVSPP == sp.i) %>%
      na.omit
    # 
    name <- unique(sp_data$name)
    
    files <- grep(paste(unique(sp_data$name), collapse = "|"),
                  list.files(paste0(derived_path, season, "/"), 
                             pattern = "model.rds"), value = TRUE)
    
    run.rf <- readRDS(paste0(derived_path, season, "/", grep(paste0(name,"-", season, "-", mod_type, "-RF"),
                                                             files, value = TRUE)))
    
    pred_vars <- run.rf$finalModel$xNames

    pd <- purrr::map(pred_vars, function(x) partial_overview(object = run.rf, pred.var = x)) %>% bind_rows()
    
    pd2 <- pd %>% 
      group_by(pred.var) %>% 
      summarize(TOTAL = sum(y),
                MAX = max(y)) %>% 
      arrange(-MAX, -TOTAL) %>% 
      mutate(pred.var = factor(pred.var, pred.var))
      
    pd$pred.var <- factor(pd$pred.var, levels =   levels(pd2$pred.var))  
    
    map_title <- case_when(mod_type == "BO" ~ "Biomass only model",
                           mod_type == "BM" ~ "Biomass with occupancy model",
                           mod_type == "PA" ~ "Occupancy model")
    
    map_legend <- case_when(mod_type %in% c("BO", "BM") ~ "Biomass (log) in tons",
                            mod_type == "PA" ~ "Probability of presence")
    
    ceiling(length(unique(pd$pred.var))/4)
    
    pd_plot <- ggplot(pd) + 
      # geom_area(aes(x = x, y = y), color = "grey20", alpha = .2) +
      geom_line(aes(x = x, y = y), color = "black") +
      facet_wrap(~ pred.var, scales = "free_x", strip.position = "top", ncol = 4) +
      theme_bw() +
      labs(subtitle = paste0(gsub("_", " ", name), " - ", map_title),
           # color = "",
           y = map_legend,
           x = "") +
      theme(legend.position = "bottom",
            strip.background = element_blank(), 
            legend.key.width = unit(0.5, "cm"))
    
    gggsave(filename = paste0(fig_path, season, "/", name,"-", season, "-partial_plot.png"),
            plot = pd_plot,
            width = 8,
            height = ifelse(ceiling(length(unique(pd$pred.var))/4) > 4,
                            10,
                            6))
    
  } # close species loop
} # close season loop
  
  # autoplot(pd, contour = FALSE, main = "ggplot2 version", 
  #          legend.title = "Partial\ndependence") +
  #   facet_wrap(.~ pred.var)
  
    # if(mod_type %in% c("BO", "BM")){
      
      # if(mod_type == "BM"){
      #   PA_rf <- readRDS(paste0(derived_path, season, "/", grep(paste0(name, "-", "PA-RF"),
      #                                                           files, value = TRUE)))
      #   
      #   sp_data$PRESPROB <- predict(PA_rf, newdata = sp_data, type = "prob")[, 2]
      #   rm(PA_rf)
      # }
      # 
      # res_var <- dplyr::case_when(mod_type %in% c("BM", "BO") ~ "BIOMASS",
      #                             mod_type == "PA" ~ "PRESENCE",
      #                             TRUE ~ NA_character_)
      # 
      # rf_data <- sp_data[, c(res_var, pred_vars, "LAT", "LON")]
      # 
      # # if(mod_type %in% c("BM", "BO")){
      # rf_data <- rf_data[rf_data$BIOMASS != 0,]
      # # }
      # 
      # rf_data <- Filter(var, rf_data)
      # 
      # rf_selection <- caret::createDataPartition(y = rf_data[, res_var],
      #                                            p = 0.75,
      #                                            list = FALSE)
      # 
      # rf_train <- rf_data[rf_selection, ]
      # rf_test <- rf_data[-rf_selection, ]
      # 
      # rf_trainX <- rf_train[, names(rf_train) %in% pred_vars]
      # rf_trainY <- rf_train[, res_var]
      # 
      # # if(mod_type == "PA"){
      # #   rf_trainY <- as.factor(rf_trainY)
      # # }
      # 
      # strt_time <- Sys.time()
      # cluster <- parallel::makeCluster(parallel::detectCores() - 1)
      # doParallel::registerDoParallel(cluster)
      # 
      # rf_run <- randomForest::randomForest(x = rf_trainX, y = rf_trainY,
      #                                      mtry = run.rf$finalModel$mtry,
      #                                      ntree = 5000,
      #                                      keep.inbag = TRUE,
      #                                      importance = TRUE,
      #                                      replace = TRUE)
      # 
      # parallel::stopCluster(cluster)
      # foreach::registerDoSEQ()
      # 
      # stp_time <- Sys.time()
      # stp_time - strt_time
      # 
      # # 
      # rf_test$PREDICTION <- predict(rf_run,  rf_test)
      # rf_test$RESID <- rf_test$BIOMASS - rf_test$PREDICTION
      # # 
      # 
      # RMSE_test <- sqrt(sum((rf_test$BIOMASS - rf_test$PREDICTION)^2)/length(rf_test$PREDICTION))
      # rRMSE_test <- round(RMSE_test/(max(rf_test$BIOMASS) - min(rf_test$BIOMASS)), 2)
      # 
      # var_hat <- randomForestCI::randomForestInfJack(rf_run, rf_test[, -1], calibrate = TRUE)
      # 
      # # 
      # df <- data.frame(BIOMASS = rf_test$BIOMASS, PREDICTED = var_hat$y.hat, error = var_hat$var.hat)
      # df <- df %>%
      #   dplyr::mutate(ci_upper = PREDICTED + (1.96 * sqrt(error)),
      #                 ci_lower = PREDICTED - (1.96 * sqrt(error)))
      # 
      # ## Predicted/observed plot
      # p1 <- ggplot(df, aes(x = BIOMASS, y = PREDICTED))
      # p1 <- p1 +
      #   geom_linerange(aes(ymin = ci_lower,
      #                      ymax = ci_upper),
      #                  color = "black", alpha = 0.2) +
      #   geom_point(color = "black", alpha = 0.8, shape = 16) +
      #   geom_abline(intercept=0, slope = 1, linetype = 2) +
      #   labs(x = "Observed biomass",
      #        y = "Predicted biomass",
      #        title = sprintf("%s (%s)", gsub("_", " ", name), season),
      #        subtitle = paste0("The error (relative RMSE) is about ", rRMSE_test * 100, "% \nas large as the mean biomass.")) +
      #   coord_equal() +
      #   theme_bw()
      ggsave(paste0(fig_path, name,"-", season, "-fit_plot.png"), p1, width = 5, height = 5)
      
      ## Importance plot
      im <- caret::varImp(run.rf)
      
      stackedData <- caret:::sortImp(im, top =  dim(im$importance)[1])
      
      featureNames <- rownames(stackedData)
      outcomeNames <- colnames(stackedData)
      featureNames <- gsub("_0d$", "", featureNames)
      
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)), 
                                    levels = rev(featureNames))
      names(stackedData) <- c("Importance", "Feature")
      
      p4 <- ggplot(stackedData, aes(x = Feature, y = Importance)) +
        geom_segment(aes(x = Feature, xend = Feature,
                         y = 0, yend = Importance)) +
        geom_point(stat = "identity", aes(y = Importance), color = "grey50") +
        coord_flip() +
        labs(x = "",
             y = "Importance (scaled)",
             title = gsub("_", " ", name)) +
        theme_bw()
      
      ggsave(paste0(fig_path, name, "-", season, "-importance_plot.png"), p4, width = 5, height = 5)
      




## The stuff below is used for plotting, and model validation
bm_test$PREDICTED <- predict(rf_bm, bm_test)
bm_test$RESIDUALS <- bm_test$BIOMASS - bm_test$PREDICTED

bm_moran_dat <- bm_test %>%
  select(LAT, LON, PREDICTED, BIOMASS, RESIDUALS) %>%
  na.omit

bm_corr_res <- ncf::correlog(x = bm_moran_dat$LAT , y = bm_moran_dat$LON, z = bm_moran_dat$RESIDUALS,
                             increment = 1, resamp = 500, latlon = TRUE)

bm_corr_raw <- ncf::correlog(x = bm_moran_dat$LAT , y = bm_moran_dat$LON, z = bm_moran_dat$BIOMASS,
                             increment = 1, resamp = 500, latlon = TRUE)


td <- data.frame(bm_corr_res$correlation)

plot(bm_corr_res)

rf_rmse <- round(sqrt(tail(rf_bm$finalModel$mse, 1)), 2)

pl1 <- ggplot(bm_test, aes(x = PREDICTED, y = BIOMASS)) +
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, col = "grey40", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, col = "grey40") +
  theme_minimal() +
  labs(title = "",
       subtitle = paste0("RMSE = ", rf_rmse),
       x = expression(Model~predicted~biomass~(log[10])),
       y = expression(Observed~biomass~(log[10]))) +
  theme(aspect.ratio = 1)

## Maps and other sundrys
#
# us <- map_data("state")
#
# data(nw.atlantic)
# atl <- as.bathy(nw.atlantic)
# #
# gg <- ggplot()
# gg <- gg + geom_map(data = us, map = us,
#                     aes(x = long, y = lat,
#                         map_id = region),
#                     fill="grey70", color="grey90", size=0.15) +
#   geom_contour(data = atl, aes(x = x, y = y, z = z), color = "grey20", size = 0.1) +
#   coord_map(xlim = c(-80, -65),
#             ylim = c(37.5, 47.5)) +
#   theme_minimal()

# pl2 <- gg +
#   geom_point(data = bm_test, aes(x = LON, y = LAT, col = PREDICTED), alpha = 0.7, size = 0.75) +
#   scale_color_viridis() +
#   labs(x = "", y = "",
#        title = gsub("_", " ", name),
#        subtitle = expression(Predicted~biomass~(log[10])),
#        color = "") +
#   theme(legend.position = "none")

# both_fit <- cowplot::plot_grid(pl2, pl1, labels = NULL, align = 'hv', axis = "b")

# ggsave(both_fit, filename = "haddock_fit.pdf", height = 210, width = 297, units = "mm")

# # Use model to make probabilistic predictions
# probability.pa.pred <- predict(wf_abund, newdata = testing.data, "raw")
# probability.pa.pred <- data.frame(matrix(unlist(probability.pa.pred),
#                                          nrow = dim(testing.data)[1],
#                                          byrow = T))
#
# table(testing.data$PRESENCE, probability.pa.pred)



# This will produce a 2 column data frame with both probability of absence and presence.
# These values of probability of occurence can then be used to calculate AUC and calibration curves.
# wf_latlong <- wf_dat %>%
#   select(-one_of(na_count$COLS[na_count$NAs >= na_nums]),
#          -one_of(join_names),
#          LAT, LON) %>%
#   # mutate(PRESENCE = as.factor(recode(PRESENCE, `1` = "present", `0` = "absent"))) %>%
#   na.omit %>%
#   as.data.frame


# Make predictions on the scale of the response variable, in this case the binary presence-absence scale
testing.data$PREDICTED <- predict(wf_abund, newdata = testing.data, type="raw")

# These predicted response values can then be used to calculate kappa values
testing.data$residuals <- as.numeric(as.character(testing.data$PRESENCE)) - as.numeric(as.character(testing.data$PREDICTED))

### ADDITIONAL CODE ###

## look at correlation structure in data
# source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
#
# ggcorr(trainX, geom = "blank", label = FALSE, hjust = .75, size = 3, layout.exp = 4) +
#   geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) >= 0.7)) +
#   scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
#   guides(color = FALSE, alpha = FALSE)
#
#

# # Plot the data distribution of complete cases
# ggplot(na_select, aes(x = NCOL, y = NROW)) +
#   geom_segment(x = max_ncols, y = 0,
#                xend = max_ncols,
#                yend = max_nrows,
#              col = "red") +
#   geom_segment(x = -Inf, y = max_nrows,
#                xend = max_ncols,
#                yend = max_nrows,
#                col = "red") +
#   geom_point() +
#   theme_bw() +
#   labs(y = "Complete cases (n)",
#        x = "Columns (n)",
#        title = paste0("Max complete cases = ", max_nrows , "\nMax columns = ", max_ncols))

