rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(party)
library(caret)

wd <- ("~/slarge/HabMod/data/")

# First get the species of interest
svspp_dat <- read.csv(paste0(wd, "SVSPP.csv"), stringsAsFactors = FALSE)

svspp_dat <- svspp_dat %>% 
  mutate(COMNAME = tolower(COMNAME),
         COMNAME = gsub("atlantic", "Atlantic", COMNAME),
         COMNAME = gsub("american", "American", COMNAME),
         COMNAME = gsub("acadian", "Acadian", COMNAME)) %>% 
  dplyr::select(-SCINAME, -FISH, -PD, -TL)

stock_list <- data.frame(matrix(c("Gulf of Maine", "Atlantic cod",
                                  "Georges Bank", "Atlantic cod",
                                  "Georges Bank", "haddock",
                                  "Gulf of Maine", "haddock",
                                  "Cape Cod/Gulf of Maine", "yellowtail flounder", 
                                  "Southern New England/Mid-Atlantic", "yellowtail flounder", 
                                  "Georges Bank", "winter flounder", 
                                  "Southern New England/Mid-Atlantic", "winter flounder", 
                                  "Gulf of Maine/Georges Bank", "American plaice", 
                                  "all", "Witch flounder", 
                                  "all", "Acadian redfish", 
                                  "all", "White hake", 
                                  "all", "Pollock", 
                                  "all", "Atlantic wolffish", 
                                  "all", "Atlantic halibut", 
                                  "Northern", "windowpane", 
                                  "Southern", "windowpane", 
                                  "all", "Ocean pout", 
                                  "Gulf of Maine", "winter flounder", 
                                  "Georges Bank", "yellowtail flounder"), 
                                byrow = TRUE, 
                                ncol = 2),
                         stringsAsFactors = FALSE)

colnames(stock_list) <- c("AREA", "COMNAME")

stock_list <- stock_list %>% 
  mutate(COMNAME = tolower(COMNAME)) %>% 
  left_join(svspp_dat, by = "COMNAME")

data_files <- list.files(path = wd, full.names = FALSE)
data_files <- data_files[!grepl("csv", data_files)]
data_files <- "spring.data.RData"

lapply(paste0(wd, data_files), load, .GlobalEnv)

# data_names <- gsub("\\..*$", "", tolower(data_files))
# hab_names <- data_names[!grepl("survdat", data_names)]
# 
# col_names <- lapply(hab_names, function(x) colnames(get(x)))
# 
# # The fancy piece of footwork below takes the list of data files and joins based on intersecting names
# join_names <- Reduce(intersect, col_names)
# 
# full_dat <- purrr::map(hab_names, get) %>% 
#   Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = join_names), .)

# clean up a bit
# rm(list = hab_names)

# Fit full model including all habitat covariates and the abundance based SAC term
# An unbiased framework is used here because predictor variables differ on their scale of measurement 

sp_name <- "witch flounder"

run_01 <- c("BOTSALIN","SURFSALIN")
run_02 <- c("BT_SD","ST_SD")
run_03 <- c("rast_complexity",
           "rast_namera_bpi","rast_namera_vrm","rast_plcurv10km","rast_plcurv20km",
           "rast_plcurv2km","rast_prcurv10km","rast_prcurv20km","rast_prcurv2km",
           "rast_rugosity","rast_seabedforms","rast_slp10km",
           "rast_slp20km","rast_slp2km","rast_slpslp10km","rast_slpslp20km",
           "rast_slpslp2km","rast_soft_sed","rast_vortfa","rast_vortsp",
           "rast_vortsu","rast_vortwi")
run_04 <- c("chl_f_ann_01","chl_f_ann_02","chl_f_ann_03","chl_f_ann_04",
           "chl_f_ann_05","chl_f_ann_06","chl_f_ann_07","chl_f_ann_08",
           "chl_f_ann_09","chl_f_ann_10","chl_f_ann_11","chl_f_ann_12",
           "chl_f_ann_s_01","chl_f_ann_s_02","chl_f_ann_s_03","chl_f_ann_s_04",
           "chl_f_ann_s_05","chl_f_ann_s_06","chl_f_ann_s_07","chl_f_ann_s_08",
           "chl_f_ann_s_09","chl_f_ann_s_10","chl_f_ann_s_11","chl_f_ann_s_12")
run_05 <- c("chl_f_clim_01","chl_f_clim_02","chl_f_clim_03","chl_f_clim_04",
           "chl_f_clim_05","chl_f_clim_06","chl_f_clim_07","chl_f_clim_08",
           "chl_f_clim_09","chl_f_clim_10","chl_f_clim_11","chl_f_clim_12",
           "chl_f_clim_s_01","chl_f_clim_s_02","chl_f_clim_s_03","chl_f_clim_s_04",
           "chl_f_clim_s_05","chl_f_clim_s_06","chl_f_clim_s_07","chl_f_clim_s_08",
           "chl_f_clim_s_09","chl_f_clim_s_10","chl_f_clim_s_11","chl_f_clim_s_12")
run_06 <- c("chl_r_ann_01","chl_r_ann_02","chl_r_ann_03","chl_r_ann_04",
           "chl_r_ann_05","chl_r_ann_06","chl_r_ann_07","chl_r_ann_08",
           "chl_r_ann_09","chl_r_ann_10","chl_r_ann_11","chl_r_ann_12",
           "chl_r_ann_s_01","chl_r_ann_s_02","chl_r_ann_s_03","chl_r_ann_s_04",
           "chl_r_ann_s_05","chl_r_ann_s_06","chl_r_ann_s_07","chl_r_ann_s_08",
           "chl_r_ann_s_09","chl_r_ann_s_10","chl_r_ann_s_11","chl_r_ann_s_12")
run_07 <- c("chl_r_clim_01","chl_r_clim_02","chl_r_clim_03","chl_r_clim_04",
           "chl_r_clim_05","chl_r_clim_06","chl_r_clim_07","chl_r_clim_08",
           "chl_r_clim_09","chl_r_clim_10","chl_r_clim_11","chl_r_clim_12",
           "chl_r_clim_s_01","chl_r_clim_s_02","chl_r_clim_s_03","chl_r_clim_s_04",
           "chl_r_clim_s_05","chl_r_clim_s_06","chl_r_clim_s_07","chl_r_clim_s_08",
           "chl_r_clim_s_09","chl_r_clim_s_10","chl_r_clim_s_11","chl_r_clim_s_12")
run_08 <- c("sst_f_ann_01","sst_f_ann_02","sst_f_ann_03","sst_f_ann_04",
           "sst_f_ann_05","sst_f_ann_06","sst_f_ann_07","sst_f_ann_08",
           "sst_f_ann_09","sst_f_ann_10","sst_f_ann_11","sst_f_ann_12",
           "sst_f_ann_s_01","sst_f_ann_s_02","sst_f_ann_s_03","sst_f_ann_s_04",
           "sst_f_ann_s_05","sst_f_ann_s_06","sst_f_ann_s_07","sst_f_ann_s_08",
           "sst_f_ann_s_09","sst_f_ann_s_10","sst_f_ann_s_11","sst_f_ann_s_12")
run_09 <- c("sst_f_clim_01","sst_f_clim_02","sst_f_clim_03","sst_f_clim_04",
           "sst_f_clim_05","sst_f_clim_06","sst_f_clim_07","sst_f_clim_08",
           "sst_f_clim_09","sst_f_clim_10","sst_f_clim_11","sst_f_clim_12",
           "sst_f_clim_s_01","sst_f_clim_s_02","sst_f_clim_s_03","sst_f_clim_s_04",
           "sst_f_clim_s_05","sst_f_clim_s_06","sst_f_clim_s_07","sst_f_clim_s_08",
           "sst_f_clim_s_09","sst_f_clim_s_10","sst_f_clim_s_11","sst_f_clim_s_12")
run_10 <- c("sst_r_ann_01","sst_r_ann_02","sst_r_ann_03","sst_r_ann_04",
            "sst_r_ann_05","sst_r_ann_06","sst_r_ann_07","sst_r_ann_08",
            "sst_r_ann_09","sst_r_ann_10","sst_r_ann_11","sst_r_ann_12",
            "sst_r_ann_s_01","sst_r_ann_s_02","sst_r_ann_s_03","sst_r_ann_s_04",
            "sst_r_ann_s_05","sst_r_ann_s_06","sst_r_ann_s_07","sst_r_ann_s_08",
            "sst_r_ann_s_09","sst_r_ann_s_10","sst_r_ann_s_11","sst_r_ann_s_12")
run_11 <- c("sst_r_clim_01","sst_r_clim_02","sst_r_clim_03","sst_r_clim_04",
            "sst_r_clim_05","sst_r_clim_06","sst_r_clim_07","sst_r_clim_08",
            "sst_r_clim_09","sst_r_clim_10","sst_r_clim_11","sst_r_clim_12",
            "sst_r_clim_s_01","sst_r_clim_s_02","sst_r_clim_s_03","sst_r_clim_s_04",
            "sst_r_clim_s_05","sst_r_clim_s_06","sst_r_clim_s_07","sst_r_clim_s_08",
            "sst_r_clim_s_09","sst_r_clim_s_10","sst_r_clim_s_11","sst_r_clim_s_12")
run_12 <- c("zoo_spr_ann_acarspp","zoo_spr_ann_calfin","zoo_spr_ann_chaeto","zoo_spr_ann_cham",
            "zoo_spr_ann_cirr","zoo_spr_ann_ctyp","zoo_spr_ann_echino","zoo_spr_ann_evadnespp",
            "zoo_spr_ann_gas","zoo_spr_ann_hyper","zoo_spr_ann_larvaceans","zoo_spr_ann_mlucens",
            "zoo_spr_ann_oithspp","zoo_spr_ann_para","zoo_spr_ann_penilia","zoo_spr_ann_pseudo",
            "zoo_spr_ann_salps","zoo_spr_ann_tlong","zoo_spr_ann_volume")
run_13 <- c("zoo_spr_clim_acarspp",
            "zoo_spr_clim_calfin","zoo_spr_clim_chaeto","zoo_spr_clim_cham","zoo_spr_clim_cirr",
            "zoo_spr_clim_ctyp","zoo_spr_clim_echino","zoo_spr_clim_evadnespp","zoo_spr_clim_gas",
            "zoo_spr_clim_hyper","zoo_spr_clim_larvaceans","zoo_spr_clim_mlucens","zoo_spr_clim_oithspp",
            "zoo_spr_clim_para","zoo_spr_clim_penilia","zoo_spr_clim_pseudo","zoo_spr_clim_salps",
            "zoo_spr_clim_tlong","zoo_spr_clim_volume")

METRIC <- "ABUNDANCE"

fit_rf <- function(sp_name, run){}
rf_df <- spring.data %>%
  left_join(svspp_dat, by = "SVSPP") %>% 
  filter(COMNAME == sp_name) %>% 
  # left_join(full_dat) %>% 
  dplyr::select_(.dots = c(METRIC, run_11)) %>% 
  as.data.frame() %>% 
  na.omit

#Randomly select 75% of the data to enable 10 fold cross validation
selection <- createDataPartition(y = rf_df[, METRIC],
                                 p = 0.75,
                                 list = FALSE)

training.data <- rf_df[selection,]
testing.data<- rf_df[-selection,]

f <- paste(METRIC, 
           paste(run_11, collapse = ' + '), 
           sep = ' ~ ')

mtry <- sqrt(ncol(training.data -1))
tunegrid <- expand.grid(.mtry = mtry)

strt_time <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

train_control <- trainControl(method = "cv",
                              number = 10, 
                              # returnResamp = "all", 
                              search = "random",
                              allowParallel = TRUE,
                              verboseIter = TRUE)


wf_abund <- train(as.formula(f),
                  data = training.data,
                  method = "rf",
                  tunegrid = tunegrid,
                  ntry = 500,
                  trControl = train_control,
                  importance = TRUE)

stopCluster(cluster)
registerDoSEQ()
stp_time <- Sys.time()
stp_time - strt_time

show(wf_abund)
wf_varImp <- varImp(wf_abund)

plot(wf_abund)
plot(wf_varImp)



# im = importance(wf_abund)
# im = data.frame(im)

# Use model to make probabilistic predictions
probability.pred <- predict(wf_abund, 
                            OOB = TRUE,
                            newdata = testing.data,
                            type = "prob") 
probability.pred <- data.frame(matrix(unlist(probability.pred),
                                      nrow = dim(testing.data)[1],
                                      byrow = T))

# Probability of occurrence is the sum of the non-zero predictions
g <- dim(probability.pred)[2]
ifelse(g <= 2,
       probability.pred$presence <- (probability.pred[,2]),
       ifelse(g > 2,
              probability.pred$presence <- rowSums(probability.pred[, c(2:g)], 0)))
# These values of probability of occurence can then be used to calculate AUC and calibration curves


# Make predictions on the scale of the response variable, in this case the categorical abundance scale
response.pred <- predict(abundance.cforest, 
                         newdata = testing.data, 
                         type="response") 
response.pred <- as.numeric(as.character(response.pred))
# These predicted response values can then be used to calculate weighted kappa values, or can be converted to presence-absence data



