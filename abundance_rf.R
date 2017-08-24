rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(party)
library(caret)
library(parallel)
library(doParallel)
library(VSURF)
library(httr)
library(googleVis)
library(xml2)
library(viridis)
library(marmap)

# Using Data Miner with R 
source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")
source("utilities.R")
# source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/WPS4D4Science.r")

#SETTING USERNAME AND TOKEN - NOT NEEDED WHEN USING RSTUDIO ON THE PORTAL
username <<- "largesi"
token <<-"1ba80d8d-15ea-4ff1-9420-f8c97ea15d40-843339462" #YOUR TOKEN FOR A VRE

wd <- ("~/slarge/HabMod/data/")
# wd <- ("~/git/slarge/HabMod/data/")

## ~~~~~~~~~~~~~ ##
## Load the data ##
## ~~~~~~~~~~~~~ ##
svspp_dat <- read.csv(paste0(wd, "SVSPP.csv"), stringsAsFactors = FALSE)

svspp_dat <- svspp_dat %>% 
  mutate(COMNAME = tolower(COMNAME)) %>% 
  dplyr::select(COMNAME, SVSPP)

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
  left_join(svspp_dat, by = "COMNAME") %>% 
  mutate(COMNAME = gsub("atlantic", "Atlantic", COMNAME),
         COMNAME = gsub("american", "American", COMNAME),
         COMNAME = gsub("acadian", "Acadian", COMNAME)) %>% 
  select(-AREA) %>% 
  distinct(.keep_all = TRUE)

data_files <- list.files(path = wd, full.names = FALSE)
data_files <- data_files[!grepl("csv", data_files)]
data_files <- "spring.data.RData"

lapply(paste0(wd, data_files), load, .GlobalEnv)
# C:\Users\scott.large\Google Drive\temp_for_scott\1_habitat_analysis_2017\spring models lt only\spring.data.Rdata

## ~~~~~~~~~~~~~~~~~ ##
## Clean up the data ##
## ~~~~~~~~~~~~~~~~~ ##
join_names <- c("CRUISE6", "STRATUM", "STATION", "SVVESSEL", "YEAR", "SEASON", "LAT",
                "LON", "EST_TOWDATE", "DEPTH", "DOY", "SVSPP")

bad_dat <- c("rast_necrm_bpi", "rast_necrm_vrm" ,
             "rast_bpi_3_25_layer", "rast_bpi_30_250_layer",
             "rast_mab_sed", "rast_gdepth",
             "rast_gravel_fraction","rast_mud_fraction",
             "rast_phi_fraction","rast_sand_fraction")

names(spring.data) <- sub(" ", "", names(spring.data))

spring_dat <- spring.data %>% 
  filter(SVSPP %in% unique(stock_list$SVSPP)) %>% 
  select(-TOW,
         -CATCHSEX,
         -one_of(bad_dat)) %>% 
  distinct(.keep_all = TRUE)

rm(list = c("spring.data", "svspp_dat", "data_files"))

## ~~~~~~~~~~~~~~ ##
## Add zero sites ##
## ~~~~~~~~~~~~~~ ##

# Each row is an observation, so count the number of each species per year, strata, and station sampled.
# Spread by each species, so NAs are created where species aren't present for a year, stratum, and station.
# Replace NAs with 0 and reorganize into a long data frame by year, stratum, and station.
# Sum the number of observations per year, species, and stratum. If a species is not found in a year, stratum, station
# but is found in that year stratum, they are considered "absent" or 0. If a species is not found in a year, stratum, station
# nor is found in that year stratum, they are NA and removed.

pa_table <- spring_dat %>%
  group_by(YEAR, SVSPP, STRATUM, STATION) %>%
  summarize(count = n()) %>%
  tidyr::spread(SVSPP, value = count) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  tidyr::gather(SVSPP, value = count, -YEAR, -STRATUM, -STATION) %>%
  group_by(YEAR, SVSPP, STRATUM) %>%
  mutate(stratum_sum = sum(count), #
         PRESENCE = case_when(count == 0 & stratum_sum >= 1 ~ 0, # Not at station but in stratum
                              count == 1 & stratum_sum >= 0 ~ 1, # Present
                              count == 0 & stratum_sum == 0 ~ NA_real_, # Not at station or in stratum
                              TRUE ~ NA_real_)) %>%
  filter(!is.na(PRESENCE))

# Create a data.frame of just the station data
station_dat <- spring_dat %>%
  select(-SVSPP,
         -ABUNDANCE,
         -BIOMASS) %>%
  distinct(.keep_all = TRUE)

# join the p/a data with the station data
pa_dat <- pa_table %>%
  ungroup %>%
  mutate(SVSPP = as.numeric(SVSPP)) %>%
  left_join(station_dat, by = c("YEAR", "STRATUM", "STATION")) %>%
  dplyr::select(-stratum_sum, -count)

# join the p/a data with abundance data
bio_dat <- spring_dat %>% 
  select(ABUNDANCE,
         BIOMASS, 
         join_names) %>% 
  distinct(.keep_all = TRUE)

all_dat <- pa_dat %>% 
  left_join(bio_dat, by = join_names) %>% 
  mutate(ABUNDANCE = ifelse(is.na(ABUNDANCE),
                            0,
                            ABUNDANCE),
         ABUNDANCE = as.numeric(ABUNDANCE),
         BIOMASS = ifelse(is.na(BIOMASS),
                          0,
                          BIOMASS),
         BIOMASS = log10(as.numeric(BIOMASS) + 1))

## Create SAC for each species and metric (PA, biomass, and abundance)
all_dat_sac <- lapply(unique(all_dat$SVSPP), sac_maker) %>% 
  bind_rows

## Split the datasets into PA and Biomass/Abundance partitions
data_part_list <- lapply(unique(all_dat_sac$SVSPP), function(x)
  createDataPartition(y = all_dat_sac %>%
                        filter(SVSPP == x) %>%
                        select(PRESENCE) %>%
                        pull,
                      p = 0.50,
                      list = FALSE))

data_partition <- list(selection = data_part_list)
class(data_partition) <- c("tbl_df", "data.frame")
attr(data_partition, "row.names") <- .set_row_names(length(data_part_list))
data_partition$SVSPP <- unique(all_dat_sac$SVSPP)
data_partition <- unnest(data_partition)

log_name <- paste("biomass_habmod_", Sys.time(),".log", sep="")
log_name <- gsub(":", "_", log_name)
log_name <- gsub(" ", "_", log_name)

save(all_dat_sac, data_partition, file = gsub(".log", ".rdata", log_name))

sp.i <- 73
set.seed(627)
for(sp.i in unique(all_dat_sac$SVSPP)) {
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Split data into P/A model and Biomass model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pa_data <- all_dat_sac[data_partition$selection[data_partition$SVSPP == sp.i] & 
                           all_dat_sac$SVSPP == sp.i,]
  
  name <- unique(pa_data$name)
  
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Starting ", name, " at ",  Sys.time(), ".\n"), file = log_con)
  close(log_con)
  
  ## ~~~~~~~~~~~~~~~~~~~~ ##
  ## Fit P/A/ VSURF model ##
  ## ~~~~~~~~~~~~~~~~~~~~ ##
  pa_y <- pa_data[, "PRESENCE"]
  pa_x <- pa_data[, !colnames(pa_data) %in% c("LAT", "LON", "BIOMASS",
                                              "PRESENCE", "ABUNDANCE", "SAC.az",
                                              "SAC.bm", "name", "SVSPP")]
  ## A few columns are all zeros... might as well remove columns with identical values, too
  pa_x <- Filter(var, pa_x)
  
  log_con <- file(log_name, open = "a")
  cat(paste0("First we'll fit the occupancy model with ",  nrow(pa_x), " observations and ", ncol(pa_x), " explanatory variables.\n"), file = log_con)
  close(log_con)
  
  ## VSURF is a data-driven approach to select important variables from high dimensional datasets
  vsurf_url <- vsurf_bb(file_x = pa_x,
                        file_y = pa_y,
                        ntree = 200,
                        mtry =  floor(sqrt(ncol(pa_x))),
                        nfor_thres = 50,
                        nmin = 5,
                        nfor_interp = 25,
                        nsd = 1,
                        nfor_pred = 25,
                        parallel = "true",
                        ncores = 15,
                        clusterType = "PSOCK",
                        seed = 627,
                        name = paste0(name, "_PA"),
                        username = "largesi",
                        token = "1ba80d8d-15ea-4ff1-9420-f8c97ea15d40-843339462")
  
  stop_condition_fail <- stop_condition_success <- FALSE
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Sent to BB and will monitor: ", vsurf_url, " \nevery 10 minutes.\n"), file = log_con)
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
    Sys.sleep(10*60)
  }
  
  if(stop_condition_success) {
    output_url <- xml_text(xml_find_all(read_xml(out1), ".//d4science:Data"))
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
                paste0(name, "_PA_VSURF_log.txt"),
                mode = "wb",
                cacheOK = FALSE)
  download.file(output_df$OUTPUT,
                paste0(name, "_PA_VSURF_output.rds"),
                mode = "wb",
                cacheOK = FALSE)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Extract important variables ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  temp_vsurf <- readRDS(paste0(name, "_PA_VSURF_output.rds"))
  pa_rf_vars <- names(pa_x)[temp_vsurf$varselect.interp]
  
  log_con <- file(log_name, open = "a")
  cat("P/A VSURF model identified ", pa_rf_vars, " as the best variables for interpretation.\n", file = log_con)
  close(log_con)
  
  pa_rf <- pa_data[, c("PRESENCE", pa_rf_vars, "LAT", "LON")]
  pa_rf[, "PRESENCE"] <- as.factor(pa_rf[, "PRESENCE"])
  pa_rf[, "PRESENCE"] <- dplyr::recode_factor(pa_rf[, "PRESENCE"], `0` = "ABSENT", `1` = "PRESENT")
  
  pa_rf <- Filter(var, pa_rf)
  
  ## ~~~~~~~~~~~~~~~~ ##
  ## Fit P/A RF model ##
  ## ~~~~~~~~~~~~~~~~ ##
  #Randomly select 75% of the data to enable 10 fold cross validation
  selection <- createDataPartition(y = pa_rf[, "PRESENCE"],
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
                                       summaryFunction = twoClassSummary,
                                       allowParallel = TRUE,
                                       verboseIter = TRUE)
  
  cluster <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
  doParallel::registerDoParallel(cluster)
  
  wf_pa <- caret::train(x = trainX,
                        y = trainY,
                        method = "rf",
                        tuneGrid = tunegrid,
                        trControl = train_control,
                        weights = h,
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
  
  png(paste0(name, "_roc.png"))
  plot(myroc, print.thres = "best")
  dev.off()
  
  ##adjust optimal cut-off threshold for class probabilities
  threshold <- pROC::coords(myroc, x="best", best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
  predCut <- factor( ifelse(predRoc[, "PRESENT"] > threshold, "PRESENT", "ABSENT") )
  
  curConfusionMatrix <- confusionMatrix(predCut, pa_rf_test$PRESENCE, positive = "PRESENT")
  
  log_con <- file(log_name, open = "a")
  cat(paste0("PA rf completed. Accuracy = ", 
             round(curConfusionMatrix$overall[1], 2), ", and Kappa = ",
             round(curConfusionMatrix$overall[2], 2), "/n"), 
      file = log_con)
  close(log_con)
  
  rf_name <- paste0(name, "_pa")
  saveRDS(assign(value = wf_pa, x = rf_name), paste0(rf_name, ".rds"))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Split data for Biomass model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  az_data <- all_dat_sac[-data_partition$selection[data_partition$SVSPP == sp.i] &
                           all_dat_sac$SVSPP == sp.i,]
  
  # Add the probability of occurance to the Abundance data
  az_data$PRESPROB <- predict(wf_pa, newdata = az_data, type = "prob")[, 2]
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Fit BIOMASS VSURF model ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  az_y <- az_data[, "BIOMASS"]
  az_x <- az_data[, !colnames(az_data) %in% c("LAT", "LON", "BIOMASS",
                                              "PRESENCE", "ABUNDANCE", "SAC.az",
                                              "SAC.pa", "name", "SVSPP")]
  ## A few columns are all zeros... might as well remove columns with identical values, too
  az_x <- Filter(var, az_x)
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Now we fit the biomass VSURF model with ", 
             nrow(az_x), " observations and ",
             ncol(az_x), " explanatory variables./n"), 
      file = log_con)
  close(log_con)
  
  ## VSURF is a data-driven approach to select important variables from high dimensional datasets
  az_url <- vsurf_bb(file_x = az_x,
                     file_y = az_y,
                     ntree = 200,
                     mtry = max(floor(ncol(az_x)/3), 1),
                     nfor_thres = 50,
                     nmin = 5,
                     nfor_interp = 25,
                     nsd = 1,
                     nfor_pred = 25,
                     parallel = "true",
                     ncores = 15,
                     clusterType = "PSOCK",
                     seed = 627,
                     name = paste0(name, "_AZ"),
                     username = "largesi",
                     token = "1ba80d8d-15ea-4ff1-9420-f8c97ea15d40-843339462")
  
  stop_condition_fail <- stop_condition_success <- FALSE
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Sent to BB and will monitor: ", az_url, " \nevery 10 minutes.\n"), file = log_con)
  close(log_con)
  
  
  ## CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION ##
  while (!stop_condition_fail && !stop_condition_success){
    
    log_con <- file(log_name, open = "a")
    cat(paste0("Checking in at ", Sys.time(), ".\n"), file = log_con)
    close(log_con)
    
    ## CHECK THE STATUS URL ##
    out1 <- GET(url = az_url, config = c(authenticate(username, token, type = "basic")), handle = NULL, timeout(3600))
    # cat(as.character(content(out1, encoding = "UTF-8")))
    stop_condition_success <- grepl("ProcessSucceeded", as.character(content(out1, encoding = "UTF-8")))
    stop_condition_fail <- grepl("Exception", as.character(content(out1, encoding = "UTF-8")))
    
    ## SLEEP FOR 10 MINUTES BEFORE THE NEXT CHECK ##
    Sys.sleep(10*60)
  }
  
  if(stop_condition_success) {
    output_az_url <- xml_text(xml_find_all(read_xml(out1), ".//d4science:Data"))
    output_az_df <- data.frame(NAME = paste0(name, "_AZ"),
                               LOG = output_az_url[1],
                               OUTPUT = output_az_url[2],
                               stringsAsFactors = FALSE)
    
    stop_condition_success <- FALSE
    
    log_con <- file(log_name, open = "a")
    cat("Success! ", name, "is all finished and output files can be found: ", output_az_url, "\n\n", file = log_con)
    close(log_con)
  }
  if(stop_condition_fail) {
    log_con <- file(log_name, open = "a")
    cat("Shoot! ", name, " failed.", "\n\n", file = log_con)
    close(log_con)
    next
  }


  download.file(output_az_df$LOG,
                paste0(name, "_AZ_VSURF_log.txt"),
                mode = "wb",
                cacheOK = FALSE)
  download.file(output_az_df$OUTPUT,
                paste0(name, "_AZ_VSURF_output.rds"),
                mode = "wb",
                cacheOK = FALSE)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Extract important variables ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  temp_vsurf <- readRDS(paste0(name, "_AZ_VSURF_output.rds"))
  az_rf_vars <- names(az_x)[temp_vsurf$varselect.interp]
  
  log_con <- file(log_name, open = "a")
  cat("Biomass VSURF model identified ", az_rf_vars, " as the best variables for interpretation.\n", file = log_con)
  close(log_con)
  
  az_rf <- az_data[, c("BIOMASS", az_rf_vars, "LAT", "LON")]
  
  az_rf <- Filter(var, az_rf)
  
  #Randomly select 75% of the data to enable 10 fold cross validation
  az_selection <- createDataPartition(y = az_rf[, "BIOMASS"],
                                      p = 0.75,
                                      list = FALSE)
  
  az_train <- az_rf[az_selection,]
  az_test<- az_rf[-az_selection,]
  
  az_trainX <- az_train[, names(az_train) %in% az_rf_vars]
  az_trainY <- az_train[, "BIOMASS"]
  
  strt_time <- Sys.time()
  train_control <- caret::trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       number = 10,
                                       search = "random",
                                       allowParallel = TRUE,
                                       verboseIter = TRUE)
  
  cluster <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cluster)
  
  rf_az <- caret::train(x = az_trainX,
                        y = az_trainY,
                        method = "rf",
                        # tuneGrid = tunegrid,
                        trControl = train_control,
                        importance = TRUE,
                        # weights = h,
                        # metric = "RMSE",
                        preProc = c("center", "scale"))
  
  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()
  
  stp_time <- Sys.time()
  stp_time - strt_time
  
  log_con <- file(log_name, open = "a")
  cat(paste0("Biomass rf completed. R2 = ", 
             round(tail(rf_az$finalModel$rsq, 1), 2), ", and RMSE = ",
             round(tail(sqrt(rf_az$finalModel$mse), 1), 2), "/n"), 
      file = log_con)
  close(log_con)
  
  rf_name <- paste0(name, "_biomass")
  saveRDS(assign(value = rf_az, x = rf_name), paste0(rf_name, ".rds"))
}

## The stuff below is used for plotting, and model validation

az_test$PREDICTED <- predict(wf_az, az_test)
az_test$RESIDUALS <- az_test$BIOMASS - az_test$PREDICTED

az_moran_dat <- az_test %>% 
  select(LAT, LON, PREDICTED, BIOMASS, RESIDUALS) %>% 
  na.omit

az_corr_res <- ncf::correlog(x = az_moran_dat$LAT , y = az_moran_dat$LON, z = az_moran_dat$RESIDUALS,
                             increment = 1, resamp = 500, latlon = TRUE)
# plot(az_corr_res)

az_corr_raw <- ncf::correlog(x = az_moran_dat$LAT , y = az_moran_dat$LON, z = az_moran_dat$BIOMASS,
                             increment = 1, resamp = 500, latlon = TRUE)


rf_rmse <- round(sqrt(tail(rf_az$finalModel$mse, 1)), 2)

pl1 <- ggplot(az_test, aes(x = PREDICTED, y = BIOMASS)) +
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, col = "grey40", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, col = "grey40") +
  theme_minimal() +
  labs(title = "",
       subtitle = paste0("RMSE = ", rf_rmse),
       x = expression(Model~predicted~biomass~(log[10])),
       y = expression(Observed~biomass~(log[10]))) +
  theme(aspect.ratio = 1) 

us <- map_data("state")

data(nw.atlantic)
atl <- as.bathy(nw.atlantic)

gg <- ggplot()
gg <- gg + geom_map(data = us, map = us,
                    aes(x = long, y = lat,
                        map_id = region),
                    fill="grey70", color="grey90", size=0.15) + 
  geom_contour(data = atl, aes(x = x, y = y, z = z), color = "grey20", size = 0.1) +
  coord_map(xlim = c(-80, -65),
            ylim = c(37.5, 47.5)) +
  theme_minimal()

pl2 <- gg + 
  geom_point(data = az_test, aes(x = LON, y = LAT, col = PREDICTED), alpha = 0.7, size = 0.75) +
  scale_color_viridis() +
  labs(x = "", y = "",
       title = "Haddock",
       subtitle = expression(Predicted~biomass~(log[10])),
       color = "") +
  theme(legend.position = "none")

had_fit <- cowplot::plot_grid(pl2, pl1, labels = NULL, align = 'hv', axis = "b")

ggsave(had_fit, filename = "haddock_fit.pdf", height = 210, width = 297, units = "mm")

# plot(az_test$RESIDUALS, )
# plot(az_test$PREDICTED, az_test$BIOMASS-az_test$PREDICTED)

# Use model to make probabilistic predictions
probability.pa.pred <- predict(wf_abund, newdata = testing.data, "raw")
probability.pa.pred <- data.frame(matrix(unlist(probability.pa.pred),
                                         nrow = dim(testing.data)[1],
                                         byrow = T)) 

table(testing.data$PRESENCE, probability.pa.pred) 

# This will produce a 2 column data frame with both probability of absence and presence.
# These values of probability of occurence can then be used to calculate AUC and calibration curves.

wf_latlong <- wf_dat %>% 
  select(-one_of(na_count$COLS[na_count$NAs >= na_nums]),
         -one_of(join_names),
         LAT, LON) %>% 
  # mutate(PRESENCE = as.factor(recode(PRESENCE, `1` = "present", `0` = "absent"))) %>%
  na.omit %>% 
  as.data.frame


# Make predictions on the scale of the response variable, in this case the binary presence-absence scale
testing.data$PREDICTED <- predict(wf_abund, newdata = testing.data, type="raw") 

# These predicted response values can then be used to calculate kappa values
testing.data$residuals <- as.numeric(as.character(testing.data$PRESENCE)) - as.numeric(as.character(testing.data$PREDICTED))

# x = predicted, y = observed
ggplot(testing.data, aes(x=PREDICTED, y = ABUNDANCE)) +
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, col = "grey40", se = FALSE) +
  geom_abline(slope = 1, intercept = 1, linetype = 2, col = "grey40") +
  theme_minimal() +
  theme(aspect.ratio = 1)+
  coord_fixed()

td <- varImp(wf_abund, scale = FALSE)

td_var <- data.frame(VAR = row.names(td),
                     IMP = td$Overall, 
                     stringsAsFactors = FALSE)

saveRDS(wf_abund, "wf_abund.rds")



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


