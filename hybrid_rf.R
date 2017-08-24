rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(party)
library(caret)
library(parallel)
library(doParallel)
# library(e1071)

wd <- ("~/slarge/HabMod/data/")

# First get the species of interest
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
         COMNAME = gsub("acadian", "Acadian", COMNAME))

data_files <- list.files(path = wd, full.names = FALSE)
data_files <- data_files[!grepl("csv", data_files)]
data_files <- "spring.data.RData"

lapply(paste0(wd, data_files), load, .GlobalEnv)

spring_dat <- spring.data %>% 
  filter(SVSPP %in% unique(stock_list$SVSPP)) %>% 
  select(-TOW,
         -CATCHSEX) %>% 
  distinct(.keep_all = TRUE)

names(spring_dat) <- sub(" ", "", names(spring_dat))

rm(list = c("spring.data", "svspp_dat", "data_files"))

# Fit full model including all habitat covariates and the abundance based SAC term
# An unbiased framework is used here because predictor variables differ on their scale of measurement 

# # First, for each species ID stations where we expect presence/absence
# # would it be a better idea to create a train/test subset for both modeling routines?

# Each row is an observation, so count the number of each species per year, strata, and station sampled.
# Spread by each species, so NAs are created where species aren't present for a year, stratum, and station.
# Replace NAs with 0 and reorganize into a long data frame by year, stratum, and station.
# Sum the number of observations per year, species, and stratum. If a species is not found in a year, stratum, station
# but is found in that year stratum, they are considered "absent" or 0. If a species is not found in a year, stratum, station
# nor is found in that year stratum, they are NA and removed.

# pa_table <- spring_dat %>%
#   group_by(YEAR, SVSPP, STRATUM, STATION) %>%
#   summarize(count = n()) %>% 
#   tidyr::spread(SVSPP, value = count) %>%
#   mutate_all(funs(replace(., is.na(.), 0))) %>%
#   tidyr::gather(SVSPP, value = count, -YEAR, -STRATUM, -STATION) %>%
#   group_by(YEAR, SVSPP, STRATUM) %>% 
#   mutate(stratum_sum = sum(count), # 
#          PRESENCE = case_when(count == 0 & stratum_sum >= 1 ~ 0, # Not at station but in stratum
#                               count == 1 & stratum_sum >= 0 ~ 1, # Present
#                               count == 0 & stratum_sum == 0 ~ NA_real_, # Not at station or in stratum
#                               TRUE ~ NA_real_)) %>% 
#   filter(!is.na(PRESENCE))
# 
# # Create a data.frame of just the station data
# station_dat <- spring_dat %>% 
#   select(-SVSPP,
#          -ABUNDANCE,
#          -BIOMASS) %>% 
#   distinct(.keep_all = TRUE)
# 
# # join the p/a data with the station data
# pa_dat <- pa_table %>%
#   ungroup %>% 
#   mutate(SVSPP = as.numeric(SVSPP)) %>% 
#   left_join(station_dat, by = c("YEAR", "STRATUM", "STATION")) %>% 
#   dplyr::select(-stratum_sum, -count)
# 
# na_dat <- pa_dat %>%
#   group_by(YEAR) %>% 
#   summarize_all(funs( sum(is.na(.))/n())) %>%
#   as.data.frame 

# Hyprid RF:
# For each SVSPP, split into training and test df

METRIC <- "ABUNDANCE"
sp_name = 107
seed <- 627

wf_dat <- spring_dat %>% 
  filter(SVSPP == sp_name) %>% 
  select(-zoo_spr_clim_penilia,
         -zoo_spr_clim_evadnespp,
         -zoo_spr_clim_salps)

# 
# # This subset is for final testing of the hybrid model
# selection_1 <- sample(1:nrow(wf_dat), .75 * nrow(wf_dat))
# wf_train <- wf_dat[selection_1,]
# wf_test <- wf_dat[-selection_1,]
# 

# selection_2 <- sample(1:nrow(wf_train), .75 * nrow(wf_dat))
# wf_lt <- wf_dat[selection,]
# wf_st <- wf_dat[selection,]

# 
join_names <- c("CRUISE6", "STRATUM", "STATION", "SVVESSEL", "YEAR", "SEASON", "LAT",
                "LON", "EST_TOWDATE", "DEPTH", "DOY", "SVSPP")
# # Select columns with 
# q_25 <- quantile(colSums(na_dat[, !colnames(na_dat) %in% c(join_names, "ABUNDANCE", "BIOMASS")]))[2]
# lt <- colnames(na_dat[, !colnames(na_dat) %in% c(join_names, "ABUNDANCE", "BIOMASS")])[colSums(na_dat[, !colnames(na_dat) %in% c(join_names, "ABUNDANCE", "BIOMASS")]) <= q_25]
# # lt <- lt[!lt %in% c(join_names, "PRESENCE")]
# 
# st <- colnames(na_dat)[!colnames(na_dat) %in% c(lt, join_names, "ABUNDANCE", "BIOMASS")]
# # st <- st[!st %in% c(join_names, "PRESENCE")]
# 
# lt_names <- c(join_names, lt, "ABUNDANCE")
# st_names <- c(join_names, st, "ABUNDANCE")
# 
# wf_lt <- wf_train %>% 
#   select(!!lt_names) %>% 
#   na.omit
# 
# wf_st <- wf_train %>% 
#   select(!!st_names) %>% 
#   na.omit
# 
# rf_df <- pa_lt %>% 
#   filter(SVSPP == sp_name) %>%
#   select(!!c("PRESENCE", lt)) %>% 
#   mutate(PRESENCE = as.factor(recode(PRESENCE, `1` = "present", `0` = "absent"))) %>%
#   as.data.frame() %>%
#   na.omit
# 


# Count of the number of NAs in each column for each year
na_dat <- wf_dat %>%
  select(-one_of(join_names[!join_names %in% c("YEAR")]),
         -BIOMASS) %>% 
  group_by(YEAR) %>%
  summarize_all(funs(sum(is.na(.))))


na_count <- data.frame(COLS = colnames(!na_dat[,-1]),
                       NAs = round(colSums(na_dat[,-1]), 2),
                       row.names = NULL, 
                       stringsAsFactors = FALSE)

# Return the complete cases dimensions
dim_maker <- function(x){
  dd <- wf_dat %>% 
    select(-one_of(na_count$COLS[na_count$NAs >= x]),
           -one_of(join_names[!join_names %in% c("YEAR")]),
           -BIOMASS) %>% 
    na.omit
 dd <- data.frame(NCOL = ncol(dd),
            NROW = nrow(dd),
            NAs = x)
  return(dd)
}

na_select <-  data.frame(do.call("rbind",
                                 lapply(unique(na_count$NAs), function(x) dim_maker(x))),
                         stringsAsFactors = FALSE)

na_select$sum_NAs <- scale(na_select$NCOL) + scale(na_select$NROW)

max_ncols <- na_select$NCOL[na_select$sum_NAs == max(na_select$sum_NAs)]
max_nrows <- na_select$NROW[na_select$sum_NAs == max(na_select$sum_NAs)]
na_nums <- na_select$NAs[na_select$sum_NAs == max(na_select$sum_NAs)]

# ggplot(na_select, aes(x = n_columns, y = complete_cases)) +
#   geom_point() +
#   geom_vline(xintercept = max_ncols,
#              col = "red") +
#   theme_bw() +
#   labs(y = "Complete cases (n)",
#        x = "Columns (n)",
#        title = paste0("Max complete cases = ", max_nrows , "\nMax columns = ", max_ncols))

wf_lt <- wf_dat %>% 
  select(-one_of(na_count$COLS[na_count$NAs >= na_nums]),
         -one_of(join_names),
         -BIOMASS) %>% 
  na.omit

#Randomly select 75% of the data to enable 10 fold cross validation
selection <- createDataPartition(y = wf_lt[, METRIC],
                                 p = 0.75,
                                 list = FALSE)

training.data <- wf_lt[selection,]
testing.data<- wf_lt[-selection,]

trainX <- training.data[, -1]
trainY <- training.data[, METRIC]

strt_time <- Sys.time()
# cluster <- makeCluster(11 - 1) # convention to leave 1 core for OS
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

train_control <- caret::trainControl(method = "cv",
                                     number = 5,
                                     returnResamp = "none",
                                     allowParallel = TRUE,
                                     verboseIter = TRUE)

wf_abund <- caret::train(x = trainX,
                         y = trainY,
                         method="cforest",
                         trControl = train_control,
                         preProc = c("center", "scale"),
                         controls = cforest_unbiased(ntree = 1000))

# Stop the parallel processing
stopCluster(cluster)
registerDoSEQ()

stp_time <- Sys.time()
stp_time - strt_time

tt <- wf_abund$finalModel

# Save model

# Save performance

# Save VI

confusionMatrix(testing.data$PREDICTED, testing.data$PRESENCE, positive = "Present")
postResample(pred = testing.data$PREDICTED, obs = testing.data$PRESENCE)
wf_varImp <- varImp(wf_abund, scale = TRUE)

twoClassSummary(testing.data, lev = levels(testing.data$PRESENCE))

print(rf_default)

testing.data$PREDICTED <- predict(wf_abund, OOB = TRUE, type = "prob", newdata = testing.data)
testing.data$PREDICTED <- predict(wf_abund, OOB = TRUE, type = "raw", newdata = testing.data)

probability.pred <- data.frame(matrix(unlist(probability.pred), 
                                      nrow = dim(testing.data)[1],
                                      byrow=T))

plot(wf_varImp)
plot(wf_abund)
testing.data$PREDICTED <- predict(tt, testing.data, OOB = TRUE, type = "response")

table(testing.data$PRESENCE, testing.data$PREDICTED)

library(edarf)
pd <- partial_dependence(wf_abund$finalModel)


# ggplot(testing.data, aes(PREDICTED, PRESENCE))+
#   geom_point(colour = "black")+
#   labs(x = "Predicted presence", y = "Observed presence")+
#   ggtitle(bquote(~"Witch flounder"))+
#   geom_abline(slope = 1, intercept = 0)+
#   theme_bw()+
#   theme(aspect.ratio=1)+
#   theme(plot.title=element_text(size=20, vjust=0),
#         axis.text.x=element_text(angle=50, size=10, vjust=1, hjust=1),
#         axis.text.y=element_text(size=10))

