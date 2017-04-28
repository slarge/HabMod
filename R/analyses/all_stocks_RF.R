rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(biomod2)
library(viridis)
library(gridExtra)
library(caret)
# library(vtreat)
# library(tidyverse)
library(maps)
library(mapproj)
# library(sf)
# library(tableplot)
# library(party)
# library(randomForestSRC)
# library(ggRandomForests)
# library(rgdal)
# library(ranger)
# devtools::install_github("btupper/fishboxes")
# library(fishboxes)


wd <- ("~/git/slarge/hab_modeling/data/")
# dest <- tempdir()
# dir(dest)
# map_dest <- paste0(wd, "maps")
# ok <- download_nefsc_gis("EcoMon_Strata", dest = dest)
# 
# P <- readOGR(path.expand(dest), "EcoMon_Strata")

# plot(P)

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
  mutate(COMNAME = toupper(COMNAME)) %>% 
  left_join(svspp_dat, by = "COMNAME")

data_files <- list.files(path = wd, full.names = FALSE)
data_files <- data_files[!grepl("csv", data_files)]

lapply(paste0(wd, data_files), load, .GlobalEnv)

data_names <- gsub("\\..*$", "", tolower(data_files))
hab_names <- data_names[!grepl("survdat", data_names)]

col_names <- lapply(hab_names, function(x) colnames(get(x)))

# The fancy piece of footwork below takes the list of data files and joins based on intersecting names
join_names <- Reduce(intersect, col_names)

full_dat <- purrr::map(hab_names, get) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = join_names), .)

# clean up a bit
rm(list = hab_names)

# # First, for each species ID stations where we expect presence/absence
# # would it be a better idea to create a train/test subset for both modeling routines?

# Filter for the species of interest. Each row is an observation, 
# so count the number of each species per year, strata, and station sampled.
# Spread by each species, so NAs are created where species aren't present
# for a year, stratum, and station.
# Replace NAs with 0 and reorganize into a long data frame by year, stratum, and station.
# Sum the number of observations per year, species, and stratum. If less than 5 individuals 
# are found in a strata each year, they are considered "absent" or 0. Greater than 5 indicates
# presence. The threshold value could be anything.

pa_table <- survdat %>%
  filter(SVSPP %in% unique(stock_list$SVSPP)) %>%
  group_by(YEAR, SVSPP, STRATUM, STATION) %>%
  summarize(count = n()) %>% 
  spread(SVSPP, value = count) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  gather(SVSPP, value = count, -YEAR, -STRATUM, -STATION) %>%
  group_by(YEAR, SVSPP, STRATUM) %>% 
  mutate(ssum = sum(count),
         PRESENCE = sum(count, na.rm = TRUE),
         PRESENCE = ifelse(PRESENCE >= 1,
                     1,
                     0))

stat_dat <- survdat %>% 
  dplyr::select(-CATCHSEX, -SVSPP, -LENGTH,
         -ABUNDANCE, -BIOMASS, -NUMLEN) %>% 
  distinct(.keep_all = TRUE)

pa_dat <- pa_table %>%
  ungroup() %>% 
  mutate(SVSPP = as.numeric(SVSPP)) %>% 
  left_join(stat_dat, by = c("YEAR", "STRATUM", "STATION")) %>% 
  left_join(svspp_dat, by = "SVSPP") %>% 
  left_join(full_dat, by = c("YEAR", "STRATUM", "STATION",
                             "CRUISE6", "TOW", "SEASON",
                             "LAT", "LON", "EST_TOWDATE")) %>% 
  dplyr::select(-ssum, -count)

source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
corr_names <- colnames(pa_dat)[!colnames(pa_dat) %in% c(join_names, "SVSPP", "SVVESSEL",
                                                        "COMNAME")]

mon_names <- grep("month", corr_names, value = TRUE)
mon_names <- mon_names[!mon_names %in% c("chl_9km_month_1", "chl_f_9km_month_1", 
                                         "sst_4km_month_1", "sst_f_9km_month_1")]
corr_names <- corr_names[!corr_names %in% mon_names]

corr_dat <- pa_dat %>%
  filter(COMNAME == "witch flounder") %>% 
  dplyr::select_(.dots = corr_names)
  
corr_plot <- ggcorr(corr_dat, geom = "blank", label = TRUE, hjust = .75, size = 3, layout.exp = 4) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) >= 0.7)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

na_dat <- pa_dat %>%
  filter(COMNAME == "witch flounder") %>%
  dplyr::select_(.dots = c("YEAR", corr_names)) %>% 
  slice_rows(.cols = "YEAR") %>%
  summarize_each(funs( sum(is.na(.))/n())) %>%
  as.data.frame %>%
  gather(YEAR)

colnames(na_dat) <- c("YEAR", "variable", "value")

na_plot <- ggplot(data = na_dat, aes(x = YEAR, y = variable,
                                     color = value)) +
  geom_point(alpha = 0.9) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 6)) +
  scale_color_gradient(low = "white", high = "#56B1F7")

grid.arrange(na_plot, corr_plot, ncol = 2)


# SET UP THE MODELS #
sp_name <- "witch flounder"

sdm_vars <- c(join_names, "PRESENCE",
              "necrm_vrm", "necrm_bpi", "namera_vrm",
              "namera_bpi", "gdepth", "DEPTH", 
              "complexity", "bpi_3_25_layer", "bpi_30_250_layer",
              "BOTTEMP", "SURFTEMP", "soft_sed",
              "seabedforms", "rugosity")

rf_vars <-  c(sdm_vars, "chl_f_9km_month_1", "chl_9km_month_1",
              "BOTSALIN", "SURFSALIN", "sst_f_9km_month_1",
              "sst_4km_month_1", "ABUNDANCE")

wf_dat <- pa_dat %>% 
  filter(COMNAME == quote(sp_name)) %>% 
  select_(.dots = sdm_vars) %>% 
  as.data.frame() %>% 
  na.omit()

selection <- sample(1:nrow(wf_dat), 0.5 * nrow(wf_dat))

# Use random selection to create training and testing data subsets
wf_dat_train <- wf_dat[selection, ]
wf_dat_test <- wf_dat[-selection, ]

# Tell biomod2 which parts of the database refer to which biomod2 object
myRespName <- sp_name
myResp <- wf_dat_train[, "PRESENCE"]
myRespXY <- wf_dat_train[,c("LON", "LAT")]
myExpl <- wf_dat_train[, setdiff(names(wf_dat_train), c("PRESENCE","LAT", "LON", join_names))]

myEvalResp <- wf_dat_test[, "PRESENCE"]
myEvalExpl <- wf_dat_test[, setdiff(names(wf_dat_test), c("PRESENCE","LAT", "LON", join_names))]
myEvalXY <- wf_dat_test[, c("LON", "LAT")]


myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     eval.resp.var = myEvalResp,
                                     eval.expl.var = myEvalExpl,
                                     eval.resp.xy = myEvalXY,
                                     PA.nb.rep = 0)

# Set the options that you have chosen for the different model algorithms
# - here using defaults (empty arguments) 
myBiomodOption <- BIOMOD_ModelingOptions()

# Run the biomod2 models that you have chosen on the data provided.
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('GLM', 'GBM', 'RF'),
  models.options = myBiomodOption,
  NbRunEval = 3,
  DataSplit = 70,
  Prevalence = 0.5,
  VarImport = 3,
  models.eval.meth = c('TSS', 'ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName, "_Species1", sep = ""))


########################### Evaluating models #############################
# Get evaluation statistics for each model
myBiomodModelEval <- get_evaluations(myBiomodModelOut, as.data.frame = FALSE)

# Display the model evaluation statistics
myBiomodModelEval["TSS", "Testing.data" ,,,]

myBiomodModelEval["ROC", "Testing.data",,,]

var_imp <- get_variables_importance(myBiomodModelOut)

df1 <- as.data.frame.table(var_imp) %>% 
  dplyr::rename(VARIABLE = Var1,
                MODEL = Var2,
                RUN = Var3,
                DATA = Var4,
                VALUE = Freq) %>% 
  group_by(VARIABLE, MODEL) %>% 
  summarize(MEAN = mean(VALUE),
            lower = MEAN - sd(VALUE),
            upper = MEAN + sd(VALUE)) %>% 
  group_by(MODEL) %>% 
  arrange(MEAN) %>% 
  mutate(VARIABLE = factor(VARIABLE, VARIABLE))

ggplot(df1, aes(x = VARIABLE, y = MEAN, color = MODEL)) +
  geom_point() +
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point")

# myGLMModels <- BIOMOD_LoadModels(myBiomodModelOut, models=c('GLM'))
# 
# myRespPlotGLM <- response.plot2(models  = myGLMModels,
#                                 Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                 show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                 do.bivariate = FALSE,
#                                 fixed.var.metric = 'median',
#                                 col = c("red", "blue", "green"),
#                                 legend = TRUE,
#                                 data_species = get_formal_data(myBiomodModelOut,'resp.var'))


# myRFModels <- BIOMOD_LoadModels(myBiomodModelOut, models=c('RF'))
# 
# myRespPlotRF <- response.plot2(models  = myRFModels,
#                                 Data = get_formal_data(myBiomodModelOut,'expl.var'), 
#                                 show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
#                                 do.bivariate = FALSE,
#                                 fixed.var.metric = 'median',
#                                 col = c("red", "blue", "green"),
#                                 legend = TRUE,
#                                 data_species = get_formal_data(myBiomodModelOut,'resp.var'))
scores_TSS <- as.numeric(myBiomodModelEval["TSS","Evaluating.data",,,])

## select a threshold to keep a single model
score_thresh <- mean(tail(sort(scores_TSS), 2))


# Build ensemble model from selected models.
myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                        # chosen.models = c("witch.flounder_AllData_RUN1_RF",
                                        #                   "witch.flounder_AllData_RUN2_RF",
                                        #                   "witch.flounder_AllData_RUN3_RF"),
                                        chosen.models = "all",
                                        em.by = 'all',
                                        eval.metric = c('TSS'),
                                        eval.metric.quality.threshold = NULL,
                                        prob.mean = TRUE,
                                        prob.cv = FALSE,
                                        prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                        prob.median = FALSE,
                                        committee.averaging = FALSE,
                                        prob.mean.weight = TRUE,
                                        prob.mean.weight.decay = 'proportional')


# Project selected models for later building ensemble model
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  xy.new.env = myRespXY,
  proj.name = 'current',
  # selected.models = c("witch.flounder_AllData_RUN1_RF",
  #                      "witch.flounder_AllData_RUN2_RF",
  #                      "witch.flounder_AllData_RUN3_RF"),
  selected.models = "all",
  binary.meth = 'TSS',
  compress = 'xz',
  build.clamping.mask = FALSE)

# get_evaluations(myBiomodEM)

# Project and plot predictions from the ensemble model 
EMplot <- BIOMOD_EnsembleForecasting(projection.output = myBiomodProj,
                                     EM.output = myBiomodEM)


wf_pred <- get_predictions(myBiomodEM, as.data.frame = TRUE, evaluation = TRUE)
colnames(wf_pred) <- c("mean", "wmean")
wf_pred <- data.frame(wf_dat_test, 
                      wf_pred)

wf_pred_plot <- bind_cols(
  wf_dat_test  %>% 
    mutate(PRESENCE = as.factor(recode(PRESENCE, `1` = "Present", `0` = "Absent"))),
  data.frame(wf_pred, stringsAsFactors = FALSE)
)


us <- map_data("state")

gg <- ggplot()
gg <- gg + geom_map(data = us, map = us,
                    aes(x = long, y = lat,
                        map_id = region),
                    fill="grey70", color="grey90", size=0.15) + 
  coord_map(xlim = c(-80, -60),
                     ylim = c(37.5, 47.5)) +
  theme_bw()


grid.arrange(
  # Predicted
  gg + geom_point(data = wf_pred_plot, aes(x = LON, y = LAT,
                                      # color = mean,
                                      color = mean),
                  alpha = 0.7, size = 0.75) +
    # scale_color_viridis() +
    scale_color_viridis() +
    labs(x = "longitude", y = "latitude",
         title = "Predicted probability of occurance",
         subtitle = "Ensemble model weighted mean occupancy for each survey station",
         color = "") +
    theme(legend.position = "bottom"),
  # Presence/Absence
  gg + geom_point(data = wf_pred_plot, aes(x = LON, y = LAT,
                                      # fill = as.factor(PRESENCE),
                                      color = as.factor(PRESENCE)),
                  alpha = 0.7, size = .75) +
    # scale_color_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
  labs(x = "longitude", y = "latitude",
       title = "Presence and absence",
       subtitle = "Absence is assumed when sp. not caught at a station is caught\nin the station's stratum during the survey year",
       color = "") +
    theme(legend.position = "bottom"),
  ncol = 2)

# remove all training data and then select based on the rf_vars
rf_vars <- rf_vars[!grepl("PRESENCE", rf_vars)]

rf_df <- survdat %>%
  left_join(svspp_dat) %>%  #, by = "SVSPP") %>% 
  filter(COMNAME == quote(sp_name)) %>% 
  left_join(full_dat) %>% 
  select_(.dots = rf_vars) %>% 
  as.data.frame() %>% 
  na.omit() %>% 
  anti_join(wf_dat_train) %>% 
  left_join(wf_pred)

rf_dat <- rf_df %>% 
  dplyr::select(-CRUISE6, -STRATUM, -TOW, -STATION,
         -YEAR, -SEASON, -LAT, -LON, -EST_TOWDATE,
         -mean, -PRESENCE)

outcome <- "ABUNDANCE"
rf_dat[[outcome]] <- as.numeric(rf_dat[[outcome]])

# identify variables
vars <- setdiff(colnames(rf_dat), outcome)

f <- paste(outcome, 
           paste(vars, collapse = ' + '), 
           sep = ' ~ ')

train_control <- trainControl(method = "cv", number = 10)
model <- train(form = as.formula(f), 
               data = rf_dat, 
               trControl = train_control, 
               method = "rf",
               importance = T)

plot(model$finalModel)
print(model)

predictions <- predict(model, rf_dat)
result <- data.frame(Actual = rf_dat$ABUNDANCE, Predicted = predictions)
result$Difference <- abs(result$Actual - result$Predicted)
summary(result$Difference) 
rf_dat$predictions <- predictions
rf_df$predictions <- predictions


ggplot(rf_dat, aes(predictions, ABUNDANCE))+
  geom_point(colour = "black")+
  labs(x = "Predicted abundance", y = "Observed abundance")+
  ggtitle(bquote(~"Witch flounder"))+
  geom_abline(slope = 1, intercept = 0)+
  theme_bw()+
  theme(aspect.ratio=1)+
  theme(plot.title=element_text(size=20, vjust=0),
        axis.text.x=element_text(angle=50, size=10, vjust=1, hjust=1),
        axis.text.y=element_text(size=10))


surv_var <- unique(colnames(survdat)[colnames(survdat %in% rf_df)]
colnames(survdat)
predicted_abundance <- survdat %>% 
  select_()
  # full_dat %>% 
  left_join(full_dat) %>%
  # survdat %>%
  # left_join(svspp_dat) %>%  #, by = "SVSPP") %>%
  # filter(COMNAME == quote(sp_name)) %>% 
  select_(.dots = rf_vars) %>%
  as.data.frame() %>%
  left_join(wf_pred) %>% 
  na.omit()
# anti_join(wf_dat_train) %>% 

predicted_abundance$dfPredictions <- predict(model, predicted_abundance, type = "raw")

gg <- ggplot()
gg <- gg + geom_map(data = us, map = us,
                    aes(x = long, y = lat,
                        map_id = region),
                    fill="grey70", color="grey90", size=0.15) + 
  coord_map(xlim = c(-80, -60),
            ylim = c(37.5, 47.5)) +
  theme_bw()

gg + geom_point(data = predicted_abundance, aes(x = LON, y = LAT, color = log(dfPredictions)),
                                         # fill = as.factor(PRESENCE),
                                         # color = predictions),
                alpha = 0.7, size = 2) +
  # scale_color_viridis(discrete = TRUE) +
  scale_color_viridis() +
  labs(x = "longitude", y = "latitude",
       title = "Predicted abundance",
       subtitle = "Random Forest with 10-fold cross validation",
       color = "log(abundance)") +
  theme(legend.position = "bottom")

gg

plot(varImp(model))

set.seed(1341)
Data <- witch_dat #%>% 
# filter(SEASON == "SPRING") %>%
  # mutate(BIOMASS =  log10(BIOMASS + 1),
  #        ABUNDANCE =  log10(ABUNDANCE + 1)) %>%
  # select(Abundance = ABUNDANCE,
  #        soft_sed,
  #        bpi_30_250_layer,
  #        DEPTH,
  #        BOTTEMP,
  #        chl_4km_month_1,
  #        chl_f_4km_month_1,
  #        sst_4km_month_1,
  #        sst_f_4km_month_1)
# 
# prepare outcome column and level
outcome <- "ABUNDANCE"
# positive <- "+"
Data[[outcome]] <- as.numeric(Data[[outcome]])
# Data[["SEASON"]] <- as.character(Data[["SEASON"]])

# identify variables
vars <- setdiff(colnames(Data), outcome)

# split into train and test/evaluation
isTrain <- runif(nrow(Data)) <= 0.8
dTrain <- Data[isTrain, , drop = FALSE]
dTest <- Data[!isTrain, , drop = FALSE]

f <- paste(outcome, 
           paste(vars, collapse = ' + '), 
           sep = ' ~ ')

# model <- ranger(as.formula(f),  
#                 probability = FALSE,
#                 data = dTrain)


treatmentsN <- vtreat::designTreatmentsN(dframe = dTrain,
                                         varlist = vars,
                                         outcomename = outcome)

dTrainTreated <- prepare(treatmentsN,
                          dTrain,
                          pruneSig = 0.99)
dTestTreated <- prepare(treatmentsN,
                         dTest, 
                         pruneSig = 0.99)

dTrainTreated <- dTrainTreated[grepl("clean", colnames(dTrainTreated))]
colnames(dTrainTreated) <- gsub("_clean", "", colnames(dTrainTreated))
dTrainTreated[[outcome]] <- dTrain[[outcome]]

dTestTreated <- dTestTreated[grepl("clean", colnames(dTestTreated))]
colnames(dTestTreated) <- gsub("_clean", "", colnames(dTestTreated))
dTestTreated[[outcome]] <- dTest[[outcome]]

library(ipred)
totest <- expand.grid(mtry = 1:4,
                      min.node.size = 1:11,
                      rep = 1)
ii = 1
la <- lapply(1:nrow(totest), function(ii) {
  ee <- errorest(as.formula(f),
                 mtry = totest$mtry[ii],
                 min.node.size = totest$min.node.size[ii],
                 model = ranger,
                 predict = function(object, newdata) 
                   predict(object, data = newdata)$predictions,
                 write.forest = TRUE,
                 data = dTrainTreated)
  cc <- c(mtry = totest$mtry[ii],
          min.node.size = totest$min.node.size[ii],
          error = ee$error)
  cc
})

sla <- do.call(rbind, la)
sla <- as.data.frame(sla)

rg.witch <- ranger(as.formula(f),  
                   importance = "impurity",
                   mtry = floor(sqrt(10)),
                   data = dTrainTreated,
                   write.forest = TRUE,
                   num.trees = 5000)

rg.witch$variable.importance

result <- getAndPlotMetric(rg.witch)
# ^ evaluated data ^
eval_data <- result$subtree_metrics
# second order vs first order plot
so_vs_fo <- result$so_vs_fo_plot
# number of splits vs first order plot
ns_vs_fo <- result$ns_vs_fo

plot(so_vs_fo)


combined_scores <- inner_join(eval_data %>% mutate(feature= rownames(.)), 
                              data.frame(VIMP = rg.witch$variable.importance) %>% 
                                mutate(feature= rownames(.)),
                              by = c("feature")) %>% 
  arrange(-VIMP)

knitr::kable(combined_scores)

plot(result$ns_vs_fo)

library(climbeR)

result <- getAndPlotMetric(rg.witch)
# evaluated data 
eval_data <- result$subtree_metrics
# another look at the result 
knitr::kable(eval_data)

pred.mod <- predict(model, data = dTestTreated)
table(dTestTreated$ABUNDANCE, pred.mod$predictions)



Data %>%
  ggplot(aes(x = log10(ABUNDANCE + 1))) +
  geom_histogram()


### Modeling ###
#Randomly select 90% of the data to enable 10 fold cross validation
selection <- sample(1:nrow(Data), 0.9*nrow(Data))

# Use random selection to create training and testing data subsets
training.data <- Data[selection,]
testing.data<- Data[-selection,]


########################
# 1. Abundance Modelling
########################

# Create a named vector that reflects the class midpoints 
a<-c("0", "5", "50","500","5000","50000","500000")
b<-as.list(a)
names(b)<-c("0", "1", "2","3","4","5","6") 

# A vector of weights based on their individual abundance classification was created for the training data set. 
# This enables the model to compensate for the high levels of data imbalance in some of the training data sets,
# e.g. where one of the abundance classes represented only a small minority of the overall data.  
# The predictive power of random forests, along with other classification algorithms, is reduced under such circumstances,
# as the model aims to minimise overall error rate by reducing attention on minority classes (Chen, Liaw & Breiman 2004).
# Within this algorithm, observations are sampled according to probabilities weights/sum weights. 

d <- ifelse(training.data$Abundance==0,
            1/(unname(table(training.data$Abundance)["0"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==1, 
            1/(unname(table(training.data$Abundance)["1"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==2,
            1/(unname(table(training.data$Abundance)["2"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==3,
            1/(unname(table(training.data$Abundance)["3"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==4,
            1/(unname(table(training.data$Abundance)["0"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==5,
            1/(unname(table(training.data$Abundance)["0"])/length(training.data$Abundance)),
     ifelse(training.data$Abundance==6,
            1/(unname(table(training.data$Abundance)["0"])/length(training.data$Abundance)),
            0))))))
     )

colnames(training.data)
plot(response.pred)


# Fit full model including all habitat covariates and the abundance based SAC term
# An unbiased framework is used here because predictor variables differ on their scale of measurement 
abundance.cforest <- cforest(Abundance ~ .,
                             data = training.data[, c("Abundance", "soft_sed", 
                                                      "bpi_30_250_layer", "DEPTH",
                                                      "BOTTEMP", "chl_4km_month_1",
                                                      "chl_f_4km_month_1", "sst_4km_month_1",
                                                      "sst_f_4km_month_1")],
                             weights = d,
                             scores = b,
                             controls = cforest_unbiased(ntree = 1000, 
                                                         mtry = 5))

# Use model to make probabilistic predictions
probability.pred <- predict(abundance.cforest, 
                            newdata = testing.data, type = "prob") 
probability.pred <- data.frame(matrix(unlist(probability.pred), 
                                      nrow = dim(testing.data)[1],
                                      byrow=T))

# Probability of occurrence is the sum of the non-zero predictions
# g<-dim(probability.pred)[2]
# ifelse(g<=2, probability.pred$presence<-(probability.pred[,2]),
#        ifelse(g>2, probability.pred$presence<-rowSums(probability.pred[,c(2:g)], 0)))
# These values of probability of occurence can then be used to calculate AUC and calibration curves

# Make predictions on the scale of the response variable, in this case the categorical abundance scale
response.pred <- predict(abundance.cforest, 
                         newdata = testing.data, type = "response") 
response.pred <- as.numeric(as.character(response.pred))
# These predicted response values can then be used to calculate weighted kappa values, 
# or can be converted to presence-absence data






