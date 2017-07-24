rm(list = ls())

strt_time <- Sys.time()
cat(paste0("Initiating the service at: ", strt_time, ".\n"))
# Load libraries
libs = c("caret", "parallel", "doParallel", "party")
library(caret)
library(parallel)
library(doParallel)
library(party)

if(all(libs %in% .packages())){
  cat("The following packages are loaded: ", libs, ".\n")
}
if(!any(libs %in% .packages())){
  cat("The following packages are not loaded: ", libs[!libs %in% .packages()], ".\n")
}

# parameters
false <- F
true <- T
x = "x.txt"
x = read.table(x, header = T, sep = ",")
y = "y.txt"
y = read.table(y, header = T, sep = ",")
ntree = 500
parallel = false
ncores = parallel::detectCores() - 1
name = "test"

if(length(x) >= 1 &
   length(y) >= 1) {
  cat(paste0("You have loaded a file with ", nrow(x), " observations of ", ncol(x), " predictors."))
}
if(length(x) < 1 |
   length(y) < 1){
  cat("x or y did not load correctly.")
}

if(ncol(y) == 1) {
  y = y[,1]
}

if(length(unique(y)) == 2) {
  y = as.factor(y)
}

if(is.null(ncores)) {
  cat("Your number of cores is... wonky.\n")
} else {
  cat("You have", ncores, "cores ready for action.\n")
}

tune_grid <- expand.grid(mtry = seq(2, 16, 2))

train_control <- caret::trainControl(method = "cv",
                                     number = 10,
                                     allowParallel = parallel,
                                     verboseIter = TRUE)

cluster <- makeCluster(ncores) # convention to leave 1 core for OS
registerDoParallel(cluster)


cat("Now we're starting the VSURF process with", ncores, "cores.\n")
wf_abund <- caret::train(x = trainX,
                         y = trainY,
                         method = "cforest",
                         tuneGrid = tune_grid,
                         trControl = train_control,
                         preProc = c("center", "scale"),
                         controls = cforest_unbiased(ntree = ntree))

# Stop the parallel processing
cat("train finished, now stopping ")
parallel::stopCluster(cluster)
cat("the cluster")
foreach::registerDoSEQ()

output_file <- paste0(name, "_output_train.rds")
saveRDS(dat.vsurf, output_file)
rm(dat.vsurf)
cat("Just to make sure we saved everything:")
summary(readRDS(output_file))

stp_time <- Sys.time()
stp_time - strt_time
