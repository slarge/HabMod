#rm(list = ls())
cat("start log\n")
strt_time <- Sys.time()
cat(paste0("Initiating the service at: ", strt_time, ".\n"))
# Load libraries
libs = c("VSURF", "parallel", "doParallel")
# library(VSURF)
# devtools::install_github("slarge/VSURF")
library(VSURF)
library(parallel)
library(doParallel)

if(all(libs %in% .packages())){
  cat("The following packages are loaded: ", libs, ".\n")
}
if(!any(libs %in% .packages())){
  cat("The following packages are not loaded: ", libs[!libs %in% .packages()], ".\n")
}

#parameters
false <-F
true <-T
# x = "x.txt"
x = "witch_flounder_PA_x.txt"
# download.file(x, "x.txt", mode = "wb", cacheOK = FALSE)
# download.file(y, "y.txt", mode = "wb", cacheOK = FALSE)
x = read.table(x, header = T, sep = ",")
# y = "y.txt"
y = "witch_flounder_PA_y.txt"
y = read.table(y, header = T, sep = ",")
ntree = 1000
mtry = max(floor(ncol(x)/3), 1)
nfor.thres = 3
nmin = 1
nfor.interp = 25 
nsd = 1
nfor.pred = 25
parallel = true
ncores = parallel::detectCores() - 1
clusterType = "FORK"
seed = 627
name = "ranger_test"

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

# cluster <- parallel::makeCluster(ncores) # convention to leave 1 core for OS
# doParallel::registerDoParallel(cluster)

# cat("Now we're starting the VSURF process with", ncores, "cores.\n")
dat.vsurf <- VSURF::VSURF(x = x,
                          y = y,
                          ntree = ntree, 
                          mtry = mtry,
                          nfor.thres = nfor.thres, 
                          nmin = nmin, 
                          nfor.interp = nfor.interp, 
                          nsd = nsd,
                          nfor.pred = nfor.pred,
                          parallel = parallel, 
                          ncores = ncores,
                          clusterType = clusterType)
# cat("VSURF finished, now stopping ")
# parallel::stopCluster(cluster)
# cat("the cluster\n")
# foreach::registerDoSEQ()
summary(dat.vsurf)


output_file <- paste0(name, "_output.rds")
saveRDS(dat.vsurf, output_file)
rm(dat.vsurf)
cat("Just to make sure we saved everything:")
summary(readRDS(output_file))
plot(dat.vsurf)
names(x)[dat.vsurf$varselect.interp]

output_file <- paste0(name, "_output_", Sys.time(), ".rds")
output_file <- gsub(" ", "_", output_file)
output_file <- gsub(":", "_", output_file)

