rm(list = ls())

# Load libraries
library(VSURF)
library(snow)
library(Rmpi)

false<-F
true<-T
removeZero<-false

#parameters
x = "x.txt"
x = read.table(x, header - T, sep = ",") # for test
y = "y.txt"
y = read.table(y, header - T, sep = ",") # for test
ntree = 500 
mtry = max(floor(ncol(x)/3), 1)
nfor.thres = 50
nmin = 1
nfor.interp = 25 
nsd = 1
nfor.pred = 25
parallel = false 
ncores = detectCores() - 1
clusterType = "PSOCK"
seed = 627

strt_time <- Sys.time()
cluster <- makeCluster(ncores) # convention to leave 1 core for OS
registerDoParallel(cluster)

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
                          
stopCluster(cluster)
registerDoSEQ()

output_file <- "output.rds"
saveRDS(dat.vsurf, output_file)
