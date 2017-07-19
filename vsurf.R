rm(list = ls())

# Load libraries
library(VSURF)
library(snow)
library(Rmpi)

#parameters
data("toys")
x = toys$x; # for test
y = toys$y; # for test
ntree = 500; 
mtry = max(floor(ncol(x)/3), 1);
nfor.thres = 50;
nmin = 1;
nfor.interp = 25; 
nsd = 1;
nfor.pred = 25;
parallel = FALSE; 
ncores = detectCores() - 1;
clusterType = "PSOCK";
seed = 627;

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
                          clusterType = clusterType);

output_file <- "output.rds";
saveRDS(dat.surf, output_file)
