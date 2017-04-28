#-------------------------------------------------------------------------------
#Required packages
library(randomForest)
library(miscTools)
library(raster)
#-------------------------------------------------------------------------------

setwd("C:/1_analyses_ne_shelf/habitat_analyses/rf")

#-------------------------------------------------------------------------------

# LOAD TRAINING SETS
load("surv.hab.spr.rdata")
load("surv.hab.fal.rdata")

# LOOK AT DATA

hist(surv.hab.spr$ABUNDANCE)
hist(log10(surv.hab.spr$ABUNDANCE+1))

hist(surv.hab.spr$BIOMASS)
hist(log10(surv.hab.spr$BIOMASS+1))


###############################################################
# form run data    test benthic variables
run <- subset(surv.hab.spr, select = c("rugosity",
                                       "namera_bpi",
                                       "namera_vrm",
                                       "necrm_bpi",
                                       "necrm_vrm",
                                       "complexity",
                                       "soft_sed",
                                       "bpi_3_25_layer",
                                       "bpi_30_250_layer",
                                       "seabedforms",
                                       "gdepth",
                                       "BIOMASS" ))

run <- subset(surv.hab.spr, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "gdepth",
                                       "ABUNDANCE"))

##############################################################
# estimation models

run <- subset(surv.hab.spr, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "DEPTH",
                                       "BOTTEMP",
                                       "chl_4km_month_1",
                                       "chl_f_4km_month_1",
                                       "sst_4km_month_1",
                                       "sst_f_4km_month_1",
                                       "BIOMASS"))

run <- subset(surv.hab.spr, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "DEPTH",
                                       "BOTTEMP",
                                       "chl_4km_month_1",
                                       "chl_f_4km_month_1",
                                       "sst_4km_month_1",
                                       "sst_f_4km_month_1",
                                       "ABUNDANCE"))

run <- subset(surv.hab.fal, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "DEPTH",
                                       "BOTTEMP",
                                       "chl_4km_month_1",
                                       "chl_f_4km_month_1",
                                       "sst_4km_month_1",
                                       "sst_f_4km_month_1",
                                       "BIOMASS"))

run <- subset(surv.hab.fal, select = c("soft_sed",
                                       "bpi_30_250_layer",
                                       "DEPTH",
                                       "BOTTEMP",
                                       "chl_4km_month_1",
                                       "chl_f_4km_month_1",
                                       "sst_4km_month_1",
                                       "sst_f_4km_month_1",
                                       "ABUNDANCE"))


##############################################################
# fit rf on data in run data

set.seed(1341)

# delete missing
run <- run[complete.cases(run), ]

# RUN biomass
# transfomr biomass
run$BIOMASS <- log10(run$BIOMASS +1)
# and run
run.rf <-randomForest(BIOMASS~ ., data=run,mtry=2, ntree=300,keep.forest=TRUE, importance=TRUE)

# RUN abundance
# transfomr abundance
run$ABUNDANCE <- log10(run$ABUNDANCE +1)
# and run
run.rf <-randomForest(ABUNDANCE~ ., data=run,mtry=2, ntree=300,keep.forest=TRUE, importance=TRUE)



#summary(run.rf)




#############################################################

# calculatiosn for ob v pred
vars=ncol(run)
run.rf.pr = predict(run.rf,run[,1:vars-1])

# simple corr between ob v pred
cor(run.rf.pr, run[,vars])^2

# compoenents of pseudo r2
y <- run[,vars]
y2 = run.rf.pr
1 - sum((y-y2)^2)/sum((y-mean(y))^2)



# variable importance
importance(run.rf)
# run results
print(run.rf)
# simple correlation between ob v pred
print (cor(run.rf.pr, run[,vars])^2)
# pseudo r2
print(rSquared( run[,vars], run[,vars] - predict(run.rf,run[,1:vars-1])))
# MSE
print(mean(( run[,vars] - predict(run.rf,run[,1:vars-1]))^2))


# ob v pred plot
plot(run[,vars],run.rf.pr)
abline(c(0,1),col=2)

# variable importance plot
varImpPlot(run.rf)

# conversion by trees
plot(run.rf)



###############################################################

save(run.rf, file="rfmodel_spr_bio.rdata")
save(run.rf, file="rfmodel_spr_abun.rdata")
save(run.rf, file="rfmodel_fal_bio.rdata")
save(run.rf, file="rfmodel_fal_abun.rdata")


###############################################################
#  absence pressence runs

set.seed(1341)

# delete missing
run <- run[complete.cases(run), ]

# transform biomass
run$BIOMASS[run$BIOMASS>0]=1

# make factor
run$BIOMASS <- as.factor(run$BIOMASS)

# and run
run.rf <-randomForest(BIOMASS~ ., data=run, importance=TRUE,proximity=TRUE)



# variable importance plot
varImpPlot(run.rf)

# conversion by trees
plot(run.rf)



save(run.rf, file="rfmodel_spr_ap.rdata")
save(run.rf, file="rfmodel_fal_ap.rdata")
###############################################################














###############################################################
# unused diagnostic plots
#partialPlot(run.rf,run, gdepth )
#partialPlot(run.rf,run, soft_sed )
#partialPlot(run.rf,run, bpi_30_250_layer )

#bestmtry <- tuneRF(run[,1:3],run[,4],
#                   ntreeTry=100, 
#                   stepFactor=1.5,improve=0.01,
#                   trace=TRUE, plot=TRUE, dobest=FALSE)
###############################################################

##################################################################
# PREIMINARY PLOTTING NOT USED

pre.data <- subset(surv.hab.spr, select = c("soft_sed",
                                            "bpi_30_250_layer",
                                            "DEPTH",
                                            "BOTTEMP",
                                            "chl_4km_month_1",
                                            "chl_f_4km_month_1",
                                            "sst_4km_month_1",
                                            "sst_f_4km_month_1",
                                            "LON",
                                            "LAT"
))


pre.data <- pre.data[complete.cases(pre.data), ]

pvars=ncol(pre.data)

run.rf.pr = predict(run.rf,pre.data[,1:pvars-2])

plot.data=cbind(pre.data$LON, pre.data$LAT,run.rf.pr)

library("akima")
library("maps")
library(marmap)

# get bathy data
getNOAA.bathy(lon1 = -72, lon2 = -66, lat1 = 40, lat2 = 45,
              resolution = 10) -> nesbath

#contour(interp(pre.data$LON, pre.data$LAT, run.rf.pr))

map('state', xlim=c(-72,-66),ylim=c(40,45))


filled.contour(interp(pre.data$LON, pre.data$LAT, run.rf.pr),add=T ,
               color.palette=colorRampPalette(c('white','yellow','orange','red')))

map('state', fill = TRUE, add=T,xlim=c(-72,-66),ylim=c(40,45),col = "grey")


plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)

points(pre.data$LON, pre.data$LAT)

image(interp(interp(pre.data$LON, pre.data$LAT, run.rf.pr)))

plot(pre.data$LON, pre.data$LAT,col=run.rf.pr)


map('state', xlim=c(-72,-66),ylim=c(40,45), fill=T,border=0,col="gray")
map.axes()
plot(nesbath,deep=-200, shallow=-200, step=1,xlim=c(-72,-66),ylim=c(40,45),add=T,lwd=1,col="gray50",lty=2)
points(pre.data$LON, pre.data$LAT,pch=20, cex=.5, col="lightskyblue")

filled.contour(x,y,z,levels=c(2,4,6,8,10),plot.axes=map("worldHires",add=T))

filled.contour(interp(pre.data$LON, pre.data$LAT, run.rf.pr),
               plot.axes=map("state",add=T) ,
               color.palette=colorRampPalette(c('white','yellow','orange','red')))


filled.contour(interp(pre.data$LON, pre.data$LAT, run.rf.pr),add=T ,
               color.palette=colorRampPalette(c('white','yellow','orange','red')))




















