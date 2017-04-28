#-------------------------------------------------------------------------------
#Required packages
# library(data.table)
library(randomForest)
# library(ROCR)
library(miscTools)
library(raster)
#-------------------------------------------------------------------------------

setwd("C:/1_analyses_ne_shelf/habitat_analyses/rf")

#-------------------------------------------------------------------------------

#  FORM TRAINING DATASETS

#Grab habitat data
load("benthic_hab_data.rdata")

#Grab habitat data
load("chl_4km_month_data.rdata")
chl_4km_month_data <- chl_4km_month_data[ -c(1:9) ]
load("chl_9km_month_data.rdata")
chl_9km_month_data <- chl_9km_month_data[ -c(1:9) ]
load("chl_25km_month_data.rdata")
chl_25km_month_data <- chl_25km_month_data[ -c(1:9) ]

load("chl_f_4km_month_data.rdata")
chl_f_4km_month_data <- chl_f_4km_month_data[ -c(1:9) ]
load("chl_f_9km_month_data.rdata")
chl_f_9km_month_data <- chl_f_9km_month_data[ -c(1:9) ]
load("chl_f_25km_month_data.rdata")
chl_f_25km_month_data <- chl_f_25km_month_data[ -c(1:9) ]

load("sst_4km_month_data.rdata")
sst_4km_month_data <- sst_4km_month_data[ -c(1:9) ]
load("sst_9km_month_data.rdata")
sst_9km_month_data <- sst_9km_month_data[ -c(1:9) ]
load("sst_25km_month_data.rdata")
sst_25km_month_data <- sst_25km_month_data[ -c(1:9) ]

load("sst_f_4km_month_data.rdata")
sst_f_4km_month_data <- sst_f_4km_month_data[ -c(1:9) ]
load("sst_f_9km_month_data.rdata")
sst_f_9km_month_data <- sst_f_9km_month_data[ -c(1:9) ]
load("sst_f_25km_month_data.rdata")
sst_f_25km_month_data <- sst_f_25km_month_data[ -c(1:9) ]



edata = cbind(benthic_hab_data,chl_4km_month_data ,chl_9km_month_data ,
chl_25km_month_data , chl_f_4km_month_data ,chl_f_9km_month_data ,
chl_f_25km_month_data ,sst_4km_month_data ,sst_9km_month_data ,
sst_25km_month_data ,sst_f_4km_month_data ,sst_f_9km_month_data ,
sst_f_25km_month_data )




remove( benthic_hab_data)
remove( chl_4km_month_data )
remove( chl_9km_month_data )
remove( chl_25km_month_data )
remove( chl_f_4km_month_data )
remove( chl_f_9km_month_data )
remove( chl_f_25km_month_data )
remove( sst_4km_month_data )
remove( sst_9km_month_data )
remove( sst_25km_month_data )
remove( sst_f_4km_month_data )
remove( sst_f_9km_month_data )
remove( sst_f_25km_month_data )

#Grab survdat.r
load("survdat_stations.RData")


# merge data with e data
surv.hab=merge(survdat_stations,edata)

remove(survdat_stations)
remove(edata)

colnames(surv.hab)

table(surv.hab$SVSPP)



#####################
library(data.table)

surv.hab = data.table(surv.hab)

setkey(surv.hab, CRUISE6, STRATUM, TOW, STATION)
surv.hab <- unique(surv.hab)
####################################


# retain only witch
surv.hab.wit <- surv.hab[(surv.hab$SVSPP==107),]
remove(surv.hab)


# divid into spr and fall
surv.hab.wit.spr <- surv.hab.wit[(surv.hab.wit$SEASON=="SPRING"),]
surv.hab.wit.fal <- surv.hab.wit[(surv.hab.wit$SEASON=="FALL"),]
remove(surv.hab.wit)

save(surv.hab.wit.spr, file="surv.hab.wit.spr.rdata")
save(surv.hab.wit.fal, file="surv.hab.wit.fal.rdata")

load("surv.hab.wit.spr.rdata")
load("surv.hab.wit.fal.rdata")



#  FORM TABLE WITH ALL STATIONS

colnames(surv.hab)
surv.hab$SVSPP=107
surv.hab$ABUNDANCE=0
surv.hab$BIOMASS=0


a=surv.hab
b=surv.hab.wit.spr
c=surv.hab.wit.fal



for (i in 1:nrow(surv.hab.wit.spr)){
a$ABUNDANCE[a$CRUISE6==b$CRUISE6[i] & a$STRATUM==b$STRATUM[i] & a$TOW==b$TOW[i]]=b$ABUNDANCE[i]
a$BIOMASS[a$CRUISE6==b$CRUISE6[i] & a$STRATUM==b$STRATUM[i] & a$TOW==b$TOW[i]]=b$BIOMASS[i]
}

for (i in 1:nrow(surv.hab.wit.fal)){
  a$ABUNDANCE[a$CRUISE6==c$CRUISE6[i] & a$STRATUM==c$STRATUM[i] & a$TOW==c$TOW[i]]=c$ABUNDANCE[i]
  a$BIOMASS[a$CRUISE6==c$CRUISE6[i] & a$STRATUM==c$STRATUM[i] & a$TOW==c$TOW[i]]=c$BIOMASS[i]
}

surv.hab=a

# divid into spr and fall
surv.hab.spr <- surv.hab[(surv.hab$SEASON=="SPRING"),]
surv.hab.fal <- surv.hab[(surv.hab$SEASON=="FALL"),]
remove(surv.hab)

save(surv.hab.spr, file="surv.hab.spr.rdata")
save(surv.hab.fal, file="surv.hab.fal.rdata")


#########################################

load("surv.hab.spr.rdata")
load("surv.hab.fal.rdata")

load("surv.hab.wit.spr.rdata")
load("surv.hab.wit.fal.rdata")

hist(surv.hab.wit.spr$ABUNDANCE)
hist(log10(surv.hab.spr$ABUNDANCE+1))
hist(log10(surv.hab.wit.spr$ABUNDANCE+1))

hist(surv.hab.spr$BIOMASS)
hist(log10(surv.hab.spr$BIOMASS+1))


###############################################################

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
                                       "ABUNDANCE"))

##############################################################


set.seed(1341)

run <- run[complete.cases(run), ]

run$ABUNDANCE <- log10(run$ABUNDANCE +1)
run$BIOMASS <- log10(run$BIOMASS +1)

run.rf <-randomForest(BIOMASS~ ., data=run,
                       mtry=2, ntree=300, 
                       keep.forest=TRUE, importance=TRUE)
summary(run.rf)

importance(run.rf)

varImpPlot(run.rf)

plot(run.rf)

print(run.rf)

#############################################################

vars=ncol(run)

run.rf.pr = predict(run.rf,run[,1:vars-1])

plot(run.rf.pr, run[,vars])
abline(c(0,1),col=2)

cor(run.rf.pr, run[,vars])^2

y <- run[,vars]
y2 = run.rf.pr
 1 - sum((y-y2)^2)/sum((y-mean(y))^2)

 
r2 <- rSquared( run[,vars], run[,vars] - predict(run.rf,run[,1:vars-1]) )

mse <- mean(( run[,vars] - predict(run.rf,run[,1:vars-1]))^2)

###############################################################

save(run.rf, file="rfmodel_spr_bio.rdata")

###############################################################

plot(surv.hab.spr$gdepth,surv.hab.spr$DEPTH)
cor(surv.hab.spr$gdepth,surv.hab.spr$DEPTH, na.rm=T)

partialPlot(run.rf,run, gdepth )
partialPlot(run.rf,run, soft_sed )
partialPlot(run.rf,run, bpi_30_250_layer )

hist(run$gdepth)
hist(run$necrm_vrm)
hist(run$bpi_30_250_layer)

plot(run$necrm_vrm,run$bpi_30_250_layer)

plot(survdat.ben.wit.spr$LON.x, survdat.ben.wit.spr$LAT.x)
points(survdat.ben.wit.fal$LON.x, survdat.ben.wit.fal$LAT.x, col="red")

##################################################################

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











bestmtry <- tuneRF(run[,1:3],run[,4],
                   ntreeTry=100, 
                   stepFactor=1.5,improve=0.01,
                   trace=TRUE, plot=TRUE, dobest=FALSE)

dep=survdat.ben.spr.wit.cc


set.seed(1341)

test.rf <-randomForest(survdat.ben.spr.wit.cc[,13,with=F]~ .,
                       mtry=2, ntree=1000, 
                       keep.forest=TRUE, importance=TRUE)

test.rf <-randomForest(survdat.ben.spr.wit.cc$ABUNDANCE~ .,
                       mtry=2, ntree=1000, 
                       keep.forest=TRUE, importance=TRUE)

test.rf <-randomForest(survdat.ben.spr.wit.cc$ABUNDANCE~ survdat.ben.spr.wit.cc$rugosity,
                       mtry=2, ntree=1000, 
                       keep.forest=TRUE, importance=TRUE)



dim(survdat.ben.spr.wit.cc)

summary(test.rf)

importance(test.rf)
varImpPlot(test.rf)



library(randomForest)
x <- cbind(x_train,y_train)
# Fitting model
fit <- randomForest(Species ~ ., x,ntree=500)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)



library(miscTools)


(r2 <- rSquared(data$y, data$y - predict(test.rf, data[-1])))
# [1] 0.6481
(mse <- mean((data$y - predict(test.rf, data[-1]))^2))
# [1] 0.6358

plot(data$y,predict(test.rf, data[-1]))


data[-1]
data[,c("y","x2","x4")]




