library (raster)
# load file station file rdata
# add column, match data from set of rasters to each record based on 
# date and lon/lat
# or climatology data


# get file to add to: blank_data_template.RData
# in C:\1_analyses_ne_shelf\habitat analyses\blank data

load(file.choose())

#colnames(blank_data_template)

# [1] "CRUISE6"     "STRATUM"     "TOW"         "STATION"     "YEAR"        "SEASON"     
# [7] "LAT"         "LON"         "EST_TOWDATE"

# add new column: 
#  chl or sst
#  4, 9, or 25km
#  ob or fob
#  1day, 8day, month, c1day, c8day, cmonth,

data_type="sst"
spatial_res="25km"
temp_res="month"

# set base directory of rasters
rast.dir=choose.dir(default = "", caption = "Select folder")



####################################################################################
# MOD MOD MOD MOD MOD MOD MOD
# dir for base 
#setwd(gsub(" ","",paste(rast.dir,"\\NESREG")))
setwd(rast.dir)


# MOD MOD MOD MOD MOD MOD MOD
# assing new column, would like to MODIFY to avoid have to replace name below
blank_data_template$chl_f_9km_month_1 <- NA   # Use the same value (0) for all rows
varnum=10


i=33300

for(i in 1:nrow(blank_data_template)){
  
  print(i)
  
  # in loop get record date and date elements
  date=blank_data_template[i,9]
  stayear=substr(date,1,4)
  stamonth=substr(date,6,7)
  staday=substr(date,9,10)
  
  # get files that match year and month
  rast.files=list.files(pattern=gsub(" ","",paste(".",stayear,".",stamonth)))
  
  if(length(rast.files)!=0){
    print("getting data")
    # get vector of file days
    filedays=as.numeric(substr(rast.files,21,22))
    
    # pick file to seach for data for this record
    temp.file=rast.files[which(abs(filedays-as.numeric(staday))==min(abs(filedays-as.numeric(staday))))]
    # takes the first one only
    temp.file <- paste(temp.file[1],sep = "\\")
    load(temp.file)
    
    # get lon lat for record
    xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
    #plot(masked.raster)
    #points(xy,pch=3)
    
    blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
    
  }  #  end if
  
}  # end i loop


hist(blank_data_template$chl_f_9km_month_1)

sum(is.na(blank_data_template$chl_f_9km_month_1))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####################################################################################
# MOD MOD MOD MOD MOD MOD MOD
# dir for base clims   monthly
#setwd(gsub(" ","",paste(rast.dir,"\\NESREG\\clim")))
setwd(gsub(" ","",paste(rast.dir,"\\clim")))
getwd()

# MOD MOD MOD MOD MOD MOD MOD
# assing new column, would like to MODIFY to avoid have to replace name below
blank_data_template$chl_f_9km_month_2 <- NA   # Use the same value (0) for all rows
varnum=11


i=33300

for(i in 1:nrow(blank_data_template)){
  
  print(i)
  
  # in loop get record date and date elements
  date=blank_data_template[i,9]
  stayear=substr(date,1,4)
  stamonth=substr(date,6,7)
  staday=substr(date,9,10)
  
  # get files that match year and month
  rast.files=list.files(pattern=gsub(" ","",paste(".",stamonth,".01")))
  
  if(length(rast.files)!=0){
    print("getting data")
    # get vector of file days
    filedays=as.numeric(substr(rast.files,21,22))
    
    # pick file to seach for data for this record
    temp.file=rast.files[which(abs(filedays-as.numeric(staday))==min(abs(filedays-as.numeric(staday))))]
    # takes the first one only
    temp.file <- paste(temp.file[1],sep = "\\")
    load(temp.file)
    
    # get lon lat for record
    xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
    #plot(masked.raster)
    #points(xy,pch=3)
    
    blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
    
  }  #  end if
  
}  # end i loop


hist(blank_data_template$chl_f_9km_month_2)

sum(is.na(blank_data_template$chl_f_9km_month_2))

# get data for each month
# assing new column, would like to MODIFY to avoid have to replace name below
# Jan 01
blank_data_template$chl_f_9km_month_3 <- NA   # Use the same value (0) for all rows
varnum=12
load(list.files(pattern=gsub(" ","",paste(".01.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
    blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop
hist(blank_data_template$chl_f_9km_month_3)
sum(is.na(blank_data_template$chl_f_9km_month_3))


# 02
print("2")
blank_data_template$chl_f_9km_month_4 <- NA   # Use the same value (0) for all rows
varnum=13
load(list.files(pattern=gsub(" ","",paste(".02.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop

# 03
print("3")
blank_data_template$chl_f_9km_month_5 <- NA   # Use the same value (0) for all rows
varnum=14
load(list.files(pattern=gsub(" ","",paste(".03.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 04
print("4")
blank_data_template$chl_f_9km_month_6 <- NA   # Use the same value (0) for all rows
varnum=15
load(list.files(pattern=gsub(" ","",paste(".04.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 05
print("5")
blank_data_template$chl_f_9km_month_7 <- NA   # Use the same value (0) for all rows
varnum=16
load(list.files(pattern=gsub(" ","",paste(".05.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 06
print("6")
blank_data_template$chl_f_9km_month_8 <- NA   # Use the same value (0) for all rows
varnum=17
load(list.files(pattern=gsub(" ","",paste(".06.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 07
print("7")
blank_data_template$chl_f_9km_month_9 <- NA   # Use the same value (0) for all rows
varnum=18
load(list.files(pattern=gsub(" ","",paste(".07.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 08
print("8")
blank_data_template$chl_f_9km_month_10 <- NA   # Use the same value (0) for all rows
varnum=19
load(list.files(pattern=gsub(" ","",paste(".08.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 09
print("9")
blank_data_template$chl_f_9km_month_11 <- NA   # Use the same value (0) for all rows
varnum=20
load(list.files(pattern=gsub(" ","",paste(".09.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 10
print("10")
blank_data_template$chl_f_9km_month_12 <- NA   # Use the same value (0) for all rows
varnum=21
load(list.files(pattern=gsub(" ","",paste(".10.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 11
print("11")
blank_data_template$chl_f_9km_month_13 <- NA   # Use the same value (0) for all rows
varnum=22
load(list.files(pattern=gsub(" ","",paste(".11.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# 12
print("12")
blank_data_template$chl_f_9km_month_14 <- NA   # Use the same value (0) for all rows
varnum=23
load(list.files(pattern=gsub(" ","",paste(".12.01"))))
for(i in 1:nrow(blank_data_template)){
#  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


####################################################################################
# dir for base alltimeclims
# MOD MOD MOD MOD MOD MOD MOD MOD
setwd(gsub(" ","",paste(rast.dir,"\\alltimeclim")))

# all time clim
blank_data_template$chl_f_9km_month_15 <- NA   # Use the same value (0) for all rows
varnum=24
load(list.files(pattern='RAST'))
for(i in 1:nrow(blank_data_template)){
  print(i)
  xy <- cbind(blank_data_template$LON[i],blank_data_template$LAT[i])
  blank_data_template[i,varnum]=extract(masked.raster,xy,method="bilinear")
}  # end i loop


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# go here
setwd(gsub(" ","",paste(rast.dir,"\\NESREG")))

chl_f_9km_month_data = blank_data_template

save(chl_f_9km_month_data,file="chl_f_9km_month_data.rdata")

cor(chl_f_9km_month_data[,10:24], use = "pairwise")

dim(chl_f_9km_month_data)
for (i in 10:24){
  print(c(colnames(chl_f_9km_month_data)[i],sum(is.na(chl_f_9km_month_data[,i]))))

}



####################################################################################
# dir for front base
setwd(gsub(" ","",paste(rast.dir,"\\NESREG\\fronts")))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


####################################################################################
# dir for front base clims
setwd(gsub(" ","",paste(rast.dir,"\\NESREG\\fronts\\clim")))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


####################################################################################
# dir for front base alltimeclims
setwd(gsub(" ","",paste(rast.dir,"\\NESREG\\fronts\\alltimeclim")))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



list.files(pattern="RAST")





hist(blank_data_template$chl_f_9km_ob_month)

sum(is.na(blank_data_template$chl_f_9km_ob_month))
sum(is.na(blank_data_template$chl_f_9km_ob_8day))

plot(blank_data_template$chl_f_9km_ob_month,blank_data_template$chl_f_9km_ob_8day)
plot(log(blank_data_template$chl_f_9km_ob_month),log(blank_data_template$chl_f_9km_ob_8day))





