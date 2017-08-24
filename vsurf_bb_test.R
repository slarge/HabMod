rm(list = ls())
# Using Data Miner with R 
source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")
#SETTING USERNAME AND TOKEN - NOT NEEDED WHEN USING RSTUDIO ON THE PORTAL
username<<-"largesi"
token<<-"1ba80d8d-15ea-4ff1-9420-f8c97ea15d40-843339462" #YOUR TOKEN FOR A VRE

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Generate data for the VSURF algorithm ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# This is the same data that is already loaded to test the VSURF algorithm
library(VSURF)
data(toys)
x <- toys$x
y <- as.numeric(gsub("-1", "0", as.character(toys$y)))

write.csv(x, "text_x.txt", row.names = FALSE)
write.csv(y, "text_y.txt", row.names = FALSE)
rm("x", "y")

#UPLOADING
wsfolder <-  paste0("/Home/", username, "/Workspace/TestUploads/") #REMOTE DESTINATION FOLDER
file_x = "text_x.txt" #LOCAL FILE TO UPLOAD
file_y = "text_y.txt" #LOCAL FILE TO UPLOAD

overwrite <- T #CHOOSE IF THE FILE SHOULD BE OVERWRITTEN
x_upload <- uploadWS(wsfolder, file_x, overwrite) #UPLOAD THE FILE TO THE WS
y_upload <- uploadWS(wsfolder, file_y, overwrite) #UPLOAD THE FILE TO THE WS

# Retreive public urls from the uploaded files
if(x_upload) { 
  x <- getPublicFileLinkWS(paste0(wsfolder, "/", file_x))
  # x <- "http://data.d4science.org/ZHBDVFdKN20zTnZrbWtZeXNwU0hhdFFDc0dxSnlnTGlHbWJQNStIS0N6Yz0"
}
if(y_upload) {
  y <- getPublicFileLinkWS(paste0(wsfolder, "/", file_y))
  # y <- "http://data.d4science.org/eWlraElpN0tkbW5rbWtZeXNwU0hhb3pvNlVYcVJPbTFHbWJQNStIS0N6Yz0"
}

ntree = 10
mtry = 2
nfor.thres = 50
nmin = 1
nfor.interp = 25
nsd = 1
nfor.pred = 25
parallel = "true"
ncores = 15
clusterType = "PSOCK"
seed = 627
name = "test"

frmt <- c("http://dataminer-prototypes.d4science.org/wps/WebProcessingService?request=Execute&service=WPS&Version=1.0.0&",
          "gcube-token=%s&lang=en-US&", 
          "Identifier=org.gcube.dataanalysis.wps.statisticalmanager.synchserver.mappedclasses.transducerers.VSURF&",
          "DataInputs=",
          "x=%s;",
          "y=%s;",
          "ntree=%i;",
          "mtry=%i;",
          "nfor.thres=%i;", 
          "nmin=%i;",
          "nfor.interp=%i;",
          "nsd=%i;",
          "nfor.pred=%i;",
          "parallel=%s;",
          "ncores=%i;",
          "clusterType=%s;",
          "seed=%i;",
          "name=%s;")

do_VSURF <- sprintf(paste(frmt, collapse = ""),
                    token,
                    x,
                    y, 
                    ntree, 
                    mtry, 
                    nfor.thres, 
                    nmin, 
                    nfor.interp, 
                    nsd, 
                    nfor.pred, 
                    parallel, 
                    ncores, 
                    clusterType, 
                    seed, 
                    name)


#run the process
#NOTE: the output is reported directly in the WPS XML answer, thus no bother with other interaction the WS
library(httr)
cat("Executing the process\n")
got <- GET(do_VSURF, 
           authenticate(username, token),
           timeout(1*3600))


#parse the output XML
tryCatch({
  got_urls <- xml_text(xml_find_all(read_xml(got), ".//d4science:Data"))
  
  # results <- xpathSApply(xmltop, paste("//wps:Output/ows:Identifier/../wps:Data/wps:ComplexData", sep = ""))
  # results <- (rapply(results, function(x) head(x, 1)))
  
  logs_of_the_computation <- got_urls[[1]] #this is the place where the logs of the computation are reported
  result_of_the_computation <- got_urls[2] #this is the place where the link to the result is reported
  
  #download the result
  cat("Downloading logs...\n")
  download.file(logs_of_the_computation, "log.txt", mode = "wb", cacheOK = FALSE)
  cat("Downloading results...\n")
  download.file(result_of_the_computation, "output.rds", mode = "wb", cacheOK = FALSE)
  cat("Computation ended.\n")
},
error = function(e) {
  cat("ERROR in algorithm execution - Execution halted\n") 
  success <- F
}
)

tt <- readRDS("output.rds")
