

#' Title
#'
#' @param file_x
#' @param file_y
#' @param ntree
#' @param mtry
#' @param nfor_thres
#' @param nmin
#' @param nfor_interp
#' @param nsd
#' @param nfor_pred
#' @param parallel
#' @param ncores
#' @param clusterType
#' @param seed
#' @param name
#' @param username
#' @param token
#'
#' @return
#' @export
#'
#' @examples
vsurf_bb <- function(file_x,
                     file_y,
                     ntree = 100,
                     mtry = 2,
                     nfor_thres = 50,
                     nmin = 1,
                     nfor_interp = 25,
                     nsd = 1,
                     nfor_pred = 25,
                     parallel = "true",
                     ncores = 15,
                     clusterType = "FORK",
                     save_template = FALSE,
                     seed = 627,
                     name = "",
                     username = NULL,
                     token = NULL) {

  # source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/WPS4D4Science.r")
  source("http://svn.research-infrastructures.eu/public/d4science/gcube/trunk/data-analysis/RConfiguration/RD4SFunctions/workspace_interaction.r")

  if(!is.null(username) &
     !is.null(token)){
    username <<- username
    token <<- token
  }  
  
  if(is.null(username) |
     is.null(token)){
    set_keys(save_key = FALSE)    
  } 
  
  #INPUT PARAMETERS
  wps_uri = "http://dataminer-prototypes.d4science.org/wps/WebProcessingService"

  file_name_y <- paste0(name, "_y.txt")
  file_name_x <- paste0(name, "_x.txt")

  write.csv(file_y, file_name_y, row.names = FALSE)
  write.csv(file_x, file_name_x, row.names = FALSE)

  #UPLOADING
  wsfolder <-  paste0("/Home/", username, "/Workspace/TestUploads/") #REMOTE DESTINATION FOLDER

  overwrite <- T
  y_upload <- uploadWS(path = wsfolder, file = file_name_y, overwrite = overwrite) #UPLOAD THE FILE TO THE WS
  x_upload <- uploadWS(wsfolder, file_name_x, overwrite) #UPLOAD THE FILE TO THE WS

  ## Retreive public urls from the uploaded files
  if(x_upload) {
    x <- getPublicFileLinkWS(paste0(wsfolder, file_name_x))
  }
  if(y_upload) {
    y <- getPublicFileLinkWS(paste0(wsfolder, file_name_y))
  }

  #PREPARE THE REQUEST FILE BY ALTERING THE TEMPLATE#
  sentfile <- paste(name, "-VSURFreq-", Sys.time(), ".xml" ,sep="")
  sentfile <- gsub(":", "", sentfile)
  sentfile <- gsub(" ", "", sentfile)

  #LOAD THE TEMPLATE#
  templateFile <- "vsurf_template.xml"
  filexml <- readChar(templateFile, file.info(templateFile)$size)
  filexml <- gsub("\r\n", "\n", filexml)

  #SUBSTITUTE INPUTS IN THE TEMPLATE AND EMBED THE FILE#
  filexml <- gsub("#XFILE#", x, filexml)
  filexml <- gsub("#YFILE#", y, filexml)
  filexml <- gsub("#NTREE#", ntree, filexml)
  filexml <- gsub("#MTRY#", mtry, filexml)
  filexml <- gsub("#THRESH#", nfor_thres, filexml)
  filexml <- gsub("#NMIN#", nmin, filexml)
  filexml <- gsub("#INTERP#", nfor_interp, filexml)
  filexml <- gsub("#NSD#", nsd, filexml)
  filexml <- gsub("#PRED#", nfor_pred, filexml)
  filexml <- gsub("#PARALLEL#", parallel, filexml)
  filexml <- gsub("#NCORES#", ncores, filexml)
  filexml <- gsub("#CLUSTERTYPE#", clusterType, filexml)
  filexml <- gsub("#SEED#", seed, filexml)
  filexml <- gsub("#NAME#", name, filexml)
  # cat(filexml)

  #WRITE THE MODIFIED XML TEMPLATE DOCUMENT LOCALLY#
  filehandle <- file(filexml)
  write(filexml, file = sentfile, append = FALSE, sep = "")
  close(filehandle)

  #SEND THE REQUEST#
  out <- httr::POST(url = wps_uri,
                    config = c(authenticate(username,
                                            token,
                                            type = "basic")),
                    body = upload_file(sentfile, type="text/xml"),
                    encode = c("multipart"),
                    handle = NULL)

  #CHECK IF THE PROCESS HAS ALREADY FINISHED#
  stop_condition_fail <- grepl("Exception", as.character(content(out, encoding = "UTF-8")))

  #GET THE STATUS LOCATION FROM THE ACCEPTANCE RESPONSE#
  lout <- as.character(content(out, encoding = "UTF-8"))
  statusLocation <- 'statusLocation=\"'
  endstatusLocation <- '">\n'
  pos1 <- regexpr(statusLocation, lout)
  pos2 <- regexpr(endstatusLocation, lout)
  llout <- substr(lout,
                  pos1 + nchar(statusLocation),
                  pos2 - 1)

  if(save_template){
    data_file <- paste0("analysis/data/derived_data/", gsub("VSURFreq", "VSURFdat", sentfile))
    data_file <- gsub(".xml", ".rdata", dat_file)
    save(list = c("file_x", "file_y"), file = data_file)
    file.rename(from = sentfile, to = paste0("analysis/data/derived_data/", sentfile))
    rm(list = c("file_name_x", "file_name_y", "sentfile"))
  }

  if(!save_template){
    if (file.exists(file_name_x)) file.remove(file_name_x)
    if (file.exists(file_name_y)) file.remove(file_name_y)
    if (file.exists(sentfile)) file.remove(sentfile)
    # rm(list = c("file_name_x", "file_name_y", "sentfile"))
  }

  cat(ifelse(!stop_condition_fail, paste0("Process delivered to the servers. Check status, here: ", llout),
             paste0("\nProcess failed\n", lout)))

  return(llout)
}



#' Title
#'
#' @param svspp
#'
#' @return
#' @export
#'
#' @examples

dat_maker <- function(svspp) {
  wf_dat <- all_dat %>%
    dplyr::filter(SVSPP == svspp)

  name <- gsub(" ", "_", svspp_dat$COMNAME[svspp_dat$SVSPP == svspp])

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Find the "best" dataset with the most rows and columns ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

  # Count of the number of NAs in each column for each year and sum for each variable
  na_count <- wf_dat %>%
    dplyr::select(-dplyr::one_of(join_names[!join_names %in% c("YEAR")])) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarize_all(dplyr::funs(sum(is.na(.)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-YEAR) %>%
    dplyr::summarize_all(dplyr::funs(sum)) %>%
    tidyr::gather(key = COLS, value = NAs)

  # Return the complete cases dimensions
  dim_maker <- function(x){
    dd <- wf_dat %>%
      dplyr::select(-dplyr::one_of(na_count$COLS[na_count$NAs >= x]),
             -dplyr::one_of(join_names[!join_names %in% c("YEAR")])) %>%
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

  wf_lt <- wf_dat %>%
    dplyr::select(-dplyr::one_of(na_count$COLS[na_count$NAs >= na_nums]),
           -dplyr::one_of(join_names),
           LAT, LON, YEAR) %>%
    na.omit %>%
    as.data.frame

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## Create the spatial autocorrelation index ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## Spatial autocorrelation may be an issue. Perhaps we test residuals for spatial autocorrelation
  # rerun the model if it is present.
#   corr_bm <- ncf::correlog(x = wf_lt$LAT , y = wf_lt$LON, z = wf_lt$BIOMASS,
#                            increment = 1, resamp = 500, latlon = TRUE)
#
#   plot(corr_bm)

  # latlong <- cbind(wf_lt$LAT, wf_lt$LON)
  #
  # # bind results on to the wf_dat data frame object
  # wf_lt$SAC.pa <- autocov_dist(z = wf_lt$PRESENCE, xy = latlong, nbs = 100,
  #                              style = "U", type = "inverse",
  #                              longlat = TRUE, zero.policy = TRUE)
  # wf_lt$SAC.az <- autocov_dist(z = wf_lt$ABUNDANCE, xy = latlong, nbs = 100,
  #                              style = "U", type = "inverse",
  #                              longlat = TRUE, zero.policy = TRUE)
  # wf_lt$SAC.bm <- autocov_dist(z = wf_lt$BIOMASS, xy = latlong, nbs = 100,
  #                              style = "U", type = "inverse",
  #                              longlat = TRUE, zero.policy = TRUE)
  wf_lt$SVSPP <- as.numeric(svspp)
  wf_lt$name <- as.character(name)
  cat("Finished with", unique(wf_lt$name), "\n")
  # SAC.bm <- autocov_dist(z = wf_lt$BIOMASS, xy = latlong, nbs = 100,
  #                        style = "U", type = "inverse", longlat = TRUE)

  # Create matrix of the distance between each station in the dataframe
  # nsites <- nrow(wf_lt)
  # latlong <- cbind(wf_lt$LAT, wf_lt$LON)
  # xmat <- matrix(unlist(lapply(1:nsites, function(x) geosphere::distHaversine(latlong[x,],
  # latlong[1:nsites,])/1000)),
  # ncol = nsites)

    # Select all stations within a specified distance of the focal station
  # near <- apply(xmat, 1, function(x) which(x <= 10 & x > 0))
  # Check that every entity included (empty vector needs replacing with integer value of 0)
  # which(sapply(near, length) < 1)

  # Remove index names and list object for each station containing what should be the nearest neighbours
  # near.a <- lapply(near, function(x) {names(x) <- NULL; x})
  # names(near.a) <- NULL
  # class(near.a) <- "nb"
  #
  # # Function to calculate the average abundance in adjacent stations
  # dens.maker <- function(neighbours, METRIC) {
  #   y <- as.vector(neighbours[, METRIC])
  #   y <- as.numeric(as.character(y))
  #   if(length(y) == 0L) {
  #     dens <- 0
  #   }
  #   if(length(y) >= 1L) {
  #     dens <- 1 / length(y) * (sum(y))
  #   }
  #   return(dens)
  # }

  # Run through each list object and use the above 'dens.abundance' function to calculate the local density for each station
  # SAC.pa <- as.vector(matrix(nrow = 0,
  #                            ncol = 2,
  #                            dimnames = list(NULL, LETTERS[1:2]))) # Create dataframe object to store results in
  #
  # SAC.az <- as.vector(matrix(nrow = 0,
  #                            ncol = 2,
  #                            dimnames = list(NULL, LETTERS[1:2]))) # Create dataframe object to store results in
  #
  # SAC.bm <- as.vector(matrix(nrow = 0,
  #                            ncol = 2,
  #                            dimnames = list(NULL, LETTERS[1:2]))) # Create dataframe object to store results in
  #
  # for(m in 1:length(near.a)) {
  #   neigh <- unlist(near.a[m])
  #   neighbours <- wf_lt[neigh,]
  #   local.density <- dens.maker(neighbours, "PRESENCE")
  #   SAC.pa <- rbind(SAC.pa, local.density)
  # }
  #
  # for(m in 1:length(near.a)) {
  #   neigh <- unlist(near.a[m])
  #   neighbours <- wf_lt[neigh,]
  #   local.density <- dens.maker(neighbours, "ABUNDANCE")
  #   SAC.az <- rbind(SAC.az, local.density)
  # }
  #
  # for(m in 1:length(near.a)) {
  #   neigh <- unlist(near.a[m])
  #   neighbours <- wf_lt[neigh,]
  #   local.density <- dens.maker(neighbours, "BIOMASS")
  #   SAC.bm <- rbind(SAC.bm, local.density)
  # }




  return(wf_lt)
}

#' Title
#'
#' @param username
#' @param token
#' @param save_key
#'
#' @return
#' @export
#'
#' @examples
set_keys <- function(save_key = FALSE){

  username <<- readline("What is your username?")
  token <<- readline("What is your token?")

  if(save_key){
    write.csv(x = data.frame(username = username,
                             token = token),
              file = "analysis/data/raw_data/keys.csv",
              row.names = FALSE)
  }
}

