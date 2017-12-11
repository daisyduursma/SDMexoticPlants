

getALA <- function(species, nRecords=FALSE){

  library(RCurl)
  library(rjson)
  
  sp <- strsplit(as.character(species)," ")[[1]]
  
  h <- basicHeaderGatherer()
  
  BaseALA_Data_URL <- "biocache.ala.org.au"  
  Referer <- ""
  
  DataStr <- paste0("/ws/webportal/occurrences?q=",sp[1],"+",sp[2])
  
  message("Querying number of records...")
  rawResponse <- getURL(paste0(BaseALA_Data_URL,DataStr), headerfunction = h$update)
  
  ResponseCode <- as.integer(h$value()["status"])
  
  if(ResponseCode == 200) {
    jsres <- fromJSON(rawResponse)
  } else {
    message("No proper response.")
    return(invisible())
  }
  
  nrecords <- jsres$totalRecords
  if(nRecords)return(nrecords)
  
  message("Downloading ", nrecords, " records.")
  
  # Download all
  DataStr <- paste0(DataStr, "&pageSize=", nrecords)
  rawResponse <- getURL(paste0(BaseALA_Data_URL,DataStr), headerfunction = h$update)
  
  if(ResponseCode == 200) {
    jsres <- fromJSON(rawResponse)
  } else {
    message("No proper response.")
    return(invisible())
  }
  
  totalRecords <- jsres$totalRecords

  res <- lapply(jsres$occurrences, function(x)c(nameSci=x$raw_scientificName, 
                                                latitude=x$decimalLatitude, 
                                                longitude=x$decimalLongitude))
  res <- as.data.frame(do.call(rbind, res))
  res$longitude <- as.numeric(as.character(res$longitude))
  res$latitude <- as.numeric(as.character(res$latitude))
  
return(res)
}


plotoz <- function(r){
  
  library(oz)
  oz()
  with(r, points(longitude, latitude, pch=19, col="red", cex=0.8))
  
}




