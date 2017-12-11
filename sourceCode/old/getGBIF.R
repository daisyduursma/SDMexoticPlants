





getGBIF <- function(species, worldclim_dataloc = "c:/data/worldclim",
                              writefile=TRUE, outdir="", gbifOnly=FALSE){


  library(raster)
  library(dismo)
  library(XML)
  library(rgdal)

  sp <- strsplit(species, " ")[[1]]
  gbifall <- gbif(sp[1], sp[2], removeZeros=TRUE, download=TRUE, geo=FALSE,ntries=10)
    
  if(is.null(gbifall))return()
  
  gbifdat<- gbifall[,c("species","lat","lon","coordUncertaintyM","locality","country","adm1")]
 
 
  # keep only original species ('species' field in gbif), and lat/lon
 
  
  gbifdat$latTF<-is.na(gbifdat$lat)
  gbifdat$longTF<-is.na(gbifdat$lon)
	#is.na(gbifdat$locality[gbifdat$locality==""])<-TRUE
	#is.na(gbifdat$country[gbifdat$country==""])<-TRUE
	gbifdat$locTF<-is.na(gbifdat$locality)
	gbifdat$couTF<-is.na(gbifdat$country)
  
  aus_dat<-subset(gbifdat,country=="Australia")
  #keep rows with known lat and long
	aus_dat2<-subset(aus_dat,latTF==FALSE & longTF==FALSE)
	#find rows with known locality and country
	aus_dat3<-subset(aus_dat,locTF==FALSE & couTF==FALSE)
	aus_dat4<-rbind(aus_dat2,aus_dat3)
	# remove duplicates
  aus_dat5 <- aus_dat4[!duplicated(paste(aus_dat4$lat,aus_dat4$lon,aus_dat4$locality,sep="-")),]
  aus_dat6 <- aus_dat5[,c("species","lat","lon","coordUncertaintyM","locality","country","adm1")]
   
  other_dat<-subset(gbifdat,country!="Australia")
  other_dat<-subset(other_dat,latTF==FALSE & longTF==FALSE)
  other_dat <- other_dat[!duplicated(paste(other_dat$lat,other_dat$lon,sep="-")),]
  other_dat <- other_dat[,c("species","lat","lon","coordUncertaintyM","locality","country","adm1")]
  
  
  gbifdat<-rbind(other_dat,aus_dat6)
  
  names(gbifdat)[1] <- "speciesFull"
  ifelse(nrow(gbifdat)==0,gbifdat<-gbifdat,gbifdat$species <- species)
 # species name as input
 
  specout <- gsub(" ","_",species)
  if(writefile)write.csv(gbifdat, paste0(outdir,specout,"_gbif.csv"), row.names=FALSE)

}



