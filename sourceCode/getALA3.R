
getALA <- function(species, outdir=""){

  sp <- strsplit(species, " ")[[1]]
  
  temp <- tempfile()
  download.file(paste0("http://biocache.ala.org.au/ws/occurrences/download?q=",sp[1],"+",sp[2]),temp, mode="wb")
  download.file("http://biocache.ala.org.au/ws/occurrences/download?q=Passer domesticus",temp, mode="wb")
  
  
  download.file("http://biocache.ala.org.au/ws/occurrences/index/download?qa=none&q=Passer+domesticus&fields=latitude,longitude&fq=basis_of_record:LivingSpecimen&reasonTypeId=7",temp, mode="wb")
  History
  
  Passer domesticus
  alaAll<- read.csv(unz(temp, "data.csv"))
  unlink(temp)
  
  alaAll$species<-species
  
  aladat<- alaAll[,c("species","Latitude...processed","Longitude...processed","Coordinate.Uncertainty.in.Metres...parsed","locality","Country...parsed" ,"State...parsed")]
  
  library(reshape)
  #rename same as gbifall
  aladat<-rename(aladat,c("species",Latitude...processed="lat",Longitude...processed="lon",Coordinate.Uncertainty.in.Metres...parsed="coordUncertaintyM","locality",Country...parsed="country",State...parsed="adm1"))
  
  
   aladat$latTF<-is.na(aladat$lat)
  aladat$longTF<-is.na(aladat$lon)
  #is.na(aladat$locality[aladat$locality==""])<-TRUE
	#is.na(aladat$country[aladat$country==""])<-TRUE
	aladat$locTF<-is.na(aladat$locality)
	aladat$couTF<-is.na(aladat$country)
  
  aus_dat<-subset(aladat,country=="Australia")
  #keep rows with known lat and long
	aus_dat2<-subset(aus_dat,latTF==FALSE & longTF==FALSE)
	#find rows with known locality and country
	aus_dat3<-subset(aus_dat,locTF==FALSE & couTF==FALSE)
	aus_dat4<-rbind(aus_dat2,aus_dat3)
	# remove duplicates
  aus_dat5 <- aus_dat4[!duplicated(paste(aus_dat4$lat,aus_dat4$lon,aus_dat4$locality,sep="-")),]
  aus_dat6 <- aus_dat5[,c("species","lat","lon","coordUncertaintyM","locality","country","adm1")]
   
  other_dat<-subset(aladat,country!="Australia")
  other_dat<-subset(other_dat,latTF==FALSE & longTF==FALSE)
  other_dat <- other_dat[!duplicated(paste(other_dat$lat,other_dat$lon,sep="-")),]
  other_dat <- other_dat[,c("species","lat","lon","coordUncertaintyM","locality","country","adm1")]
  
  
  aladat<-rbind(other_dat,aus_dat6)
  
  names(aladat)[1] <- "speciesFull"
  ifelse(nrow(aladat)==0,aladat<-aladat,aladat$species <- species)
 # species name as input
 
  
  
  
      specout <- gsub(" ","_",species)
  
  write.csv(aladat, paste0(outdir,specout,"_ala.csv"), row.names=FALSE)
}
  