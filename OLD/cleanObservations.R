#combine all data into one
#clip data to landmask
#remove observations that do not have accuracy required
#divide data into Australia and non-Australia
#for non Australia, 
  #remove ones with key words
  #keep only one observation per a cell
#for Australia
  #seperate observations with descriptive words in locality and clip to land mask (write to file)
  #seperate observations that need to be georefrenced (write to file)
  #observations that are okay (write to file)
  #recombine all three files
  # 


rm(list = ls())

datdir<-"c:\\Daisy\\Current_Projects\\exotic_plants_2\\old\\observationsRaw\\"
asciiDir<-"C:\\Daisy\\Raw Data\\"

dat<-list.files(datdir,full.names=TRUE)

#combine all observations
  alldat<-list()
  for (i in 1:length(dat)){
      sp<-paste(strsplit(strsplit(dat[i],"\\\\")[[1]][7],"\\+")[[1]],collapse=" ")
      sp<-strsplit(sp,"_") [[1]]
      sp<-paste(sp[1:length(sp)-1], collapse=" ")
      spdat<-read.csv(dat[i])
      spdat<-spdat[,c("species","lat","lon","locality","country")]
      spdat$species<-paste(sp, collapse=" ")
      alldat[[i]]<-spdat
      message(i)
      message(sp)
  }
  findat<-do.call("rbind",alldat)

write.csv(findat,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\combinedObs.csv",row.names = FALSE)
  

#read in data and make common landmask and 
  findat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\combinedObs.csv")
  library(raster)
  soil<-raster(paste0(asciiDir,"Soil Data\\clay_5min2.asc"))
  climBase<-raster(paste0(asciiDir,"van_der_Wal_data_future\\base.asc"))
  e<-extent(climBase)
  soil<-crop(soil,e)
  landmask<-mask(soil,climBase)

    #divide data into 2 groups (lat and long know and lat and long unknown)
      findat$latTF<-is.na(findat$lat)
      findat$longTF<-is.na(findat$lon)
      #rows with known lat and long
      locsKnown<-subset(findat,latTF==FALSE & longTF==FALSE)
      
    #keep Australian observations for georefrecencing
      locsUnkonwn<-subset(findat,latTF==TRUE & longTF==TRUE)
      locsUnkonwn$locTF<-is.na(locsUnkonwn$locality)
      locsUnkonwn$couTF<-is.na(locsUnkonwn$country)
      aus_dat<-subset(locsUnkonwn,country=="Australia")
      aus_dat<-subset(aus_dat,locTF==FALSE)
      write.csv(aus_dat,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\AusObsGeorefrence.csv",row.names = FALSE)


    #Keep observation with lat and longs are within land
      locs<-SpatialPoints(locsKnown[,c("lon","lat")])
      land_obs<-extract(landmask,locs,cellnumbers=TRUE)
      locsKnown<-cbind(locsKnown,land_obs)
      locsKnown$clayTF<-is.na(locsKnown$clay_5min2)
      locsKnownLand<-subset(locsKnown,clayTF==FALSE)
      locsKnownWater<-subset(locsKnown,clayTF==TRUE)
      #get Australian observations that fall on water
        auslocsKnownWater<-subset(locsKnownWater,country=="Australia")
        write.csv(auslocsKnownWater,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\AusObsINwater.csv",row.names = FALSE)

    #seperate Australian obs
      #Australian mask
      Aus<-raster("c:\\daisy\\Raw Data\\Australia masks\\Australia.asc")
      locs<-SpatialPoints(locsKnownLand[,c("lon","lat")])
      AUS_obs<-is.na(extract(Aus,locs,cellnumbers=FALSE))
      locsKnownLand<-cbind(locsKnownLand,AUS_obs)
      AUSlocsknowland<-subset(locsKnownLand,AUS_obs==FALSE)
      otherlocsknowland<-subset(locsKnownLand,AUS_obs==TRUE)

    #remove observations with any of the works, and write out file of Australin obs with the words

      key_words<-c("greenhouse","ornemental","Cultivated","cultivar","cultivado","coltivata","culto","cultiver","cultive","garden","gardens","jardin","jardins","jardim","jardims","garten","giardino", "parque ecologico","nursery","viveiro","vivaismo","viveiro","vivero","kwekerij")
	
      for (ii in 1:length(key_words)){
      bad <- grepl(paste(key_words[ii]), otherlocsknowland$locality,ignore.case=TRUE)
      otherlocsknowland<-subset(otherlocsknowland,bad==FALSE)
      }
    write.csv(otherlocsknowland,paste("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\AVH_GBIF_nonAUS_locsknown_land_NOunwantedWords.csv",sep=""))
    
    #divide Aus obs into 2 groups 
      #Austrailian obs with no bad words in them
      test<-AUSlocsknowland
      for (ii in 1:length(key_words)){
        bad <- grepl(paste(key_words[ii]), test$locality,ignore.case=TRUE)
      test<-subset(test,bad==FALSE)
      }
    write.csv(test,paste("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\AVH_GBIF_AUS_locsknown_land_NOunwantedWords.csv",sep=""))
    

txt<-as.vector(AUSlocsknowland$locality)

    #get observations with unwanted words
      #rownumber of cells
      badrow<-vector()
      for (ii in 1:length(key_words)){
        bad <- grep(paste(key_words[ii]), txt)
        badrow<-c(badrow,bad)
      }
    badrow<-unique(badrow)
    goodrow<-1:length(txt)
    goodrow<-setdiff(goodrow,badrow)
    a<-AUSlocsknowland[badrow,]
    #b<-AUSlocsknowland[goodrow,]
write.csv(a,paste("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\AVH_GBIF_AUS_locsknown_land_WITHunwantedWords.csv",sep=""))



    
	


      


      


      










