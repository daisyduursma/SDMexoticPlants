#remove aquatics
  #write out aquatics file and email to alex
#make sure everything falls on common land mask
  #write out file to be used when write if species are present in area
#one observation for grid cell
 #to be used in modelling

#########################3
aquatics{


rm(list = ls())

datdir<-"C:\\Users\\dduursma\\Google Drive\\weeds\\"

#read in data
  traits<-read.csv(paste0(datdir,"OEH trait database Version5.csv"))
#read in observations
  obs<-read.csv(paste0(datdir,"cleanedcombinedObs4_wons_obs.csv"))

#get list of aquatic species
  aqua<-as.vector(unique(subset(traits,Aquatic==1)$species))


#loop through and get aquatic species
aqua_obs<-list()
  for (i in 1:length(aqua)){
aqua_obs[[i]]<-subset(obs, species==aqua[i])
}

aqua_final<-do.call("rbind",aqua_obs)
write.csv(aqua_final,paste0(datdir,"observations_aqua_weeds.csv"),row.names=FALSE)




}

###########################################


  
  rm(list = ls())
  datdir<-"C:\\Users\\dduursma\\Google Drive\\weeds\\"
  asciiDir<-"F:\\daisy\\Raw Data\\"
  library(raster)
  library(stringr)
   
#read in data
  traits<-read.csv(paste0(datdir,"OEH trait database Version5.csv"))
#read in observations
  obs<-read.csv(paste0(datdir,"cleanedcombinedObs4_wons_obs.csv"))
#get list of aquatic species
    nonaqua<-as.vector(unique(subset(traits,Aquatic==0)$species))
#loop through and get nonaquatic species
  nonaqua_obs<-list()
  for (i in 1:length(nonaqua)){
    nonaqua_obs[[i]]<-subset(obs, species==nonaqua[i])
  }
  
  nonaqua_final<-do.call("rbind",nonaqua_obs)
#check that they are in common landmask
  soil<-raster(paste0(asciiDir,"Soil Data\\clay_5min2.asc"))
  climBase<-raster(paste0(asciiDir,"van_der_Wal_data_future\\base.asc"))
  e<-extent(climBase)
  soil<-crop(soil,e)
  landmask<-mask(soil,climBase)
  locs<-SpatialPoints(nonaqua_final[,c("lon","lat")])
  land_obs<-as.data.frame(extract(landmask,locs,cellnumbers=TRUE))
  findat<-cbind(nonaqua_final,land_obs)
  findat$land<-is.na(findat$clay_5min2)
  findat<-subset( findat,land==FALSE,select=c(species,lat,lon,cells))
  
  write.csv(findat,paste0(datdir,"observations_land_weeds.csv"),row.names=FALSE)
  
  
  #one per a cell
  celldat<-list()
  for(i in 1:length(nonaqua)){
    
    #get single species obs    
    subdat<-subset(findat,species==nonaqua[i])
    
    # #remove duplicates, this keeps the first observation of the dupli
    dups <- duplicated(subdat$cells)
    # # keep the records that are _not_ duplicate
    celldat[[i]] <- subdat[!dups,]
    
    
  }
  
  dat2<-do.call("rbind",celldat)  
  dat2<-subset(dat2,select=c(species,lat,lon))
  dat2<-droplevels(dat2)
  
  
  write.csv(dat2,paste0(datdir,"final_obs_OnePerCell_17102013.csv"),row.names=FALSE)
  
  
 
a<-as.data.frame(table(dat2$species))
b<-subset(a,Freq >=30)

write.csv(b,paste0(datdir,"species_17102013.csv"),row.names=FALSE)

#remove species with less than 30 observations


  spec<-b$Var1


keepdat<-list()
for(i in 1:length(spec)){
  
  #get single species obs    
  keepdat[[i]]<-subset(dat2,species==spec[i])
  
  
}

dat3<-do.call("rbind",keepdat)  
dat3<-droplevels(dat3)


write.csv(dat3,paste0(datdir,"more_than_30_obs_final_obs_OnePerCell_17102013.csv"),row.names=FALSE)





  
  
  
  