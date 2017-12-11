

rm(list = ls())

datdir<-"c:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\"
asciiDir<-"C:\\Daisy\\Raw Data\\"

dat_names<-list.files(datdir,full.names=TRUE,pattern="cleaned")

# #combine all observations
#   alldat<-list()
#   for (i in 1:length(dat_names)){
#       dat<-read.csv(dat_names[i])
#       subdat<-subset(dat,select=c(species,lat,lon))   
#       alldat[[i]]<-subdat
#       message(i)
#      }
# # #remove duplicates
# 
#   findat<-do.call("rbind",alldat)
#   findat$dups<-duplicated(findat)
#   findat<-subset(findat,dups==FALSE,select=c(species,lat,lon))
#  
# write.csv(findat,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\cleanedcombinedObs.csv",row.names = FALSE)
  

#read in data and make common landmask and 
  findat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\cleanedcombinedObs.csv")
  library(raster)
  library(stringr)
  soil<-raster(paste0(asciiDir,"Soil Data\\clay_5min2.asc"))
  climBase<-raster(paste0(asciiDir,"van_der_Wal_data_future\\base.asc"))
  e<-extent(climBase)
  soil<-crop(soil,e)
  landmask<-mask(soil,climBase)
  locs<-SpatialPoints(findat[,c("lon","lat")])
  land_obs<-as.data.frame(extract(landmask,locs,cellnumbers=TRUE))
  findat<-cbind(findat,land_obs)
  findat$land<-is.na(findat$clay_5min2)
  findat<-subset( findat,land==FALSE,select=c(species,lat,lon,cells))

#find out when species occur in same cell
  #get list of species
  traits<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version4.csv")
    species<-as.vector(traits[,1])
species <- str_trim(gsub("[[:space:]]{2,}", " ", species))
    traits$scientific_name <- species
    write.csv(traits,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version4.csv",row.names=FALSE)


a<-findat$species
 
b <- str_trim(gsub("[[:space:]]{2,}", " ", a))
findat$species<-b
spec<-as.vector(unique(findat$species))

c<-setdiff(spec,species)
d<-setdiff(species,spec)

library(car)

#get species names that need to be fixed
  new<-read.csv(paste0(datdir,"fixSpeciesNames.csv"))
  subnew<-subset(new, action=="fix")
findat_test<-findat

for (i in 1:nrow(subnew)){
  n1<-as.vector(subnew[i,1])
  n1<-str_trim(gsub("[[:space:]]{2,}", " ", n1))
  n2<-as.vector(subnew[i,3])
  n2<-str_trim(gsub("[[:space:]]{2,}", " ", n2))
  dat<-subset(findat,species==paste(n1))
  findat<-subset(findat,species!=paste(n1))
  dat$species<-n2
  findat<-rbind(findat,dat)
 
}

 write.csv(findat,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\cleanedcombinedObs2.csv",row.names=FALSE)

write.csv(a,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\speciescounts.csv",row.names=FALSE)


dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\cleanedcombinedObs2.csv")
 

spec<-as.vector(unique(dat$species))
spec<-unique(species)

gooddat<-list()
for(i in 1:355){

  #read in the species observations
	
  subdat<-subset(dat,species==spec[i])
  
	# #remove duplicates, this keeps the first observation of the dupli
	dups <- duplicated(subdat$cells)
	# # keep the records that are _not_ duplicate
gooddat[[i]] <- subdat[!dups,]
  
		
	}
		
 dat2<-do.call("rbind",gooddat)  


write.csv(findat,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\cleanedcombinedObs2_OnePerCell.csv",row.names=FALSE)













