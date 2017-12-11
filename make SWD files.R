

#make sure workspace is clean
rm(list = ls())

#libraries
library(RGraphics)
library(raster)
library(dismo)
library(fields)



#directory 
  work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\"
	dat.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\data\\"
	wclim.dir<-"C:\\Daisy\\Raw Data\\van_der_Wal_data\\"

#red in data
obs<-read.csv(paste0(work.dir,"Final_observation_bias_removed_21_10_13.csv"),header = TRUE)
bg<-read.csv(paste0(work.dir,"RK_background_10000_23_10_13.csv"),header=TRUE)
#bg<-bg[,c("species","POINT_X","POINT_Y")]
#colnames(bg)<-c("species","lat","lon")

#  
# make a raster stack of the climate variables
  predictors <- stack(c(
	paste(wclim.dir,"bioclim_01.asc",sep=""),
	paste(wclim.dir,"bioclim_05.asc",sep=""),
	paste(wclim.dir,"bioclim_06.asc",sep=""),
	paste(wclim.dir,"bioclim_12.asc",sep=""),
	paste(wclim.dir,"bioclim_15.asc",sep=""),
	paste(wclim.dir,"clay_5min2.asc",sep="")))

# #extract all variable for obs and background

species_dat<-as.vector(bg$species)
locs<-bg[,c("lon","lat")]
bg_swd<-extract(predictors,locs)
bg_dat<-cbind(species_dat,bg_swd)
bg_dat<-na.omit(bg_dat)

write.csv(bg_dat,row.names=FALSE,file=paste(work.dir,"background_SWD_28_10_13.csv",sep=""))



obs_species_dat<-as.vector(obs$species)
obs_locs<-obs[,c("lon","lat")]
obs_swd<-extract(predictors,obs_locs)
obs_dat<-cbind(obs_species_dat,obs_swd)
obs_dat<-na.omit(obs_dat)

write.csv(obs_dat,row.names=FALSE,file=paste(work.dir,"observations_SWD_28_10_13.csv",sep=""))

