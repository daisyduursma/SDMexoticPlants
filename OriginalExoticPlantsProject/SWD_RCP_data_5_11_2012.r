

#make SWD file on obs points and background. 

#make sure workspace is clean
rm(list = ls())

#libraries
library(RGraphics)
library(raster)
library(dismo)
library(fields)

#directory 
	work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")
	wclim.dir<-"C:\\Daisy\\Raw Data\\van_der_Wal_data\\"
	
	
#read in species data
	dat1<-read.csv(paste(dat.dir,"observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv",sep=""))
	dat1<-dat1[,c("species","Latitude","Longitude")]
#make vector of species
	spec<-as.vector(unique(dat1$species))
#read in background data
  dat2<-read.csv(paste(dat.dir,"RK_background_10000_2_10_12_8km_grid.csv",sep=""))
  dat2<-dat2[,c("species","Latitude","Longitude")]



#make a raster stack of the climate variables
	predictors <- stack(c(
	paste(wclim.dir,"bioclim_01.asc",sep=""),
	paste(wclim.dir,"bioclim_02.asc",sep=""),
	paste(wclim.dir,"bioclim_03.asc",sep=""),
	paste(wclim.dir,"bioclim_04.asc",sep=""),
	paste(wclim.dir,"bioclim_05.asc",sep=""),
	paste(wclim.dir,"bioclim_06.asc",sep=""),
	paste(wclim.dir,"bioclim_07.asc",sep=""),
	paste(wclim.dir,"bioclim_08.asc",sep=""),
	paste(wclim.dir,"bioclim_09.asc",sep=""),  
	paste(wclim.dir,"bioclim_10.asc",sep=""),
	paste(wclim.dir,"bioclim_11.asc",sep=""),
	paste(wclim.dir,"bioclim_12.asc",sep=""),
	paste(wclim.dir,"bioclim_13.asc",sep=""),
	paste(wclim.dir,"bioclim_14.asc",sep=""),
	paste(wclim.dir,"bioclim_15.asc",sep=""),
	paste(wclim.dir,"bioclim_16.asc",sep=""),
	paste(wclim.dir,"bioclim_17.asc",sep=""),
	paste(wclim.dir,"bioclim_18.asc",sep=""),
	paste(wclim.dir,"bioclim_19.asc",sep=""),
	paste(wclim.dir,"clay_5min2.asc",sep="")))


#make an empty lists

  obs_SWD<-list()
  bg_SWD <- list()	

#start loop for all species
for (i in 1:length(spec)){
	
	
	#get the obs data for this species
	remove_dat<-dat1$species==spec[i]
	species_dat<-dat1[remove_dat,]
  
  #get background data for species
	remove_dat2<-dat2$species==spec[i]
	species_backdat<-dat2[remove_dat2,]
	
#find location
	obs_locs<-species_dat[,c("Longitude","Latitude")]
	back_locs<-species_backdat[,c("Longitude","Latitude")]
  
#extract the bioclim and clay data  
	sp_obs_swd<-extract(predictors,obs_locs)
	sp_bg_swd<-extract(predictors,back_locs)  
  
  sp_obs_swd2<-cbind(species_dat[,c("species","Longitude","Latitude")],sp_obs_swd)
	sp_bg_swd2<-cbind(species_backdat[,c("species","Longitude","Latitude")],sp_bg_swd)
	
	
	#join data to list
	bg_SWD[[i]] <-  sp_bg_swd2
  obs_SWD[[i]]<-sp_obs_swd2
	message(paste(i))
}


#combine lists to a dataframes
observations_SWD<-do.call("rbind",obs_SWD)
background_SWD<-do.call("rbind",bg_SWD)

#write out SWD file for background points
write.csv(observations_SWD,row.names=FALSE,file=paste(dat.dir,"observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv",sep=""))
write.csv(background_SWD,row.names=FALSE,file=paste(dat.dir,"RK_background_10000_05_11_12_8km_grid.csv",sep=""))
