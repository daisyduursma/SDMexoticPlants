

#get SWD for obs. by doing this first any points that do not fall within land areas of climate data and soil data are removed.

#make file with columns: species, number of observations, number of koeppen zones it occupies, area of koeppen zones. 


############################

#make sure workspace is clean
rm(list = ls())

#libraries
library(RGraphics)
library(raster)
library(dismo)
library(fields)

#directory 
	work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")
	wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
		
#read in species data
	dat1<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_one_per_cell.csv",sep=""))
#make vector of species
	spec<-as.vector(unique(dat1$species))
	
#get land_mask
	land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
	
#read in raster of Koeppen values. The resolution of the KG data is .5degrees but I have resampled it to 5 arc minute.  
	r <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
#expanded it to the extent of world clim data and mask the land area to world clim data.
	r<-expand(r, land_mask) 
	r<-mask(r,land_mask)

 
#make a raster stack of the climate variables
	predictors <- stack(c(
	paste(wclim.dir,"bio_12.asc",sep=""),
	paste(wclim.dir,"bio_1.asc",sep=""),
	paste(wclim.dir,"bio_6.asc",sep=""),
	paste(wclim.dir,"bio_5.asc",sep=""),
	paste(wclim.dir,"bio_15.asc",sep=""),
	paste(wclim.dir,"clay_5min2.asc",sep="")))

#make an empty list
	obs <- list()	
	

#start loop for all species and 1. Identify koeppen zones where species occures, 2. Find the area of the koeppen zones that the species occures 
for (i in 1:length(spec)){
	
	#get the data for this species
	remove_dat<-dat1$species==spec[i]
	species_dat<-dat1[remove_dat,]
	
	#get climate data
	locs<-species_dat[,c("Longitude","Latitude")]
	obs_swd<-extract(predictors,locs)
	all_sp_dat<-cbind(species_dat,obs_swd)
	all_sp_dat<-na.omit(all_sp_dat)
	obs_num<-nrow(all_sp_dat)
	#find the koeppen values of observations
	locs<-all_sp_dat[,c("Longitude","Latitude")]
	all_sp_dat$koeppengei<-extract(r,locs)
	koeppen<-as.vector(na.omit(unique(all_sp_dat$koeppengei)))
	num_koep_zones<-length(koeppen)
	
	#make a new raster of just the desired Koeppen zones
	x <- calc(r, function(x) ifelse(x %in% koeppen, 1, NA) )
	
	#finds area of each cell in the new raster
	cell_area<-area(x,na.rm= TRUE)
	#calculate total area of species' koeppen zones
	sum_cell_area<-cellStats(cell_area,stat="sum")
	area_koep_km2<-sum_cell_area
	species_name<-spec[i]
	
	#make a dataframe of data
	info<-cbind(species_name,obs_num,num_koep_zones,area_koep_km2)

	#add dataframe to list
	obs[[i]] <- info
	
	message(paste(i))
}

#combine the list to a dataframe
	all_backdat<-do.call("rbind",obs)

#write out SWD file for background points
	write.csv(all_backdat,row.names=FALSE,file=paste(dat.dir,"observation_koeppen summary.csv",sep=""))


