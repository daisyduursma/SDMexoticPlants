

#make background points based on location of where observations occur. Background points will be from the same Koeppen zones as the observations . by doing this first any points that do not fall within land areas of climate data and soil data are removed.

#create random background points for  all species. The minimum number of background points is 10000, background points can not occur in same cells as presence points.

#make map of all species koppen zones, where observations are, number of observations and area of koeppen zones (This is currently not active

############################

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
	wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
	

# 	
# #read in raster of Koeppen values. The resolution of the KG data is .5degrees but I have resampled it to 5 arc minute. 
  worldclim<-raster(paste(wclim.dir,"bio_1.asc",sep=""))
	r <- raster(paste(wclim.dir,"clay_5min2.asc",sep=""))
# #expanded it to the extent of world clim data and mask the land area to world clim data.
	r<-mask(r,worldclim)
  kg_5min <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
  a<-extent(kg_5min)
  r1<-crop(r,a)
  r2<-mask(kg_5min,r1)

#read in raster of Koeppen values. The resolution of the KG data is .5degrees but I have resampled it to 5 arc minute and then to an 8km equal area grid.  
#kg_8km <- raster(paste(dat.dir,"kg_worldclim_clay_mask_8km.asc",sep=""))
	
	
#read in species data
	dat1<-read.csv(paste(work.dir,"Final_observation_bias_removed_21_10_13.csv",sep=""))
#make vector of species
	spec<-as.vector(unique(dat1$species))

 #make an empty list
	bg_SWD <- list()	
		
#start loop for all species and 1. Identify koeppen zones where species occures, 2. Find the area of the koeppen zones that the species occures 3. find the number of background points needed so all species have an equal density of background points 4)make  backgound points, 5) make SWD file for background points
for (i in 1:length(spec)){
	
	
	#get the data for this species
	remove_dat<-dat1$species==spec[i]
	species_dat<-dat1[remove_dat,]
	#find the koeppen values of observations
	locs<-species_dat[,c("lon","lat")]
	species_dat$koeppengei<-extract(r2,locs)
	koeppen<-as.vector(na.omit(unique(species_dat$koeppengei)))
	num_koep_zones<-length(koeppen)
	#make a new raster of just the desired Koeppen zones
	x <- calc(r2, function(x) ifelse(x %in% koeppen, 1, NA) )
		bg_num<-10000
	bg<-as.data.frame(randomPoints(x,n = bg_num))
  
  
	#make a dataframe of background points so that it is lat, long; and species name
	bg$species<-rep(spec[i],nrow(bg))
	#change names
	names(bg)[1]<-"lon"
	names(bg)[2]<-"lat"
	##make images
	
		
	#join data to list
	bg_SWD[[i]] <-  bg[,c("species","lat","lon")]
	message(paste(i))
}

	
	
#combine lists to a dataframes
background<-do.call("rbind",bg_SWD)

#write out SWD file for background points
write.csv(background,row.names=FALSE,file=paste(dat.dir,"RK_background_10000_23_10_13.csv",sep=""))





















