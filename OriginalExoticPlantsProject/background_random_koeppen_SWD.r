

#make SWD file on obs points. by doing this first any points that do not fall within land areas of climate data and soil data are removed.

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
	work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")
	wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
	
	land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
	
#read in raster of Koeppen values. The resolution of the KG data is .5degrees but I have resampled it to 5 arc minute.  
	r <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
#expanded it to the extent of world clim data and mask the land area to world clim data.
	r<-expand(r, land_mask) 
	r<-mask(r,land_mask)

	
	
#read in species data
	dat1<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked_one_per_cell.csv",sep=""))
	dat1<-dat1[,c("species","Latitude","Longitude")]
#make vector of species
	spec<-as.vector(unique(dat1$species))

 
#make a raster stack of the climate variables
	predictors <- stack(c(
	paste(wclim.dir,"bio_12.asc",sep=""),
	paste(wclim.dir,"bio_1.asc",sep=""),
	paste(wclim.dir,"bio_6.asc",sep=""),
	paste(wclim.dir,"bio_5.asc",sep=""),
	paste(wclim.dir,"bio_15.asc",sep=""),
	paste(wclim.dir,"clay_5min2.asc",sep="")))


 #make an empty list
	bg_SWD <- list()	
	obs_summary<-list()
	obs_SWD<-list()
	
koep_avg<-78431535.11
	
 (area of koeppen zones/(Averagea area for all species/2))*10000
		
#start loop for all species and 1. Identify koeppen zones where species occures, 2. Find the area of the koeppen zones that the species occures 3. find the number of background points needed so all species have an equal density of background points 4)make  backgound points, 5) make SWD file for background points
for (i in 1:length(spec)){
	
	
	#get the data for this species
	remove_dat<-dat1$species==spec[i]
	species_dat<-dat1[remove_dat,]
	#get climate data for observations and make SWD
	locs<-species_dat[,c("Longitude","Latitude")]
	obs_swd<-extract(predictors,locs)
	all_sp_dat<-cbind(species_dat,obs_swd)
	#remove observations with NA values
	all_sp_dat<-na.omit(all_sp_dat)
	#find out how many observations there are
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
	# #find out the number of background points needed to keep ratio the same. The ratio is determined by species with the smallest area of observed koeppen zones
	
	
	
	
	bg_num_1<-round((sum_cell_area/(koep_avg/2))*10000)
	bg_num<-ifelse(bg_num_1>9999,bg_num_1,10000)
	
	# #matrix of observations lats and longs
	pres<-all_sp_dat[,c("Longitude","Latitude")]
	# #create random points that do not occur in cells where presences are
	bg<-as.data.frame(randomPoints(x,n = bg_num,p = pres))
	#make a dataframe of background points so that it is lat, long; and species name
	bg$species<-rep(spec[i],nrow(bg))
	#Extract the variables SWD
	absenc<-bg[,c("x","y")]
	bg_swd<-extract(predictors,absenc)
	#merge the lat, longs with variables
	bg_sp_dat<-cbind(bg,bg_swd)
	#change names
	names(bg_sp_dat)[1]<-"Longitude"
	names(bg_sp_dat)[2]<-"Latitude"
	##make images
	
	 # image(x,col = "grey")
	# points(all_sp_dat$Longitude,all_sp_dat$Latitude,col="red",pch=20,cex=0.75)
	# title(main = paste(species[i]),sub = paste("bg pts = ",bg_num,"  obs_num=",length(species_dat$long),"  area=",sum_cell_area," km2",sep=""))
	# dev.copy2pdf(file=paste("C:\\Daisy\\Current Projects\\Grass Paper\\images\\",species[i],"_obs_koeppen_zones.pdf",sep=""))
	#points(bg,col="black",cex=0.75)
	
	info<-cbind(species_name,obs_num,num_koep_zones,area_koep_km2,bg_num)
	
	#join data to list
	bg_SWD[[i]] <-  bg_sp_dat[,c("species","Latitude","Longitude","bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
	
	obs_summary[[i]] <- info
	obs_SWD[[i]]<- all_sp_dat[,c("species","Latitude","Longitude","bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
	
	message(paste(i))
}

	
	
#combine lists to a dataframes
background_SWD<-do.call("rbind",bg_SWD)
observation_SWD<-do.call("rbind",obs_SWD)
observation_summary<-do.call("rbind",obs_SWD)

#write out SWD file for background points
write.csv(background_SWD,row.names=FALSE,file=paste(dat.dir,"RK_background_SWD_8_8_12.csv",sep=""))

#write out SWD file for observations points
write.csv(observation_SWD,row.names=FALSE,file=paste(dat.dir,"observation_SWD_8_8_12.csv",sep=""))

#write out summary file for observations points
write.csv(observation_summary,row.names=FALSE,file=paste(dat.dir,"observation_koeppen_summary_8_8_12.csv",sep=""))
