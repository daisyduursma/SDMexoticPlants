# 1) Add Australia lat and long
# 2)remove rows with NA values in lat and long
# 3)randomly select one obs per grid cell
# 4)for each species plot observations to be used, 2 maps - global and Australia




rm(list = ls())

#load packages
	library(raster)

	

#directorys
	work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")


# #get obs
	# dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed2.csv",sep=""))
	# latlong<-read.csv(paste(dat.dir,"Australia_no_lat_long_FINAL.csv",sep=""))

# #remove duplicates
	# dups <- duplicated(latlong)
	# latlong <- latlong[!dups,]
	# rm(dups)
	
# # Add Australia lat and long to dat
	# #get Aus observations with no lat and long
	# Ausdat<-subset(dat,Country..interpreted. == "Australia")
	# Ausdat$latTF<-is.na(Ausdat$Latitude)
	# Ausdat$longTF<-is.na(Ausdat$Latitude)
	# Ausdat<-subset(Ausdat,latTF==TRUE & longTF==TRUE)
	# Ausdat<-subset(Ausdat,select=c(species,Country..interpreted.,Locality))
	# #merge lat and longs
	# Ausdat<-merge(Ausdat,latlong,by ="Locality")
	
# #Add the updated Australian data back to the main data and remove duplicates
	# dat<-subset(dat, select =c("species","Latitude","Longitude","Country..interpreted.","Locality"))
	# dat2<-rbind(dat,Ausdat)
	# #rm(dat,Ausdat)
# #remove rows with blanks in lat and long
	# dat2$latTF<-is.na(dat2$Latitude)
	# dat2$longTF<-is.na(dat2$Latitude)
	# dat2<-subset(dat2,latTF==FALSE & longTF==FALSE)
# #remove columns of country, and locality and then remove duplicates, for any one species,there will only be one observations per unique lat and long.
	# dat3<-subset(dat2,select=c(species,Latitude,Longitude))
	# long<-as.vector(dat3$Longitude)
	# b<-as.numeric(long)
	# dat3$Longitude<-b
	# dat3<-(na.omit(dat3))
	# #rm(dat2)
	# dups <- duplicated(dat3)
	# dat3 <- dat3[!dups,]
	# #rm(dups)

#write.csv(dat3,paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long.csv",sep=""))

#read in dat3, see above code to make dat3

#dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long.csv",sep=""))

dat3<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value.csv",sep=""))

#get values that fall inside common land area
land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
r <- raster(paste(wclim.dir,"clay_5min2.asc",sep=""))
r<-expand(r, land_mask) 
r<-mask(r,land_mask)

locs<-dat3[,c("Longitude","Latitude")]
	obs_swd<-extract(r,locs)
	all_sp_dat<-cbind(dat3,obs_swd)
	#remove observations with NA values
	all_sp_dat<-na.omit(all_sp_dat)

	write.csv(all_sp_dat,paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked_3_09_12.csv",sep=""))



#select one obs per grid cell, regardless of landmass	
	# # create a RasterLayer with the extent from worldClim data
	# r<-raster(nrows=1800, ncols=4320, xmn=-180, xmx=180, ymn=-60, ymx=90, crs="+proj=longlat +datum=WGS84")
	# #r<-raster(nrows=1800, ncols=4320, xmn=-180, xmx=180, ymn=-60, ymx=90, crs=NA)
	# #asign unique value to each cell
	# r[]=1:ncell(r)
	# # get the cell number for each point
	# loc<-dat3[c("Longitude","Latitude")]
	# cell <- cellFromXY(r, loc)
	
	# dat3<-cbind(dat3,cell)
	
	dat3<-all_sp_dat
	dat3<-(na.omit(dat3))
	#get species names
	spp<-as.vector(unique(dat3$species))
	#make empty list
	undup <- list()
	for (i in 1:length(spp)){
		#extract data for one species
		sub_sp<-subset(dat3,species==spp[i])
		#Find out if rows are duplicates
		dup<-duplicated(sub_sp$RASTERVALU)
		#remove duplicates and keep only the first record in a grid cell
		undup[[i]] <- sub_sp[!dup,]
	}
	#turn list into dataframe
	#dat4<-do.call("rbind",undup)
	dat5<-do.call("rbind",undup)
	


#random process	
	# sp_obs<-list()
	# obs <- list()
	# for(ii in 1:length(species)){
		# sp_dat<-subset(dat3,species==paste(species[ii]))
		# #find the unique cell numbers
		# ucell<-unique(sp_dat$cell)
		# for(i in 1:length(ucell)){
			# subdat<-subset(sp_dat, cell==paste(ucell[i]))
			# l_subdat<-nrow(subdat)
			# if (l_subdat==1) {obs[[i]] <- subdat} else{
			# l_s<-sample(l_subdat,1)
			# l_subdat2<-subdat[l_s,]
			# obs[[i]]<-l_subdat2
			# }
		# }
		# sp_dat<-do.call("rbind",obs)
		# sp_obs[[ii]]<-sp_dat
	# }
		#final_obs<-do.call("rbind",sp_obs)
	
	
write.csv(dat5,paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked_one_per_cell_3_09_12.csv",sep=""))


		