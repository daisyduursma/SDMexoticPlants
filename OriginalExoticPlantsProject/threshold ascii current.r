
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
  
#get directories where data located
	work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
	out.dir<-paste(work.dir,"outputs\\logistic\\",sep="")
	data.dir<-paste(work.dir,"data\\",sep="")
	
#read in observation SWD file
	sp_dat<- read.table(paste(data.dir,"Observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
#species
	species <-as.vector(unique(sp_dat$species))


#remove the unwanted file
	rm(sp_dat)
#Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
	aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
	aus_mask<-crop(aus_mask,aus_extent)

#for each species:
for (i in 1:40){
  
	#get list of species files
		sp_files<-list.files(paste(out.dir),pattern=species[i],full.names = TRUE)

	#for each of the files
	for(ii in 1:length(sp_files)){
		#find the reg number
			reg<-as.vector(strsplit(sp_files[ii],"_")[[1]])[6]
      setnum<-as.vector(strsplit(sp_files[ii],"_")[[1]])[4]
		#Get the full name of the current avg climate conditions
			current_files<-list.files(sp_files[ii],pattern="Current Climate_avg",full.names = TRUE)
		#load the ascii as a raster
			mean_current<-raster(current_files)
		
		#mask to Australia and trim the size of the ascii so it only has columns and rows where Australia is
		
		 # aus_r<-crop(mean_current,aus_extent)
	#	aus_r<-mask(aus_r,aus_mask)
			
		#make thresholded maps
		#get the threshold value
		
		
			dat1<-read.csv(paste(sp_files[ii],"\\maxentResults.csv",sep=""),row.names=1)
			thresh<-dat1["species (average)","Maximum.test.sensitivity.plus.specificity.logistic.threshold"]
		#apply the threshold value
			fun <- function(x) { x[x<thresh] <- 0; return(x) }
			rc2 <- calc(mean_current, fun)
			fun <- function(x) { x[x>=thresh] <- 1; return(x) }
			rc3 <- calc(rc2, fun)
		
		#Austraila_map
		
		# aus_r2<-crop(rc3,aus_extent)
		#aus_r<-mask(aus_r2,aus_mask)
			
	#		writeRaster(aus_r,filename=paste(out.dir,"Australia_Maps\\",species[i],"_current_",reg,"_",setnum,".asc",sep=""),overwrite=TRUE,NAflag=-9999)
		# #Global_map
			 writeRaster(rc3,filename=paste(out.dir,"Global_Maps\\",species[i],"_current_",reg,"_",setnum,".asc",sep=""),overwrite=TRUE,NAflag=-9999)
			 
			 message(i)
			
}
}

