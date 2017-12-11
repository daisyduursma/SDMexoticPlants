
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
#get directories where data located
	work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
	out.dir<-paste(work.dir,"outputs\\",sep="")
	data.dir<-paste(work.dir,"data\\",sep="")
	
#read in observation SWD file
	sp_dat<- read.table(paste(data.dir,"Observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
#species
	species <-as.vector(unique(sp_dat$species))
	

#read in species data
dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_one_per_cell.csv",sep=""))

	
#remove the unwanted file
	rm(sp_dat)
	
	for (i in 1:length(species){
	#get list of species files
		sp_files<-list.files(paste(out.dir,"Australia_Maps\\",sep=""),pattern=species[i],full.names = TRUE)
		
		
		r<-raster(paste(sp_files))
		plot(r,axes=FALSE,main=species[i])
		
		
		
		
		dev.copy2pdf(file=paste(work.dir,"images\\",species[i]," Australia_current_thresholded.pdf",sep=""))
		
		}
	