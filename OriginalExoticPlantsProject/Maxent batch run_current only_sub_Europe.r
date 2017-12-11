
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(dismo)
#get directories where data located

work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\sub_Europe\\"
dir.create(out.dir)

wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
	
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar)) {}

#read in SWD files
	sp_dat<- read.table(paste(work.dir,"sub_Europe_observations_17_09_2012.csv", sep=""),header=TRUE, sep=",")
	all_background<-read.table(paste(work.dir,"RK_background_10000_10_09_12_8km_grid.csv", sep=""),header=TRUE, sep=",")[,c(2:24)]
#make list of species
	
	species<-c("Euphorbia helioscopia", "Fallopia convolvulus", "Festuca arundinacea", "Ficus carica", "Fumaria muralis", "Galium palustre", "Lycium barbarum")
	
  
	
	#Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
	#mask of Australia
	aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
	aus_mask<-crop(aus_mask,aus_extent)
	
	

#for each species
for (i in 1:length(species)){

	#get occurance points for one species}
	
		remove_dat<-sp_dat$species==species[i]
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
	#background points
		remove_dat<-all_background$species==species[i]
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
  #get 10,000 background points
	#background<-background[sample(nrow(background), 10000), ]
    
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
	
	#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
	#set1
		ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
		
	#path to write out files
		new.out.dir<-(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe",sep=""))
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("-d", "betamultiplier=1","nohinge","nothreshold","replicates=5","nooutputgrids","randomseed","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\"))
		
		
		#load the averge ascii as a raster
		sp_avg<-raster(paste(new.out.dir,"\\species_Current Climate_avg.asc",sep=""))
    
		dat1<-read.csv(paste(new.out.dir,"\\maxentResults.csv",sep=""),row.names=1)
		thresh<-dat1["species (average)","Maximum.test.sensitivity.plus.specificity.logistic.threshold"]
		#apply the threshold value
		fun <- function(x) { x[x<thresh] <- 0; return(x) }
		rc2 <- calc(sp_avg, fun)
		fun <- function(x) { x[x>=thresh] <- 1; return(x) }
		rc3 <- calc(rc2, fun)
				
		writeRaster(rc3,filename=paste(new.out.dir,"\\",species[i],"_current_threshold.asc",sep=""),overwrite=TRUE,NAflag=-9999)
		
	
    #make pdf maps
    par(mfrow = c(1,2),mar = c(1,1,2,1))
		
		#Title, species, environmental variables, etc.
		sp_run<-"set1_bgRK_r1.0_nhnt_sub_Europe"
		sp<-species[i]
	
    #get the AUC values, 
		training.AUC<-dat1["species (average)","Training.AUC"]
		test.AUC<-dat1["species (average)","Test.AUC"]
		
    #global maps
		plot(rc3,frame.plot=FALSE,main=paste(species[i], " sub_Europe",sep=""),axes = FALSE)
		text(-170,125,paste("train.AUC= ",training.AUC),cex=1,pos=4)
		text(-170,105,paste("test.AUC = ",test.AUC),cex=1,pos=4)
		points(occurence$Longitude,occurence$Latitude,col="red",pch=20,cex=.1)
		
		
		#Australia
		aus_r2<-crop(rc3,aus_extent)
		plot(aus_r2,frame.plot=FALSE,axes = FALSE)
		#add observations 
		points(occurence$Longitude,occurence$Latitude,col="red",pch=20,cex=.7)
				
		dev.copy2pdf(file=paste("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\","Suitable habitat pdf\\",species[i],"_suitable_habitat_sub_Europe.pdf",sep=""))	
		  
		min_files<-list.files(paste(new.out.dir),pattern="_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_median.asc",full.names = TRUE)
		zz1<-list.files(paste(new.out.dir),pattern="_avg.csv",full.names = TRUE)
		zz2<-list.files(paste(new.out.dir),pattern="_stdev.csv",full.names = TRUE)
		zz3<-list.files(paste(new.out.dir),pattern="_max.csv",full.names = TRUE)
		zz4<-list.files(paste(new.out.dir),pattern="_median.csv",full.names = TRUE)
		zz5<-list.files(paste(new.out.dir),pattern="_min.csv",full.names = TRUE)
		file.remove(zz5)
		zz5<-list.files(paste(new.out.dir),pattern="_1",full.names = TRUE)
		file.remove(zz5)
		zz5<-list.files(paste(new.out.dir),pattern="_2",full.names = TRUE)
		file.remove(zz5)
		zz5<-list.files(paste(new.out.dir),pattern="_3",full.names = TRUE)
		file.remove(zz5)
		zz5<-list.files(paste(new.out.dir),pattern="_4",full.names = TRUE)
		file.remove(zz5)	
		
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		file.remove(zz1)
		file.remove(zz2)
		file.remove(zz3)
		file.remove(zz4)
	
  message(i)  
  }
  
  
  
  
  