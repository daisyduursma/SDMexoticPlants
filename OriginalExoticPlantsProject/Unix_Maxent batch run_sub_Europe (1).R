
#make sure workspace is clean
	rm(list = ls())
#load library 


.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


	library(dismo)
#get directories where data located
	work.dir<-"/data2/home/dduursma/exotic plants/"
	out.dir<-paste(work.dir,"/outputs/",sep="")
  dir.create(out.dir)
	
	

	
jar <-  "/data1/dduursma/R/x86_64-redhat-linux-gnu-library/dismo/java/maxent.jar"
if (file.exists(jar)) {}

#read in observation SWD file
	sp_dat<- read.table(paste(work.dir,"data/", " observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
  sp_dat<-sp_dat[,2:24]
	all_background<-read.table(paste(work.dir,"data/","RK_background_10000_2_10_12_8km_grid.csv", sep=""),header=TRUE, sep=",")
  all_background<-all_background[,3:ncol(all_background)]
#make list of species
	species <-as.vector(unique(sp_dat$species))
	

#for each species
for (i in 270:length(species)){
	#get occurance points for one species
		remove_dat<-sp_dat$species==species[i]
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
	#background points
		remove_dat<-all_background$species==species[i]
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
# 		background<-background[sample(nrow(background), 10000), ]
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
		ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
		#ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15")]
	#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
	#SEND TO MAXENT	
	#path to write out files
		new.out.dir<-(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe",sep=""))
	#maxent runs
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("-d", "betamultiplier=1","nohinge","nothreshold","replicates=2","randomseed","projectionlayers=/data2/home/dduursma/exotic plants/data"))
	#load the averge ascii as a raster
		sp_avg<-raster(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/species_data_avg.asc",sep=""))
	#get file with maxent results(
    dat1<-read.csv(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/maxentResults.csv",sep=""),row.names=1)
		sp_avg<-raster(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/species_data_avg.asc",sep=""))
	#get threshold value
		thresh<-dat1["species (average)","Maximum.training.sensitivity.plus.specificity.logistic.threshold"]
	#apply the threshold value
		fun <- function(x) { x[x<thresh] <- 0; return(x) }
		rc2 <- calc(sp_avg, fun)
		fun <- function(x) { x[x>=thresh] <- 1; return(x) }
		rc3 <- calc(rc2, fun)
	#write out raster
		writeRaster(rc3,filename=paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Max_training.asc",sep=""),overwrite=TRUE,NAflag=-9999)
                    thresh2<-dat1["species (average)","Equal.training.sensitivity.and.specificity.logistic.threshold"]   
					fun <- function(x) { x[x<thresh2] <- 0; return(x) }
		            rc2 <- calc(sp_avg, fun)
		            fun <- function(x) { x[x>=thresh2] <- 1; return(x) }
		            rc3 <- calc(rc2, fun)
		            writeRaster(rc3,filename=paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Equal_training.asc",sep=""),overwrite=TRUE,NAflag=-9999)
		                        thresh3<-dat1["species (average)","Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold"]   
		                        fun <- function(x) { x[x<thresh3] <- 0; return(x) }
		                        rc2 <- calc(sp_avg, fun)
		                        fun <- function(x) { x[x>=thresh3] <- 1; return(x) }
		                        rc3 <- calc(rc2, fun)
		                        writeRaster(rc3,filename=paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Balance_training.asc",sep=""),overwrite=TRUE,NAflag=-9999)
											thresh4<-dat1["species (average)","X10.percentile.training.presence.logistic.threshold"]   
		            						fun <- function(x) { x[x<thresh4] <- 0; return(x) }
											rc2 <- calc(sp_avg, fun)
											fun <- function(x) { x[x>=thresh4] <- 1; return(x) }
											rc3 <- calc(rc2, fun)
											writeRaster(rc3,filename=paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_10_percentile.asc",sep=""),overwrite=TRUE,NAflag=-9999)
    
                    		thresh5<-dat1["species (average)","Fixed.cumulative.value.5.logistic.threshold"]   
                    		fun <- function(x) { x[x<thresh5] <- 0; return(x) }
                    		rc2 <- calc(sp_avg, fun)
                    		fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
                    		rc3 <- calc(rc2, fun)
                    		writeRaster(rc3,filename=paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_cumulative_5.asc",sep=""),overwrite=TRUE,NAflag=-9999)
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
		zz5<-list.files(paste(new.out.dir),pattern="_0",full.names = TRUE)
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
		
		
	







