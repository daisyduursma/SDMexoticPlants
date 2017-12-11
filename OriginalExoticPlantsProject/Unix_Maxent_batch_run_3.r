args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
 eval(parse(text=args[[i]]))
}

.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")

	library(dismo)
	

	
jar <-  "/data1/dduursma/R/x86_64-redhat-linux-gnu-library/dismo/java/maxent.jar"
if (file.exists(jar)) {}
	

#read in observation SWD file
	sp_dat<- read.table(paste(work.dir,"data/", " observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
	all_background<-read.table(paste(work.dir,"data/","RK_background_10000_2_10_12_8km_grid.csv", sep=""),header=TRUE, sep=",")

	

#get occurance points for one species
		remove_dat<-sp_dat$species==spec
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
		occurence<-occurence[,2:ncol(occurence)]
	#background points
		remove_dat<-all_background$species==spec
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
		background<-background[,3:ncol(background)]
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
	
		ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
		#ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15")]
	#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
		
	#SEND TO MAXENT	
	
	#path to write out files
		new.out.dir<-(paste(out.dir."test/",spec,"_set1_bgRK_r1.0_nhnt",sep=""))
	#maxent runs
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=2","nooutputgrids","raw","projectionlayers=/data2/home/dduursma/exotic_plants/data"))
		
		
		min_files<-list.files(paste(new.out.dir),pattern="_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_median.asc",full.names = TRUE)
		zz1<-list.files(paste(new.out.dir),pattern="_avg.csv",full.names = TRUE)
		zz2<-list.files(paste(new.out.dir),pattern="_stdev.csv",full.names = TRUE)
		zz3<-list.files(paste(new.out.dir),pattern="_max.csv",full.names = TRUE)
		zz4<-list.files(paste(new.out.dir),pattern="_median.csv",full.names = TRUE)
		zz5<-list.files(paste(new.out.dir),pattern="_min.csv",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		file.remove(zz1)
		file.remove(zz2)
		file.remove(zz3)
		file.remove(zz4)
		file.remove(zz5)
		
	message(i)	
		
		}
		
		
