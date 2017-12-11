args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
 eval(parse(text=args[[i]]))
}

#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")

	library(dismo)
	

options(java.parameters = "-Xmx1g" )

#read in observation SWD file
sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
all_background<-read.table(paste(work.dir,"data/","RK_background_10000_05_11_12_8km_grid.csv", sep=""),header=TRUE, sep=",")


#get occurance points for one species
		remove_dat<-sp_dat$species==spec
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
		#background points
		remove_dat<-all_background$species==spec
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
		
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
	
ev<-env_dat[,c("bioclim_01","bioclim_05","bioclim_06","bioclim_12","bioclim_15","clay_5min2")]
		#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
		
	#SEND TO MAXENT	
	
new.out.dir<-(paste(out.dir,spec,"_final",sep=""))
#maxent runs
maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("-d","-x","-r","betamultiplier=1","nohinge","nothreshold","replicates=5","randomseed","projectionlayers=/data2/home/dduursma/exotic_plants/data/current_climate/current,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_cccma-cgcm31_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_cccma-cgcm31_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ccsr-miroc32hi_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ccsr-miroc32hi_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ccsr-miroc32med_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ccsr-miroc32med_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_cnrm-cm3_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_cnrm-cm3_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_csiro-mk30_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_csiro-mk30_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_gfdl-cm20_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_gfdl-cm20_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_gfdl-cm21_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_gfdl-cm21_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_giss-modeleh_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_giss-modeleh_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_giss-modeler_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_giss-modeler_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_iap-fgoals10g_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_iap-fgoals10g_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_inm-cm30_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_inm-cm30_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ipsl-cm4_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ipsl-cm4_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_mpi-echam5_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_mpi-echam5_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_mri-cgcm232a_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_mri-cgcm232a_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ncar-ccsm30_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ncar-ccsm30_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ncar-pcm1_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ncar-pcm1_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ukmo-hadcm3_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ukmo-hadcm3_2065,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ukmo-hadgem1_2035,/data2/home/dduursma/exotic_plants/data/future_climate/RCP45_ukmo-hadgem1_2065"))
		
		
		min_files<-list.files(paste(new.out.dir),pattern="_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_median.asc",full.names = TRUE)
    std_files<-list.files(paste(new.out.dir),pattern="_stddev.asc",full.names = TRUE)
		zz1<-list.files(paste(new.out.dir),pattern="_avg.csv",full.names = TRUE)
		zz2<-list.files(paste(new.out.dir),pattern="_stdev.csv",full.names = TRUE)
		zz3<-list.files(paste(new.out.dir),pattern="_max.csv",full.names = TRUE)
		zz4<-list.files(paste(new.out.dir),pattern="_median.csv",full.names = TRUE)
		zz5<-list.files(paste(new.out.dir),pattern="_min.csv",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
    file.remove(std_files)
		file.remove(zz1)
		file.remove(zz2)
		file.remove(zz3)
		file.remove(zz4)
		file.remove(zz5)
		






		
		
