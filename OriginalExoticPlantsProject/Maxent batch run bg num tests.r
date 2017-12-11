
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(dismo)
#get directories where data located

work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\data\\"
out.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\outputs\\"
wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
	
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar)) {}

#read in SWD files
	sp_dat<- read.table(paste(work.dir,"observation_SWD.csv", sep=""),header=TRUE, sep=",")
	all_background<-read.table(paste(work.dir,"RK_background_SWD_40000.csv", sep=""),header=TRUE, sep=",")
#make list of species
	species <-as.vector(unique(sp_dat$species))
	

#for each species
for (i in 1:length(species)){

	#get occurance points for one species
		remove_dat<-sp_dat$species==species[i]
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
	#background points
		remove_dat<-all_background$species==species[i]
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
	
		ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
	#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
		
	#SEND TO MAXENT	
	
	#path to write out files
		new.out.dir<-(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt",sep=""))
	#maxent runs
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\"))
		
		
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		
	message(i)	
		
		}
		
		
	









	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	######################################################################################################################################################################################################################################################





	
		
		
		
		
		
	all_background<-read.table(paste(work.dir,"RG_Background_SWD.csv", sep=""),header=TRUE, sep=",")
#make list of species
	species <-as.vector(unique(sp_dat$species))[c(6,7,3,10)]
	

#for each species
for (i in 1:length(species)){

	#get occurance points for one species
		remove_dat<-sp_dat$species==species[i]
		occurence<-sp_dat[remove_dat,]
		occurence$p<-rep(1,nrow(occurence))
	#background points
		remove_dat<-all_background$species==species[i]
		background<-all_background[remove_dat,]
		background$p<-rep(0,nrow(background))
	#bind the dat so it is in form needed by maxent
		env_dat<-rbind(background,occurence)
	#list of environmental variables
		ev<-env_dat[,c("bio_1","bio_2","bio_3","bio_5","bio_13","bio_14","bio_6","bio_15")]
	#list of 0 and 1 values to show if species is present or absent at location
		pres<-as.vector(env_dat[,"p"])
		
	#SEND TO MAXENT	
	
	#path to write out files
		new.out.dir<-(paste(out.dir,species[i],"_set1_bgRG_r1.0_nhnt",sep=""))
	#maxent runs
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\"))
		
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		
		
		}
		
		
		
		
		
		
		
		
		
		
		
		
 
		new.out.dir<-(paste(out.dir,species[i],"_set2__gbRK__r2.0_nhnt",sep=""))
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=2","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\bccr_bcm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_t63_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cnrm_cm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk3_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\giss_aom_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\iap_fgoals1_0_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ipsl_cm4_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_hires_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_medres_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miub_echo_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mpi_echam5_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mri_cgcm2_3_2a_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ncar_pcm1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\"))  
		
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		
	
	ev<-env_dat[,c("bio_1","bio_2","bio_3","bio_5","bio_13","bio_14","bio_6","bio_15")]
	pres<-as.vector(env_dat[,"p"])
	
	
	#path to write out files
		new.out.dir<-(paste(out.dir,species[i],"_set1_gbRK_r1.0_nhnt",sep=""))
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\bccr_bcm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_t63_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cnrm_cm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk3_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\giss_aom_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\iap_fgoals1_0_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ipsl_cm4_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_hires_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_medres_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miub_echo_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mpi_echam5_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mri_cgcm2_3_2a_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ncar_pcm1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\"))  
		
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		
 
		new.out.dir<-(paste(out.dir,species[i],"_set1__gbRK__r2.0_nhnt",sep=""))
		maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=2","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Current Climate\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\bccr_bcm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_t63_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\cnrm_cm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk3_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\giss_aom_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\iap_fgoals1_0_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ipsl_cm4_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_hires_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_medres_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\miub_echo_g_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mpi_echam5_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\mri_cgcm2_3_2a_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\ncar_pcm1_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\"))  
		
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)
		
		
		
		
}






























"C:\\Daisy\\Raw Data\\Future Climates 2050\\bccr_bcm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\cccma_cgcm3_1_t63_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\cnrm_cm3_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\csiro_mk3_0_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_0_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\gfdl_cm2_1_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\giss_aom_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\iap_fgoals1_0_g_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\ipsl_cm4_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_hires_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\miroc3_2_medres_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\miub_echo_g_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\mpi_echam5_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\mri_cgcm2_3_2a_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\ncar_pcm1_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\","C:\\Daisy\\Raw Data\\Future Climates 2050\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\"           


list.files(C:\\Daisy\\Raw Data\\Future Climates 2050\\,full.names=TRUE)


		# ,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2020s_bio_5min_no_tile_asc\\"))
		# min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		# max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		# med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		# file.remove(min_files)
		# file.remove(max_files)
		# file.remove(med_files)

		# C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2020s_bio_5min_no_tile_asc\\"))
	# #remove unwanted files
		# min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		# max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		# med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		# file.remove(min_files)
		# file.remove(max_files)
		# file.remove(med_files)

#more maxent runs with other future climates


,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2020s_bio_5min_no_tile_asc\\"))
		min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
		max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
		med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)
		file.remove(min_files)
		file.remove(max_files)
		file.remove(med_files)

}

#maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\current_asc\\"))
 
# min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
# max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
# #med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)

# file.remove(min_files)
# file.remove(max_files)
# #file.remove(med_files)

# new.out.dir<-(paste(out.dir,species[i],"_",ev_set[k],"_",fold_abrv[ii],"_r2.0_nhnt",sep=""))
#maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=2","nohinge","nothreshold","replicates=5","nooutputgrids","projectionlayers=C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\cccma_cgcm2_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\csiro_mk2_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2020s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\hccpr_hadcm3_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2050s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_b2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\current_asc\\"))


# min_files<-list.files(paste(new.out.dir),pattern="_asc_min.asc",full.names = TRUE)
# max_files<-list.files(paste(new.out.dir),pattern="_asc_max.asc",full.names = TRUE)
# #med_files<-list.files(paste(new.out.dir),pattern="_asc_median.asc",full.names = TRUE)

# file.remove(min_files)
# file.remove(max_files)
# #file.remove(med_files)

 }
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ########################################
 
 
# # witholding a 20% sample for testing 
# fold <- kfold(pres, k=5)
# occtest <- pres[fold == 1, ]
# occtrain <- pres[fold != 1, ]

#C:\Daisy\Current Projects\Adelaide Species\outputs
#maxent(x=ev,p=pres,path=paste(new.out.dir))

#"C:\\Daisy\\Current Projects\\Adelaide Species\\outputs\\Pipt_mont_set2_rnkp_r1.0\\"

# maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("noaskoverwrite","betamultiplier=1","replicates=5","projectionlayers=C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\"))
# cla

# aa<-"noaskoverwrite"
# bb<-c("betamultiplier=1","betamultiplier=1.5","betamultiplier=2")
# cc<-"replicates=1"

 
 
# projec <- stack(c(
	# paste(wclim.dir,"bio_12",sep=""),
	# paste(wclim.dir,"bio_1",sep=""),
	# paste(wclim.dir,"bio_6",sep=""),
	# paste(wclim.dir,"bio_5",sep=""),
	# paste(wclim.dir,"bio_15",sep=""),
	# "C:\\Daisy\\Raw Data\\Soil Data\\clay_5min2.bil"
	# ))
# plot(predictors)


> maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("noaskoverwrite","betamultiplier=1","replicates=5","projectionlayers=list.files(C:\\Daisy\\Raw Data\\Future Climates\\)nies99_sres_a2a_2080s_bio_5min_no_tile_asc\\,C:\\Daisy\\Raw Data\\Future Climates\\nies99_sres_a2a_2050s_bio_5min_no_tile_asc\\"))

list.files(C:\\Daisy\\Raw Data\\Future Climates\\)

maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("noaskoverwrite","betamultiplier=1","replicates=5","projectionlayers=
	C:\\Daisy\\Raw Data\\WorldClim bioClim variables global\\bio_5m_esri_5_03_12\\bio\\,
	


"))


maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("noaskoverwrite","betamultiplier=1","replicates=1","projectionlayers=c(
	paste(wclim.dir,"bio_12",sep=""),
	paste(wclim.dir,"bio_1",sep=""),
	paste(wclim.dir,"bio_6",sep=""),
	paste(wclim.dir,"bio_5",sep=""),
	paste(wclim.dir,"bio_15",sep=""),
	C:\\Daisy\\Raw Data\\Soil Data\\clay_5min2.bil"))

maxent(x=ev,p=pres,args=c("noaskoverwrite","betamultiplier=1","replicates=1","projectionlayers=C:\\Daisy\\Raw Data\\WorldClim bioClim variables global\\bio_5m_esri_5_03_12\\bio\\"))



maxent(p=pres,a=background,path=paste(new.out.dir),-r,args=c("noaskoverwrite","betamultiplier=1","replicates=10","outputdirectory=C:\\Daisy\\Current Projects\\Adelaide Species\\output\\","projectionlayers=C:\\Daisy\\Raw Data\\WorldClim bioClim variables global\\bio_5m_esri_5_03_12\\bio\\"))


system(paste('"java"','-mx512m -jar "C:/Program 
Files/maxent/3.3/maxent.jar"', '-s "H:/me/input/Cal250.csv" -e 
"H:/me/input/BG.csv" -j "H:/me/input/V.csv" -o "H:/me/Out" -a')) 


maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1","replicates=2","outputdirectory=C:\\Daisy\\Current Projects\\Adelaide Species\\outputs\\"))


maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1","nohinge","replicates=10"))
maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1","nohinge","nothreshold","replicates=10"))

maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1.5","replicates=10"))
maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1.5","nohinge","replicates=10"))
maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=1.5","nohinge","nothreshold","replicates=10"))

maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=2","replicates=10"))
maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=2","nohinge","replicates=10"))
maxent(x=predictors,p=pres,a=background,args=c("betamultiplier=2","nohinge","nothreshold","replicates=10"))


 
 
