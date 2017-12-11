
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
#get directories where data located


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\logistic\\"
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Global_maps\\",sep="")


	
#read in observation SWD file
	sp_dat<- read.table(paste(data.dir,"observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
#species
	spec <-as.vector(unique(sp_dat$species))

	
#Extent wanted for Aus maps
	#aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
	#aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
	#aus_mask<-crop(aus_mask,aus_extent)
  
#read in csv of maxent results
  maxent<-read.csv(paste(out.dir,"Summary_MaxentResults_paramameter_tests.csv",sep=""))
  maxent$run<-paste(maxent$species,maxent$env.variable.set,maxent$betamultiplier)
  
#read in csv of AIC and BIc scores
  AIC_BIC<-read.csv(paste(work.dir,"AIC\\exotic_plants_model_selection.csv",sep=""))
  
  AICc_dat<-aggregate(AICc_score~run,data=AIC_BIC,mean)
  BIC_dat<-aggregate(BIC_score~run,data=AIC_BIC,mean)
  
#for each species:

for (i in 1:length(spec[1:40])){

	sp_files<-list.files(paste(ascii.dir),pattern=spec[i],full.names = TRUE)
	
	#nf<-layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE),c(3,3), c(1,2,2),TRUE)
	#layout.show(nf)
	par(mfrow = c(2,2),mar = c(1,1,2,1))
	
	#Title, species, environmental variables, etc.
	sp_run<-(strsplit(sp_files,"\\\\")[[1]][8])
	sp<-strsplit(sp_run,"_")[[1]][1]
	env_var_a<-strsplit(sp_run,"_")[[1]][4]
	env_var<-strsplit(env_var_a,"\\.")[[1]][1]
	reg<-strsplit(sp_run,"_")[[1]][3]
	
	
	#get the AUC values, AICc, and BIC
	training.AUC<-subset(maxent, run==paste(sp,env_var,reg), select=Training.AUC )
	test.AUC<-subset(maxent, run==paste(sp,env_var,reg), select=Test.AUC )
  AICc<-subset(AICc_dat,run==paste(strsplit(sp," ")[[1]][1],strsplit(sp," ")[[1]][2],env_var,"bgRK",reg,"nhnt",sep="_"),select=AICc_score)
	BIC<-subset(BIC_dat,run==paste(strsplit(sp," ")[[1]][1],strsplit(sp," ")[[1]][2],env_var,"bgRK",reg,"nhnt",sep="_"),select=BIC_score)
	
	
	plot(raster(sp_files[1]),frame.plot=FALSE,main=spec[i],axes = FALSE)
	dat<-subset(sp_dat, species==spec[i])
	
	#add observations 
	points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.5)
	
	
	text(-170,95,paste(env_var, reg, sep="  "),cex=1,pos=4,,font=2)
 	text(-170,-15,paste("train.AUC= ",training.AUC),cex=1,pos=4)
	text(-170,-30,paste("test.AUC = ",test.AUC),cex=1,pos=4)
	text(-170,-45,paste("AICc = ",AICc, sep="  "),cex=1,pos=4)
	text(-170,-60,paste("BIC = ",BIC, sep="  "),cex=1,pos=4)
    
  #add australian observations
	#get species data

	for (ii in 2:length(sp_files)){
	  #Title, species, environmental variables, etc.
	  sp_run<-(strsplit(sp_files,"\\\\")[[ii]][8])
	  sp<-strsplit(sp_run,"_")[[1]][1]
	  env_var_a<-strsplit(sp_run,"_")[[1]][4]
	  env_var<-strsplit(env_var_a,"\\.")[[1]][1]
	  reg<-strsplit(sp_run,"_")[[1]][3]
	  
	  
	  #get the AUC values, AICc, and BIC
	  training.AUC<-subset(maxent, run==paste(sp,env_var,reg), select=Training.AUC )
	  test.AUC<-subset(maxent, run==paste(sp,env_var,reg), select=Test.AUC )
	  AICc<-subset(AICc_dat,run==paste(strsplit(sp," ")[[1]][1],strsplit(sp," ")[[1]][2],env_var,"bgRK",reg,"nhnt",sep="_"),select=AICc_score)
	  BIC<-subset(BIC_dat,run==paste(strsplit(sp," ")[[1]][1],strsplit(sp," ")[[1]][2],env_var,"bgRK",reg,"nhnt",sep="_"),select=BIC_score)
	  
	  
	  plot(raster(sp_files[ii]),frame.plot=FALSE,axes = FALSE)
	  
	  text(-170,95,paste(env_var, reg, sep="  "),cex=1,pos=4,,font=2)
	  text(-170,-15,paste("train.AUC= ",training.AUC),cex=1,pos=4)
	  text(-170,-30,paste("test.AUC = ",test.AUC),cex=1,pos=4)
	  text(-170,-45,paste("AICc = ",AICc, sep="  "),cex=1,pos=4)
	  text(-170,-60,paste("BIC = ",BIC, sep="  "),cex=1,pos=4)
	  
	  
		
	
}
  
  
	dev.copy2pdf(file=paste(ascii.dir,"images\\",sp,"_Global_suitable_habitat.pdf",sep=""))
  
}
  