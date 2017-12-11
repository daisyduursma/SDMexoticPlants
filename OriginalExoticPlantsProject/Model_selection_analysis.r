
#make sure workspace is clean
  rm(list = ls())
#load library 
	library(raster)
#get directories where data located


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\logistic\\"
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Australia_maps\\",sep="")


#read in csv of maxent results
  maxent<-read.csv(paste(out.dir,"Summary_MaxentResults_paramameter_tests.csv",sep=""))
  maxent$run<-paste(maxent$species,maxent$env.variable.set,maxent$betamultiplier)
  
#read in csv of AIC and BIc scores
  AIC_BIC<-read.csv(paste(work.dir,"AIC\\exotic_plants_model_selection.csv",sep=""))
 
  AICc<-aggregate(AICc_score~run,data=AIC_BIC,mean)
  AICc_stdev<-aggregate(AICc_score~run,data=AIC_BIC,sd)
  BIC<-aggregate(BIC_score~run,data=AIC_BIC,mean)
  BIC_stdev<-aggregate(BIC_score~run,data=AIC_BIC,sd)
  
#   training.AUC<-subset(maxent, select=Training.AUC )
#   test.AUC<-subset(maxent, select=Test.AUC )
#   
  #make run name for maxent
  sp<-as.vector(maxent$species)
  sp2<-strsplit(sp," ")
  all_dat<-do.call("rbind",sp2)
  maxent$sp2<-paste(all_dat[,1],"_",all_dat[,2],sep="")
  maxent$run<-paste(maxent$sp2,maxent$env.variable.set,"bgRK",maxent$betamultiplier,"nhnt",sep="_")
  
  #comine all thefiles
  aa<-cbind(AICc,AICc_stdev[,2],BIC[,2],BIC_stdev[,2])

  
  zz<-merge(maxent,aa,by = "run")
  
  write.csv(zz, paste(out.dir,"Summary_MaxentResults_AIC_BIC_paramameter_tests.csv"))
 