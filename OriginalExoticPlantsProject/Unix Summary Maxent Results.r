rm(list = ls())
#load library 

.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")

#library(dismo)
#get directories where data located
work.dir<-"/data2/home/dduursma/exotic plants/"
out.dir<-paste(work.dir,"/outputs/",sep="")

#read in observation SWD file
sp_dat<- read.csv(paste(work.dir,"data/", " observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE)
species <-as.vector(unique(sp_dat$species))

files<-list.files(out.dir,pattern="_set1_bgRK_r1.0_nhnt")

#read in first file
#   dat1<-read.csv(paste(out.dir,files[1],"/maxentResults.csv",sep=""),row.names=1)["species (average)",]
# #get data needed
#   row.names(dat1) <- paste(files[1])
#   Run.Name<-paste(files[1])
#   a<-strsplit(Run.Name, "_")
#   a<-as.vector(a[[1]])
#   species<-a[1]
#   env.variable.set<-a[2]
#   background.data<-a[3]
#   betamultiplier<-a[4]
#   hinge.threshold<-a[5]
# #cbind the data needed 
#   nam_dat1<-cbind(species,env.variable.set,background.data,betamultiplier,hinge.threshold,dat1)


obs<-list()

#start loop and merge the rows together
for (i in 1:length(species)){
  dat1<-read.csv(paste(out.dir,files[i],"/maxentResults.csv",sep=""),row.names=1)["species (average)",]
  #get data needed
  row.names(dat1) <- paste(files[i])
  Run.Name<-paste(files[i])
  a<-strsplit(Run.Name, "_")
  a<-as.vector(a[[1]])
  species<-a[1]
  env.variable.set<-a[2]
  background.data<-a[3]
  betamultiplier<-a[4]
  hinge.threshold<-a[5]
  #cbind the data needed 
  obs[[i]]<-cbind(species,env.variable.set,background.data,betamultiplier,hinge.threshold,dat1)
 
}

	all_dat<-do.call("rbind",obs)
	
	write.csv(all_dat,row.names=FALSE,file=paste(out.dir,"Summary_MaxentResults_14_10_2011.csv",sep=""))
	