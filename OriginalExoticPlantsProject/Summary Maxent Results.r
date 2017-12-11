
#make sure workspace is clean
rm(list = ls())

#get directory
data.dir<-"c:\\daisy\\Current_Projects\\exotic_plants\\outputs\\"

#list the output files
files<-list.files(data.dir,pattern = "_final2")
# 
# #read in first file
#   dat1<-read.csv(paste(data.dir,files[1],"\\maxentResults.csv",sep=""),row.names=1)["species (average)",]
# #get data needed
#   row.names(dat1) <- strsplit(paste(files[1]),"_")[[1]][1]
#   
# 
# dat1<-read.csv(paste(data.dir,files[3],"\\maxentResults.csv",sep=""),row.names=1)["species (average)",]
# #get data needed
# row.names(dat1) <- paste(files[3])
# 
# #cbind the data needed 
# nam_dat2<-cbind(species,env.variable.set,background.data,betamultiplier,hinge.threshold,dat1)
# 
# c_names<-intersect(names(nam_dat2), names(nam_dat1))
# 

obs<-list()

#start loop and merge the rows together
for (i in 1:length(files)){

dat1<-read.csv(paste(data.dir,files[i],"\\maxentResults.csv",sep=""),row.names=1)["species (average)",]
dat1$species<- strsplit(paste(files[i]),"_")[[1]][1]
	
obs[[i]] <- dat1

}

	all_dat<-do.call("rbind",obs)
	
	write.csv(all_dat,row.names=FALSE,file=paste(data.dir,"Summary_Maxent_23_05_12.csv",sep=""))
	