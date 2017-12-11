
#make sure workspace is clean
rm(list = ls())

#get directory
work.dir<-"D:\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste0(work.dir,"outputs\\")

#list the output files
files<-list.files(out.dir,full.names=TRUE)
species<-list.files(out.dir)

obs<-list()
#start loop and merge the rows together
for (i in 1:length(files)){

dat1<-read.csv(paste0(files[i],"\\maxentResults.csv"),row.names=1)["species (average)",]
dat1$species<- species[i]
	
obs[[i]] <- dat1

}

	all_dat<-do.call("rbind",obs)
	
	write.csv(all_dat,row.names=FALSE,file=paste(out.dir,"Summary_Maxent_19_11_13.csv",sep=""))
	