
#make sure workspace is clean
	rm(list = ls())
					# library(dismo)
#get directories where data located

work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
data.dir<-paste(work.dir,"data\\",sep="")
out.dir<-paste(work.dir,"outputs\\",sep="")


	
# #read in observation SWD file
	# sp_dat<- read.table(paste(data.dir, "observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
	
# #make list of species
	# species <-as.vector(unique(sp_dat$species))[1:4]

	
	sp_files<-list.files(paste(out.dir),full.names = TRUE,pattern="set")
	all_details<-list()
	f_details<-list()
for(i in 1:length(sp_files)){

for(ii in 0:4){

jj<-ii+1

aa<-strsplit(sp_files[i],"\\\\")[[1]][6]
bb<-strsplit(aa,"_set")[[1]][1]

#make a row of data for each of the three elements that are needed in EMNTOOLS
f_details[[jj]] <- paste(data.dir,"Observations_by_species\\",bb,".csv,",sp_files[i],"\\species_",ii,"_data.asc,",sp_files[i],"\\species_",ii,".lambdas",sep="")


}
all_details[[i]]<-do.call("rbind",f_details)
}
all_dat<-do.call("rbind",all_details)

	
	write.csv(all_dat,paste(work.dir,"AIC\\exotic_plants.csv",sep=""))
