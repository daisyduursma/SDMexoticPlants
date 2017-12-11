

#make sure workspace is clean
	rm(list = ls())
					


.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")

#get directories where data located
	work.dir<-"/data2/dduursma/exotic plants/"
	out.dir<-paste(work.dir,"outputs",sep="")
	keep.dir<-"/data2/dduursma/exotic_plants_files_wanted/"
	dir.create(paste(keep.dir))
	
	
# #read in observation SWD file
	# sp_dat<- read.table(paste(work.dir,"data/", "observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
	
# #make list of species
	# species <-as.vector(unique(sp_dat$species))
	

all_files<-list.files(paste(out.dir),full.names = TRUE)

for(i in 1:length(all_files)){

file_names<-strsplit(all_files[i],"\\/")
aa<-strsplit(file_names[[1]][6]," ")
new_file_name<-(paste(keep.dir,aa[[1]][1],"_",aa[[1]][2],sep=""))
dir.create(new_file_name)



file.copy(paste(all_files[i],"/maxentResults.csv",sep=""), paste(new_file_name
,"/maxentResults.csv",sep=""),overwrite=TRUE)

file.copy(paste(all_files[i],"/species_data_avg.asc",sep=""), paste(new_file_name
,"/species_data_avg.asc",sep=""),overwrite=TRUE)

for (ii in 0:4){

file.copy(paste(all_files[i],"/species_",ii,".lambdas",sep=""),paste(new_file_name
,"/species_",ii,".lambdas",sep=""))


file.copy(paste(all_files[i],"/species_",ii,"_data.asc",sep=""),paste(new_file_name
,"/species_",ii,"_data.asc",sep=""))


}
}
