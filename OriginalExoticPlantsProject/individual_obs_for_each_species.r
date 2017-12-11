#make individual observation .csv files for each species, this is needed to calculate AIC and BIC in ENMtools.


#make sure workspace is clean
	rm(list = ls())
					# library(dismo)
#get directories where data located

work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\data\\"

#read in SWD files
	sp_dat<- read.table(paste(work.dir,"observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
	
	species <-as.vector(unique(sp_dat$species))
	

#for each species
for (i in 1:length(species)){

	#get occurance points for one species
		remove_dat<-sp_dat$species==species[i]
		occurence<-sp_dat[remove_dat,]
	
		
		write.csv(occurence,paste(work.dir,"Observations_by_species\\",species[i],".csv",sep=""))
		}