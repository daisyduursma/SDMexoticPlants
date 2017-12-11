

#initial cleaning of GBIF data

# 1) remake csv files so they have full species name. These files with have species name, lat, long, country, and locality. 
# 2)remove duplicates
# 3)plot observations with lat and long locations on a map
#4)Moved to a new folder.



rm(list = ls())




#directory where data is
dat.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
work.dir<-paste(dat.dir,"aa_Species_locs_1st cleaning\\",sep="")
#list of species folders
sp_files<-list.files(work.dir)

#loop for each species folder

locations <- data.frame(Country..interpreted.=character(0), Locality=character(0),Latitude=numeric(0),Longitude=numeric(0)) 
 
for(i in 1:length(sp_files)){

	#read in the species observations
	
	dat<-read.csv(paste(work.dir,sp_files[i],sep=""),header=TRUE)[c(2:5)]
	
	locations<-rbind(locations,dat)
	
	
	# #remove duplicates, this keeps the first observation of the dupli
	dups <- duplicated(locations)
	# # keep the records that are _not_ duplicate
	locations <- locations[!dups,]
	
	
	}
		
		
		
	
	#remove observations where lat, long are filled but if either Country..interpreted. or Locality is blank it is removed.
	
	#dat3$latTF<-is.na(dat3$Latitude)
	#dat3$longTF<-is.na(dat3$Latitude)
	
	locations$locTF<-is.na(locations$Locality)
	locations$couTF<-is.na(locations$Country..interpreted.)
	
	#keep rows with known Locality and Country..interpreted.
	locations2<-subset(locations,locTF==FALSE & couTF==FALSE)
	
	locations2$latTF<-is.na(locations2$Latitude)
	locations2$longTF<-is.na(locations2$Latitude)
	
	locations3<-subset(locations2,latTF==TRUE)
	locations4<-subset(locations2,longTF==TRUE)
	
	locations5<-rbind(locations3,locations4)
	
	# #remove duplicates, this keeps the first observation of the dupli
	dups <- duplicated(locations)
	# # keep the records that are _not_ duplicate
	locations5 <- locations5[!dups,]
	
	
	
	
	
	
	locations6<-subset(locations5,select=1:4)
	locations6<-subset(locations6,Country..interpreted.== "Australia")
	
	write.csv(locations6,paste(dat.dir,"aa_Species_locs_1st cleaning\\","Australia_no_lat_long.csv",sep=""),row.names = FALSE,)
	
	
	
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	#make a new column with a combined Country...interpreted and Locality
	locations2$place<-paste(locations2$Country..interpreted.,locations2$Locality,sep="_")
	
	a$place<-sort(locations2$place)
	a<-do.call("rbind",a)
	
	locations3<-cbind(a,locations2)
	
	


	
	
	# dat6<-rbind(dat4,dat5)
		
	# #remove duplicates 
	# dups <- duplicated(dat6)
	# # keep the records that are _not_ duplicate
	# dat7 <- dat6[!dups,]
	
	
	dat8<-subset(dat7,select=1:5)
	write.csv(dat8,paste(dat.dir,"aa_Species_locs_1st cleaning\\",sp,".csv",sep=""),row.names = FALSE,)
	
	}
	
	sort
	
	