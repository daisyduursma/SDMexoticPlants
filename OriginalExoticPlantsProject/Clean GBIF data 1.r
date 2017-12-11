

#initial cleaning of GBIF data

# 1) remake csv files so they have full species name. These files with have species name, lat, long, country, and locality. 
# 2)remove duplicates
# 3)plot observations with lat and long locations on a map
#4)Moved to a new folder.



rm(list = ls())


11


#directory where data is
dat.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
work.dir<-paste(dat.dir,"GBIF_sp_data\\",sep="")
#list of species folders
sp_files<-list.files(work.dir)

#loop for each species folder
for(i in 12:length(sp_files)){

	#read in the species observations
	sp_csv<-list.files(paste(work.dir,sp_files[i],sep=""),pattern="occurrence",full.names=TRUE)
	dat<-read.csv(sp_csv,header=TRUE)
	dat<-dat[c("Country..interpreted.","Locality","Latitude","Longitude")]
	
	#get species name
	a<-as.vector(strsplit(sp_csv,split="GBIF_sp_data"))[[1]]
	b<-as.vector(strsplit(a[2],split="/"))[[1]]
	sp<-substr(b[1],2,nchar(b[1], type = "chars"))
	#vector of species name the length of the dat
	species<-rep(sp,nrow(dat))
	dat2<-cbind(species,dat)
	
	#remove duplicates
	dups <- duplicated(dat2)
	# keep the records that are _not_ duplicate
	dat3 <- dat2[!dups,]
	
	# fix lat and long that have 0, 0 values to NA
	zerozero<- which(dat3$Latitude == 0 & dat3$Longitude==0)
    dat3$Latitude[zerozero]<-NA
    dat3$Longitude[zerozero]<-NA

	
	
	#remove observations where lat, long are blank and if either Country..interpreted. or Locality are balnk.
	
	dat3$latTF<-is.na(dat3$Latitude)
	dat3$longTF<-is.na(dat3$Latitude)
	is.na(dat3$Locality[dat3$Locality==""])<-TRUE
	is.na(dat3$Country..interpreted.[dat3$Country..interpreted.==""])<-TRUE
	dat3$locTF<-is.na(dat3$Locality)
	dat3$couTF<-is.na(dat3$Country..interpreted.)
	
	#keep rows with known lat and long
	dat4<-subset(dat3,latTF==FALSE & longTF==FALSE)
	
	#find rows with known locality and country
	dat5<-subset(dat3,locTF==FALSE & couTF==FALSE)
	
	dat6<-rbind(dat4,dat5)
		
	#remove duplicates 
	dups <- duplicated(dat6)
	# keep the records that are _not_ duplicate
	dat7 <- dat6[!dups,]
	
	
	dat8<-subset(dat7,select=1:5)
	write.csv(dat8,paste(dat.dir,"aa_Species_locs_1st cleaning\\",sp,".csv",sep=""),row.names = FALSE,)
	
	}
	