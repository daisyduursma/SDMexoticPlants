
# 1)combine AVH and GBIF data for all species add a column that says where it is from

# 2)remove observations from gardens cultivated nuseries etc

# 3)fill in known lat and longs

# 4)remove observations with NA values.

# 5)remove duplicates


rm(list = ls())



#directory where data is
dat.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\data\\"
work.dir<-paste(dat.dir,"aa_Species_locs_1st cleaning\\",sep="")
#list of species folders
sp_files<-list.files(work.dir)

#loop for each species folder

locs <- data.frame(Country..interpreted.=character(0), Locality=character(0),Latitude=numeric(0),Longitude=numeric(0)) 

#words that remove rows. This is a mixture of English, Spanish, Portugese, French, Italian, German, and Dutch. 

 
for(i in 1:length(sp_files)){

	#read in the species observations
	
	dat<-read.csv(paste(work.dir,sp_files[i],sep=""),header=TRUE)
	
	locs<-rbind(locs,dat)
	
	
	# #remove duplicates, this keeps the first observation of the dupli
	dups <- duplicated(locs)
	# # keep the records that are _not_ duplicate
	locs <- locs[!dups,]
	
	
	}
	
#merge AVH data

#avh_dat<-read.csv(paste(dat.dir,"AVH.csv",sep=""),header=TRUE)

#locs2<-rbind(avh_dat,locs)

#write.csv(locs2,paste(dat.dir,"AVH_GBIF_all_species.csv",sep=""))

locs2<-read.csv(paste(dat.dir,"AVH_GBIF_all_species.csv",sep=""))

locs2a<-locs2[1:1000000,]
write.csv(locs2a,paste(dat.dir,"AVH_GBIF_all_species_A.csv",sep=""))

locs2b<-locs2[1000001:2000000,]
write.csv(locs2b,paste(dat.dir,"AVH_GBIF_all_species_B.csv",sep=""))

locs2c<-locs2[2000001:3000000,]
write.csv(locs2c,paste(dat.dir,"AVH_GBIF_all_species_C.csv",sep=""))
	
locs2d<-locs2[3000001:nrow(locs2),]	
write.csv(locs2d,paste(dat.dir,"AVH_GBIF_all_species_D.csv",sep=""))

	
	#Observations where lat, long are blank and if either Country..interpreted. or Locality is blank have already been removed

	
#read back in observations and merge into one file



aa<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_A.csv",sep=""))


bb<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_B.csv",sep=""))

cc<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_C.csv",sep=""))
	
dd<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_D.csv",sep=""))

ee<-rbind(aa,bb,cc,dd)

write.csv(ee,paste(dat.dir,"AVH_GBIF_all_species_cultivated_removed.csv",sep=""))
	
	
	
	
	
	
	
	
	
	
	
	
#remove rows if they have any of the key words

key_words<-c("greenhouse","ornemental","cultivar","cultivado","coltivata","culto","cultiver","cultive","garden","gardens","jardin","jardins","jardim","jardims","garten","giardino", "parque ecologico","nusrery","viveiro","vivaismo","viveiro","vivero","kwekerij")

dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed.csv",sep=""))

	
for (ii in 1:length(key_words)){

bad <- grepl(paste(key_words[ii]), all_dat$Locality,ignore.case=TRUE)

all_dat<-subset(all_dat,bad==FALSE)
	
}


	
write.csv(dat,paste(dat.dir,"AVH_GBIF_all_species_key_words_removed.csv",sep=""))
	
	
#add AVH and GBIF Identifier to 


rm(list = ls())

#directory where data is
dat.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\data\\"


dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed.csv",sep=""))
dat2<-dat[,5:9]
avh_dat<-read.csv(paste(dat.dir,"old\\AVH.csv",sep=""),header=TRUE)
a<-setdiff(dat2,avh_dat)
a$source<-"GBIF"
avh_dat$source<-"AVH"

all_dat<-rbind(a,avh_dat)

# #remove duplicates, this keeps the first observation of the dupli
dups <- duplicated(all_dat)
# # keep the records that are _not_ duplicate
all_dat <- all_dat[!dups,]


write.csv(all_dat,paste(dat.dir,"AVH_GBIF_all_species_key_words_removed2.csv",sep=""))
##################################################	
	
	
	
# #fill in lat and long that are known


# ##################################################



# rm(list = ls())

# #directory where data is
	# dat.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\data\\"
# #read in data
	# dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed2.csv",sep=""))
# #make a new column with a combined Country...interpreted and Locality
	# dat$place<-paste(dat$Country..interpreted.,dat$Locality,sep="_")
	
	
# #get rows with lat long and know locations, not duplicated 
	# dat2 <- with(dat,data.frame(Country..interpreted.,Locality,Latitude,Longitude))
	# # #remove duplicates, this keeps the first observation of the dupli
	# dups <- duplicated(dat2)
	# # # keep the records that are _not_ duplicate
	# dat2 <- dat2[!dups,]
	# #remove anything with NA values	
	# dat3<-na.omit(dat2)
	# #make new column
	# dat3$place<-paste(dat3$Country..interpreted.,dat3$Locality,sep="_")
	# #select columns needed
	# #dat3 <- with(dat3,data.frame(place,Latitude,Longitude))
	# #remove observations with key_word
	
	

# key_words<-c("","unknown","no name",".","no site name","

dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed.csv",sep=""))

	
for (ii in 1:length(key_words)){

bad <- grepl(paste(key_words[ii]), all_dat$Locality,ignore.case=TRUE)

all_dat<-subset(all_dat,bad==FALSE)
	
}



dat4<-subset(dat3, !duplicated(dat3$place) 


	
key_words<-c("unknown","not available")

dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_species_key_words_removed.csv",sep=""))

	
for (ii in 1:length(key_words)){

bad <- grepl(paste(key_words[ii]), dat$Locality,ignore.case=TRUE)

dat<-subset(dat,bad==FALSE)
	
}



#divide up original data into 2 sets of data 1)rows with lat and long filled in 2)rows with lat and long not filled in
	#find lat and long that have NA values		
	dat$latTF<-is.na(dat$Latitude)
	dat$longTF<-is.na(dat$Latitude)
	#divide into 2 datasets
	dat_known<-subset(dat,latTF==FALSE & longTF==FALSE)
	dat_unknown<-subset(dat,latTF==TRUE & longTF==TRUE)
	#for data with unknown locations check to see if lat and long can be filled in
	#1st step is to find rows with known locality and country
	is.na(dat_unknown$Locality[dat_unknown$Locality==""])<-TRUE
	is.na(dat_unknown$Country..interpreted.[dat_unknown$Country..interpreted.==""])<-TRUE
	dat_unknown$locTF<-is.na(dat_unknown$Locality)
	dat_unknown$couTF<-is.na(dat_unknown$Country..interpreted.)
	#keep rows with known Locality and Country..interpreted.
	dat_unknown2<-subset(dat_unknown,locTF==FALSE & couTF==FALSE)
	#get the columns needed for merge
	dat_unknown2 <- with(dat_unknown2,data.frame(place,Country..interpreted.,Locality,species))
#merge known lat and longs by place
	dat_fix_unknown2<-merge(dat_unknown2,dat3,by=intersect(names(dat_unknown2), names(dat3)))

	
	
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
	
	