

#make sure workspace is clean
rm(list = ls())


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\"


sum_dat<-read.csv(paste(work.dir,"Eur_USA_observation_summary_2_10_12.csv",sep=""))

sp_dat<- read.table(paste(work.dir,"observations_2_10_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")
background<-read.table(paste(work.dir,"old/RK_background_10000_27_09_12_8km_grid.csv", sep=""),header=TRUE, sep=",")
#replace names of species 

names<-read.csv(paste(work.dir,"synonyms.csv",sep=""),header=FALSE)[c(1:16),]
was<-paste(names$V1," ",names$V2,sep="")
is<-paste(names$V3," ",names$V4,sep="")

write.csv(sum_dat,paste(work.dir,"Eur_USA_observation_summary_2_10_12.csv",sep=""))
write.csv(sp_dat,paste(work.dir,"observations_2_10_12_8km_grid_all_worldclimvar.csv",sep=""))
write.csv(background,paste(work.dir,"RK_background_10000_2_10_12_8km_grid.csv",sep=""))



###############################



names<-read.csv(paste(work.dir,"synonyms.csv",sep=""),header=FALSE)[c(17:18),]
was<-paste(names$V1,names$V2,sep="")
is<-paste(names$V3," ",names$V4,sep="")

for(i in 1:length(was)){
  
  a$species <- as.character( a$species)
  b$species <- as.character( b$species)
  e$species <- as.character( e$species)
  
  a$species[a$species == was[i]] <- is[i]
  b$species[b$species == was[i]] <- is[i]
 e$species[e$species == was[i]] <- is[i]
}

write.csv(sum_dat,paste(work.dir,"Eur_USA_observation_summary_2_10_12.csv",sep=""))
write.csv(sp_dat,paste(work.dir,"observations_2_10_12_8km_grid_all_worldclimvar.csv",sep=""))
write.csv(background,paste(work.dir,"RK_background_10000_2_10_12_8km_grid.csv",sep=""))