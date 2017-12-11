rm(list = ls())


# make table for each state with column for each analyis
library(raster)
library(maptools)

work.dir<-"/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants_2/"
out.dir<-paste(work.dir,"outputs/aa_all_species/species_records/",sep="")
#dir.create(out.dir)
grid.dir<-"d:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"

#list of polygon files ($ means that it is the end of a string)
grids<-unlist(strsplit(list.files(grid.dir,pattern=".shp$"),".shp"))[3:9]


sp_dat<- read.csv(paste0(work.dir,"data\\", "Final_observation_bias_removed_21_10_13.csv"),header=TRUE, sep=",")[,c(1:3)]
#keep only species of interest
#get species names
sp<-list.files(paste0(work.dir,"outputs"))[2:254]

sp_dat2<-list()
for(i in 1:length(sp)){
  dat<-subset(sp_dat,species==sp[i])
  sp_dat2[[i]]<-dat
  
}

sp_dat3<-do.call(rbind,sp_dat2)





locs<-SpatialPoints(cbind(sp_dat$lon,sp_dat$lat))



for(i in 1:length(grids)){
  
  polys<-readShapePoly(paste(grid.dir,grids[i],".shp",sep=""))
  
  dat1<-over(locs,polys)
  dat2<-cbind(sp_dat,dat1)
  #determine rows in dat1 where all columns in a row have NA values, these will be removed
  dat2<-cbind(dat2,all_na<-apply(dat1,1,function(x)any(!is.na(x))))
  P_tab<-subset(dat2,all_na <- apply(dat1, 1, function(x) any(!is.na(x)))==TRUE)
  P_tab<-P_tab[,c(1,4:ncol(P_tab))]
  P_tab$dups<-duplicated(P_tab)
  P_tab<-subset(P_tab,dups=="FALSE")
  P_tab2<-P_tab[,c(1:(ncol(P_tab)-2))]
  
  #################ASK REMKO HOW TO MAKE THE TABLE AN OBJECT IN MY WORKING ENVIRONMENT############## 
  #write output table
  write.csv(P_tab2,paste(out.dir,grids[i],"_species_recorded.csv"))
  
  
}

  
