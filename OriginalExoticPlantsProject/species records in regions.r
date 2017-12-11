rm(list = ls())


# make table for each state with column for each analyis
library(raster)
library(maptools)



work.dir<-"f:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"Website\\species_records\\",sep="")
grid.dir<-"f:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"

#list of polygon files ($ means that it is the end of a string)
grids<-unlist(strsplit(list.files("f:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\",pattern=".shp$"),".shp"))[2:8]


sp_dat<- read.table(paste(work.dir,"data\\", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")[,c(1:3)]

#locs<-sp_dat[,c("Longitude","Latitude")]
locs<-SpatialPoints(cbind(sp_dat$Longitude,sp_dat$Latitude))



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

  
