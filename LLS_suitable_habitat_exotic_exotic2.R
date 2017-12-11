
# with this script you can work through each land division and find out the proportion of each area that is suitable for a certian species. This was not put into a function because every area has its own distinct set names, areas, etc. Before running the scripts two sections of code need to be run (first two sections)

#make sure workspace is clean
rm(list = ls())

r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
library(maptools)

scen<-c("Australia_current_thresholded_10.asc","Australia_average_rcp45_2035_thresholded_10.asc","Australia_average_rcp45_2065_thresholded_10.asc","Australia_average_rcp85_2035_thresholded_10.asc","Australia_average_rcp85_2065_thresholded_10.asc")
scen2<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")

out.dir<-"/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants_2/outputs/"
write.dir<-"/Users/daisy/Desktop/"
scen.dir<-paste0(out.dir,"/aa_all_species/scenario_Pres_abs/")
grid.dir<-"/Users/daisy/Desktop/LLS_shape_files/"

r1<-raster(paste0(out.dir,"Acetosa sagittata/Australia_average_rcp45_2035.asc"))
r1<-disaggregate(r1,fact=3)
b<-area(r1)

############exotic2
all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species/scenario_Pres_abs/",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LLS.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LLS)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LLS))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LLS==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scen2[i],ncol(sp_dat))
    presence_suitable<-apply(sp_dat,2,FUN=max)
    state<-rep(st[j],ncol(sp_dat))
    #keep data for only species with a presence
    dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
    all_state[[j]]<-dat4
  }
  all_scen[[i]]<-do.call("rbind",all_state)
}
final_dat_1_150<-do.call("rbind",all_scen)


all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species/scenario_Pres_abs/",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LLS.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LLS)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LLS))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LLS==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scen2[i],ncol(sp_dat))
    presence_suitable<-apply(sp_dat,2,FUN=max)
    state<-rep(st[j],ncol(sp_dat))
    #keep data for only species with a presence
    dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
    all_state[[j]]<-dat4
  }
  all_scen[[i]]<-do.call("rbind",all_state)
}
final_dat_151_292<-do.call("rbind",all_scen)


final_all_dat<-rbind(final_dat_1_150,final_dat_151_292)


write.csv(final_all_dat,paste0("/Users/daisy/Desktop/exotic2_LLS_percent_suitable_habitat.csv"),row.names=FALSE)


########exotic

out.dir<-"/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants/outputs/"
scen.dir<-paste0(out.dir,"/all species/")

all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"all species/thresholded_",scen2[i],"_10_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LLS.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LLS)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LLS))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LLS==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scen2[i],ncol(sp_dat))
    presence_suitable<-apply(sp_dat,2,FUN=max)
    state<-rep(st[j],ncol(sp_dat))
    #keep data for only species with a presence
    dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
    all_state[[j]]<-dat4
  }
  all_scen[[i]]<-do.call("rbind",all_state)
}
final_dat_1_150<-do.call("rbind",all_scen)


all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"all species/thresholded_",scen2[i],"_10_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LLS.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LLS)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LLS))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LLS==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scen2[i],ncol(sp_dat))
    presence_suitable<-apply(sp_dat,2,FUN=max)
    state<-rep(st[j],ncol(sp_dat))
    #keep data for only species with a presence
    dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
    all_state[[j]]<-dat4
  }
  all_scen[[i]]<-do.call("rbind",all_state)
}
final_dat_151_292<-do.call("rbind",all_scen)


final_all_dat<-rbind(final_dat_1_150,final_dat_151_292)


write.csv(final_all_dat,paste0("/Users/daisy/Desktop/exotic1_LLS_percent_suitable_habitat.csv"),row.names=FALSE)


dat1<-read.csv("/Users/daisy/Desktop/exotic2_LLS_percent_suitable_habitat.csv")

