


#make sure workspace is clean
rm(list = ls())

r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
library(maptools)


out.dir<-"F:\\Current_Projects\\exotic_plants\\outputs\\all species\\"
grid.dir<-"F:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"

r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")

#if you do NOT already have a table of species observations do the following
 {
  
   #r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
#   locs<-rasterToPoints(r1,spatial=TRUE)
#   #normal data.frame of locs
#   locations<-as.data.frame(locs)[,c("x","y")]
#   locs2<-SpatialPoints(cbind(locations$x,locations$y))
#   
#   scen<-c("thresholded_current_10.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")
#   
#   for(i in 1:length(scen)){
#   sp_ascii<-list.files("f:/Current_Projects/exotic_plants/outputs",
#                        pattern=scen[i],recursive=TRUE,full.names=TRUE)
#   
#   locations<-as.data.frame(locs)[,c("x","y")]
#   for (ii in 1:length(sp_ascii)){
#     
#     rr<-raster(sp_ascii[ii])
#     dat1<-as.data.frame(extract(rr,locs))
#     sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
#     colnames(dat1)<-paste(sp_name,sep=" ")
#     locations<-cbind(locations,dat1)
#     message(ii)
#     }
#   #write output table
#   write.csv(locations,paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_27_04_3013.csv"))
#   
#   }
  
}


scen<-c("thresholded_current_10","thresholded_rcp45_2035_10","thresholded_rcp45_2065_10","thresholded_rcp85_2035_10","thresholded_rcp85_2065_10")

scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
 

##############for states###########################
{
  all_scen<-list()
  
for(i in 1:length(scen)){

dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_27_04_3013.csv",sep=""))
locs2<-SpatialPoints(cbind(dat$x,dat$y))

#find out which states point occurs in 
aa<-readShapePoly(paste(grid.dir,"NRM_Australia_states.shp",sep=""))
dat$states<-over(locs2,aa)[,"STATE"]
b<-area(r1)
dat$grid_size<-extract(b,locs2)

# for each state
st<-as.vector(unique(dat$states))

all_state<-list()

for (j in 1:length(st)){
   sub_dat<-subset(dat,states==st[j])
  st_area<-sum(sub_dat$grid_size)
  sp_dat<-sub_dat[,4:295]
  percent_suitable<-sp_dat*sub_dat$grid_size
  percent_suitable<-(colSums(percent_suitable)/st_area)*100
  species<-dimnames(sub_dat)[[2]][4:295]
  year<-rep(scenario[i],292)
  presence_suitable<-apply(sp_dat,2,FUN=max)
  state<-rep(st[j],292)
  all_state[[j]]<-cbind(year,species,state,percent_suitable,presence_suitable)
}
all_scen[[i]]<-do.call("rbind",all_state)
}

final_dat<-do.call("rbind",all_scen)

write.csv(final_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\state_percent_suitable_habitat.csv",row.names=FALSE)

}

}
}
############### for Australia
{
all_scen<-list()

for(i in 1:length(scen)){
  
  dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_27_04_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find areas
  b<-area(r1)
  dat$grid_size<-extract(b,locs2)
  st_area<-sum(dat$grid_size)
  sp_dat<-dat[,4:295]
  #calculate percent suitable habitat
  percent_suitable<-sp_dat*dat$grid_size
  percent_suitable<-(colSums(percent_suitable)/st_area)*100
  species<-dimnames(dat)[[2]][4:295]
  year<-rep(scenario[i],292)
  presence_suitable<-apply(sp_dat,2,FUN=max)
  Nation<-rep("Australia",292)
  all_state<-cbind(year,species,Nation,percent_suitable,presence_suitable)
  
  all_scen[[i]]<-all_state
}

final_dat<-do.call("rbind",all_scen)

write.csv(final_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australia_percent_suitable_habitat.csv",row.names=FALSE)


}






############################################################
############################################################
############################################################


#smaller regions

############################################################
############################################################
############################################################


#If you do not already have the factor of three data, need to this twice because it uses to much memory
{
# 
# r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
# r1<-disaggregate(r1,fact=3)
# locs<-rasterToPoints(r1,spatial=TRUE)
#   #normal data.frame of locs
#   locations<-as.data.frame(locs)[,c("x","y")]
#   locs2<-SpatialPoints(cbind(locations$x,locations$y))
#   
#   scen<-c("thresholded_current_10.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")
#   
#   for(i in 1:length(scen)){
#   sp_ascii<-list.files("f:/Current_Projects/exotic_plants/outputs",
#                        pattern=scen[i],recursive=TRUE,full.names=TRUE)
#   
#   locations<-as.data.frame(locs)[,c("x","y")]
#   for (ii in 151:292){
#     
#     rr<-raster(sp_ascii[ii])
#     dat1<-as.data.frame(extract(rr,locs))
#     sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
#     colnames(dat1)<-paste(sp_name,sep=" ")
#     locations<-cbind(locations,dat1)
#     message("scenario ", i, " species ",ii)
#     }
#   #write output table
#   write.csv(locations,paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
#   
#   }

}



#######for NRM

{

#keep only those species that have suitable habitat



#to get area
r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
r1<-disaggregate(r1,fact=3)
b<-area(r1)


all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"NRM.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$NRM_REGION)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$NRM_REGION))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,NRM_REGION==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scenario[i],ncol(sp_dat))
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
  dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"NRM.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$NRM_REGION)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$NRM_REGION))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,NRM_REGION==st[j])
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scenario[i],ncol(sp_dat))
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


write.csv(final_all_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\NRM_percent_suitable_habitat.csv",row.names=FALSE)


}


#######for LGA ##################3
{


r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
r1<-disaggregate(r1,fact=3)
b<-area(r1)


all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LGA.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LGA_CODE11)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LGA_CODE11))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LGA_CODE11==st[j])
    if (nrow(sub_dat)<3) next
    
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scenario[i],ncol(sp_dat))
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
  dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
  aa<-readShapePoly(paste(grid.dir,"LGA.shp",sep=""))
  dat2<-over(locs2,aa) 
  dat2$grid_size<-extract(b,locs2)
  cols<-ncol(dat2)+4
  cols_short<-ncol(dat2)-1
  dat3<-cbind(dat2,dat)
  dat3$remove<-is.na(dat3$LGA_CODE11)
  dat3<-subset(dat3,remove==FALSE)
  dat3<-dat3[,1:ncol(dat3)-1]
  
  # for each region
  st<-as.vector(unique(dat3$LGA_CODE11))
  all_state<-list()
  for (j in 1:length(st)){
    sub_dat<-subset(dat3,LGA_CODE11==st[j])
    if (nrow(sub_dat)<3) next
    
    st_area<-sum(sub_dat$grid_size)
    sp_dat<-sub_dat[,cols:ncol(sub_dat)]
    percent_suitable<-sp_dat*sub_dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    reg_dat<-sub_dat[1,1:cols_short]
    species<-dimnames(sp_dat)[[2]]
    year<-rep(scenario[i],ncol(sp_dat))
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


write.csv(final_all_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\LGA_percent_suitable_habitat.csv",row.names=FALSE)


}



#######for RAMSAR ##################3
{
  
  
  r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
  r1<-disaggregate(r1,fact=3)
  b<-area(r1)
  
  
  all_scen<-list()
  for(i in 1:length(scen)){
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"ramsar.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$RAMSAR_NAM)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$RAMSAR_NAM))
    all_state<-list()
    for (j in 15:length(st)){
      sub_dat<-subset(dat3,RAMSAR_NAM==st[j])
      if (nrow(sub_dat)<3) next
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"ramsar.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$RAMSAR_NAM)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$RAMSAR_NAM))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,RAMSAR_NAM==st[j])
      if (nrow(sub_dat)<3) next
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
  
  
  write.csv(final_all_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\ramsar_percent_suitable_habitat.csv",row.names=FALSE)
  
  
}



#######for CAPAD ##################3
{
  
  
  r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
  r1<-disaggregate(r1,fact=3)
  b<-area(r1)
  
  
  all_scen<-list()
  for(i in 1:length(scen)){
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"CAPAD.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$PA_ID)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$PA_ID))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,PA_ID==st[j])
      #nrow subdat <3 then continue to the next one
      if (nrow(sub_dat)<3) next
      
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"CAPAD.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$PA_ID)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$PA_ID))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,PA_ID==st[j])
      if (nrow(sub_dat)<3) next
      
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
  
  
  write.csv(final_all_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\CAPAD_percent_suitable_habitat.csv",row.names=FALSE)
  
  
}


#######for CAPAD restricted ##################3
{
  
  
  r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
  r1<-disaggregate(r1,fact=3)
  b<-area(r1)
  
  
  all_scen<-list()
  for(i in 1:length(scen)){
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species1_TO_150_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"CAPAD_restrictedt.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$PA_ID)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$PA_ID))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,PA_ID==st[j])
      #nrow subdat <3 then continue to the next one
      if (nrow(sub_dat)<3) next
      
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
    dat<-read.csv(paste(out.dir,scen[i],"_Presence_absence_lat_long_all_apecies_factor_of_3_species151_TO_292_06_05_3013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"CAPAD_restrictedt.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$PA_ID)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$PA_ID))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,PA_ID==st[j])
      if (nrow(sub_dat)<3) next
      
      
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,cols:ncol(sub_dat)]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      reg_dat<-sub_dat[1,1:cols_short]
      species<-dimnames(sp_dat)[[2]]
      year<-rep(scenario[i],ncol(sp_dat))
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
  
  
  write.csv(final_all_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\CAPAD_restricted_percent_suitable_habitat.csv",row.names=FALSE)
  
  
}













#fix names in files


dat_files<-list.files("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\",full.names=TRUE)[c(2,9)]

for (i in 1:2){

dat<-read.csv(dat_files[i])

aa<-as.character(dat$species)

sp <- strsplit(aa,".",fixed=T)
n<- sapply(sp,length)

aa[n==2] <- sapply(sp[n==2], paste, collapse=" ")
aa[n==3] <- sapply(sp[n==3], function(x)paste0(x[1]," ",x[2],"-",x[3]))


dat$species<-aa


write.csv(dat,dat_files[i],row.names = FALSE)

}









