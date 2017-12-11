
# with this script you can work through each land division and find out the proportion of each area that is suitable for a certian species. This was not put into a function because every area has its own distinct set names, areas, etc. Before running the scripts two sections of code need to be run (first two sections)

#make sure workspace is clean
rm(list = ls())

r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
library(maptools)


out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\"
write.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\aa_all_species\\PrcntSUITregion\\"
scen.dir<-paste0(out.dir,"\\aa_all_species\\scenario_Pres_abs\\")
grid.dir<-"D:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"


r1<-raster("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\Acetosa sagittata\\Australia_average_rcp45_2035.asc")


  locs<-rasterToPoints(r1,spatial=TRUE)
  #normal data.frame of locs
  locations<-as.data.frame(locs)[,c("x","y")]
  locs2<-SpatialPoints(cbind(locations$x,locations$y))
  
  scen<-c("Australia_current_thresholded_10.asc","Australia_average_rcp45_2035_thresholded_10.asc","Australia_average_rcp45_2065_thresholded_10.asc","Australia_average_rcp85_2035_thresholded_10.asc","Australia_average_rcp85_2065_thresholded_10.asc")

   scen2<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
   scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
  


#Table with species observations for each gridcell
{  
for(i in 1:length(scen)){
    sp_ascii<-list.files("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\",
                         pattern=scen[i],recursive=TRUE,full.names=TRUE)
    locations<-as.data.frame(locs)[,c("x","y")]
  for (ii in 1:length(sp_ascii)){
    
    rr<-raster(sp_ascii[ii])
    dat1<-as.data.frame(extract(rr,locs))
    sp_name<-strsplit(sp_ascii[ii],"/")[[1]][2]
    colnames(dat1)<-paste(sp_name)
    locations<-cbind(locations,dat1)
    message(ii)
  }
  #write output table
  write.csv(locations,paste0(out.dir,"aa_all_species\\",scen2[i],"_Presence_absence_lat_long_all_apecies_25_11_3013.csv"))
  
  }
  
  
}

#for smaller areas make a table with species observations for each gridcell but by factor of 3
{

#get ascii names and short names
  scen<-c("Australia_current_thresholded_10.asc","Australia_average_rcp45_2035_thresholded_10.asc","Australia_average_rcp45_2065_thresholded_10.asc","Australia_average_rcp85_2035_thresholded_10.asc","Australia_average_rcp85_2065_thresholded_10.asc")
  scen2<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")

#disagregate files so we can better captute the fine detail
  
r1<-raster("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\Acetosa sagittata\\Australia_average_rcp45_2035.asc")

  r1<-disaggregate(r1,fact=3)
  locs<-rasterToPoints(r1,spatial=TRUE)
  #normal data.frame of locs
  
  
#run loop for the different scenatrios   
  for(i in 1:length(scen)){
    sp_ascii<-list.files("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\",
                         pattern=scen[i],recursive=TRUE,full.names=TRUE)
#for half the species find out if pres or absent  
    locations<-as.data.frame(locs)[,c("x","y")]
    for (ii in 131:253){
      rr<-raster(sp_ascii[ii])
      dat1<-as.data.frame(extract(rr,locs))
      sp_name<-strsplit(sp_ascii[ii],"/")[[1]][2]
      colnames(dat1)<-paste(sp_name,sep=" ")
      locations<-cbind(locations,dat1)
      message("scenario ", i, " species ",ii)
    }
  #write output table
  write.csv(locations,paste0(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv"))
 }
  
  
}



############### for States
{
  all_scen<-list()
  
for(i in 1:length(scen)){

dat<-read.csv(paste(scen.dir,scenario[i],"_Presence_absence_lat_long_all_apecies_25_11_2013.csv",sep=""))
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
  sp_dat<-sub_dat[,4:256]
  percent_suitable<-sp_dat*sub_dat$grid_size
  percent_suitable<-(colSums(percent_suitable)/st_area)*100
  species<-dimnames(sub_dat)[[2]][4:256]
  year<-rep(scenario[i],253)
  presence_suitable<-apply(sp_dat,2,FUN=max)
  state<-rep(st[j],253)
  all_state[[j]]<-cbind(year,species,state,percent_suitable,presence_suitable)
}
all_scen[[i]]<-do.call("rbind",all_state)
}

final_dat<-do.call("rbind",all_scen)

write.csv(final_dat,paste0(write.dir,"state_percent_suitable_habitat.csv"),row.names=FALSE)

}


############### for Australia
{
all_scen<-list()

for(i in 1:length(scen)){
  
  dat<-read.csv(paste(scen.dir,scenario[i],"_Presence_absence_lat_long_all_apecies_25_11_2013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find areas
  b<-area(r1)
  dat$grid_size<-extract(b,locs2)
  st_area<-sum(dat$grid_size)
  sp_dat<-dat[,4:256]
  #calculate percent suitable habitat
  percent_suitable<-sp_dat*dat$grid_size
  percent_suitable<-(colSums(percent_suitable)/st_area)*100
  species<-dimnames(dat)[[2]][4:256]
  year<-rep(scenario[i],253)
  presence_suitable<-apply(sp_dat,2,FUN=max)
  Nation<-rep("Australia",253)
  all_state<-cbind(year,species,Nation,percent_suitable,presence_suitable)
  
  all_scen[[i]]<-all_state
}

final_dat<-do.call("rbind",all_scen)

write.csv(final_dat,paste0(write.dir,"Australia_percent_suitable_habitat.csv"),row.names=FALSE)


}



############## smaller regions
  #only keep areas with suitable habitat
  #to get area
  r1<-raster("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\Acetosa sagittata\\Australia_average_rcp45_2035.asc")
  r1<-disaggregate(r1,fact=3)
  b<-area(r1)

############## for NRM

{

#keep only those species that have suitable habitat
all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
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
  dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
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


write.csv(final_all_dat,paste0(write.dir,"NRM_percent_suitable_habitat.csv"),row.names=FALSE)


}

############## for LGA
{
all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
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
   dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
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


write.csv(final_all_dat,paste0(write.dir,"LGA_percent_suitable_habitat.csv"),row.names=FALSE)


}

############## for RAMSAR
{
  all_scen<-list()
  for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
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
    dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
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
  
  
 write.csv(final_all_dat,paste0(write.dir,"ramsar_percent_suitable_habitat.csv"),row.names=FALSE)
  
}

############## for CAPAD 
{
  all_scen<-list()
  for(i in 1:length(scen)){
     dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
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
    dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
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
  
  write.csv(final_all_dat,paste0(write.dir,"CAPAD_percent_suitable_habitat.csv"),row.names=FALSE)
  
  
}

############## for CAPAD restricted 
{
  all_scen<-list()
  for(i in 1:length(scen)){
     dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
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
 dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
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
  
  
  write.csv(final_all_dat,paste0(write.dir,"CAPAD_restricted_percent_suitable_habitat.csv"),row.names=FALSE)
  
  
}

############## for IBRA7_regions restricted
{
  rm(r1)
  all_scen<-list()
  for(i in 1:length(scen)){
    
 dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"IBRA7_regions.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$REG_CODE_7)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$REG_CODE_7))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,REG_CODE_7==st[j])
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
    dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
    #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"IBRA7_regions.shp",sep=""))
    dat2<-over(locs2,aa) 
    dat2$grid_size<-extract(b,locs2)
    cols<-ncol(dat2)+4
    cols_short<-ncol(dat2)-1
    dat3<-cbind(dat2,dat)
    dat3$remove<-is.na(dat3$REG_CODE_7)
    dat3<-subset(dat3,remove==FALSE)
    dat3<-dat3[,1:ncol(dat3)-1]
    
    # for each region
    st<-as.vector(unique(dat3$REG_CODE_7))
    all_state<-list()
    for (j in 1:length(st)){
      sub_dat<-subset(dat3,REG_CODE_7==st[j])
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
  
  
  write.csv(final_all_dat,paste0(write.dir,"IBRA7_percent_suitable_habitat.csv"),row.names=FALSE)
  
  
}

############## for ecoregions
{
#   
#   all_scen<-list()
#   for(i in 1:length(scen)){
#     
#      dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_1-130_factor_3_26-11-2013.csv",sep=""))
#     locs2<-SpatialPoints(cbind(dat$x,dat$y))
#     #find out which states point occurs in 
#     aa<-readShapePoly("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\Bioregions.shp")
#     dat2<-over(locs2,aa) 
#     dat2$grid_size<-extract(b,locs2)
#     cols<-ncol(dat2)+4
#     cols_short<-ncol(dat2)-1
#     dat3<-cbind(dat2,dat)
#     dat3$remove<-is.na(dat3$ECO_NAME)
#     dat3<-subset(dat3,remove==FALSE)
#     dat3<-dat3[,1:ncol(dat3)-1]
#     
#     # for each region
#     st<-as.vector(unique(dat3$ECO_NAME))
#     all_state<-list()
#     for (j in 1:length(st)){
#       sub_dat<-subset(dat3,ECO_NAME==st[j])
#       #nrow subdat <3 then continue to the next one
#       if (nrow(sub_dat)<3) next
#       
#       
#       st_area<-sum(sub_dat$grid_size)
#       sp_dat<-sub_dat[,cols:ncol(sub_dat)]
#       percent_suitable<-sp_dat*sub_dat$grid_size
#       percent_suitable<-(colSums(percent_suitable)/st_area)*100
#       reg_dat<-sub_dat[1,1:cols_short]
#       species<-dimnames(sp_dat)[[2]]
#       year<-rep(scenario[i],ncol(sp_dat))
#       presence_suitable<-apply(sp_dat,2,FUN=max)
#       state<-rep(st[j],ncol(sp_dat))
#       #keep data for only species with a presence
#       dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
#       all_state[[j]]<-dat4
#     }
#     all_scen[[i]]<-do.call("rbind",all_state)
#   }
#   final_dat_1_150<-do.call("rbind",all_scen)
#   
#   
#   all_scen<-list()
#   for(i in 1:length(scen)){
#      dat<-read.csv(paste(out.dir,"aa_all_species\\scenario_Pres_abs\\",scen2[i],"_Presence_absence_lat_long_species_131-253_factor_3_26-11-2013.csv",sep=""))
#     locs2<-SpatialPoints(cbind(dat$x,dat$y))
#     #find out which states point occurs in 
#     dat2<-over(locs2,aa) 
#     dat2$grid_size<-extract(b,locs2)
#     cols<-ncol(dat2)+4
#     cols_short<-ncol(dat2)-1
#     dat3<-cbind(dat2,dat)
#     dat3$remove<-is.na(dat3$ECO_NAME)
#     dat3<-subset(dat3,remove==FALSE)
#     dat3<-dat3[,1:ncol(dat3)-1]
#     
#     # for each region
#     st<-as.vector(unique(dat3$ECO_NAME))
#     all_state<-list()
#     for (j in 1:length(st)){
#       sub_dat<-subset(dat3,ECO_NAME==st[j])
#       if (nrow(sub_dat)<3) next
#       
#       
#       st_area<-sum(sub_dat$grid_size)
#       sp_dat<-sub_dat[,cols:ncol(sub_dat)]
#       percent_suitable<-sp_dat*sub_dat$grid_size
#       percent_suitable<-(colSums(percent_suitable)/st_area)*100
#       reg_dat<-sub_dat[1,1:cols_short]
#       species<-dimnames(sp_dat)[[2]]
#       year<-rep(scenario[i],ncol(sp_dat))
#       presence_suitable<-apply(sp_dat,2,FUN=max)
#       state<-rep(st[j],ncol(sp_dat))
#       #keep data for only species with a presence
#       dat4<-subset(cbind(reg_dat,year,species,state,percent_suitable,presence_suitable),presence_suitable==1)
#       all_state[[j]]<-dat4
#     }
#     all_scen[[i]]<-do.call("rbind",all_state)
#   }
#   final_dat_151_292<-do.call("rbind",all_scen)
#   
#   
#   final_all_dat<-rbind(final_dat_1_150,final_dat_151_292)
#   
#   
#   write.csv(final_all_dat,"F:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\ECOREGION_percent_suitable_habitat.csv",row.names=FALSE)
#   
#   
}

############## for East and West
 {
#   all_scen<-list()
#   
# for(i in 1:length(scen2)){
# 
# dat<-read.csv(paste("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\all species\\ ",scen2[i]," _Presence_absence_lat_long_all_apecies_19_08_3013.csv",sep=""))
# locs2<-SpatialPoints(cbind(dat$x,dat$y))
# 
# #find out which states point occurs in 
# aa<-readShapePoly("C:\\Daisy\\Current_Projects\\exotic_plants\\paper\\manuscripts\\hotspots\\ARCGIS\\east_west_hotspots.shp")
# dat$states<-over(locs2,aa)[,"location"]
# b<-area(r1)
# dat$grid_size<-extract(b,locs2)
# 
# # for each state
# st<-as.vector(unique(dat$states))[2:3]
# 
# all_state<-list()
# 
# for (j in 1:length(st)){
#    sub_dat<-subset(dat,states==st[j])
#   st_area<-sum(sub_dat$grid_size)
#   sp_dat<-sub_dat[,4:295]
#   percent_suitable<-sp_dat*sub_dat$grid_size
#   percent_suitable<-(colSums(percent_suitable)/st_area)*100
#   species<-dimnames(sub_dat)[[2]][4:295]
#   year<-rep(scen2[i],292)
#   presence_suitable<-apply(sp_dat,2,FUN=max)
#   state<-rep(st[j],292)
#   all_state[[j]]<-cbind(year,species,state,percent_suitable,presence_suitable)
# }
# all_scen[[i]]<-do.call("rbind",all_state)
# }
# 
# final_dat<-do.call("rbind",all_scen)
# 
# write.csv(final_dat,"c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\east_west_percent_suitable_habitat2.csv",row.names=FALSE)
# 
# }
# 
# }
#   
}














#fix names in files


dat_files<-list.files(write.dir,full.names=TRUE)

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









