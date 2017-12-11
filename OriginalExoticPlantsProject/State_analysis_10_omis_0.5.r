rm(list = ls())

#read in csv files of log format and % suitable for 10% omission

aus<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australia_percent_suitable_habitat.csv")
states<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\state_percent_suitable_habitat.csv")

#variables
time<-unique(aus$year)
state_name<-unique(states$state)


###############################
#get Australian area of suitabile habitat for all time periods
  
  obs<-subset(aus,year==time[1],select=species)
  for(i in 1:length(time)){
      sub_aus<-subset(aus,year==time[i],select=c(species,percent_suitable))
    colnames(sub_aus)[2]<-paste0("Aus_",time[i],"_%area_suitable")
    obs<-merge(obs,sub_aus,by="species")
  }

################################################
#get state level data

  for(ii in 1:length(state_name)){
    sub_state<-subset(states,state==state_name[ii])
  for(j in 1:length(time)){
    sub_state_time<-subset(sub_state,year==time[j],select=c(species,percent_suitable))
    colnames(sub_state_time)[2]<-paste0(state_name[ii],"_",time[j],"_%area_suitable")
    obs<-merge(obs,sub_state_time,by="species")
  }
  }
  
##############################
#% area above 0.5



aus<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australia_percent_suitable_highly_habitat.csv")
states<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\state_percent_suitable_highly_habitat.csv")

#variables
time<-unique(aus$year)
state_name<-unique(states$state)


###############################
#get Australian area of suitabile habitat for all time periods
  

  for(i in 1:length(time)){
      sub_aus<-subset(aus,year==time[i],select=c(species,percent_suitable))
    colnames(sub_aus)[2]<-paste0("Aus_",time[i],"_%area_highly_suitable")
    obs<-merge(obs,sub_aus,by="species")
  }

################################################
#get state level data

  for(ii in 1:length(state_name)){
    sub_state<-subset(states,state==state_name[ii])
  for(j in 1:length(time)){
    sub_state_time<-subset(sub_state,year==time[j],select=c(species,percent_suitable))
    colnames(sub_state_time)[2]<-paste0(state_name[ii],"_",time[j],"_%area_highly_suitable")
    obs<-merge(obs,sub_state_time,by="species")
  }
  }



write.csv(obs,"C:\\Daisy\\Current_Projects\\exotic_plants\\Prioritsation scheme\\regional_suit.csv",row.names=FALSE)






# 
# 
# 
# 
# 
# library(raster)
# library(maptools)
# 
# out.dir<-"f:\\Current_Projects\\exotic_plants\\outputs\\all species\\"
# grid.dir<-"f:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"
# 
# r1<-raster("f:\\Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
# 
# #if you do NOT already have a table of species observations do the following and then make the long format tables
#  {
#   
#   locs<-rasterToPoints(r1,spatial=TRUE)
#   #normal data.frame of locs
#   locations<-as.data.frame(locs)[,c("x","y")]
#   locs2<-SpatialPoints(cbind(locations$x,locations$y))
#   
#   scen<-c("^current_10.asc","^average_rcp45_2035_10.asc","^average_rcp45_2065_10.asc","^average_rcp85_2035_10.asc","^average_rcp85_2065_10.asc")
#   
#   for(i in 1:length(scen)){
#   sp_ascii<-list.files("c:/daisy/Current_Projects/exotic_plants/outputs",
#                        pattern=scen[i],recursive=TRUE,full.names=TRUE)
#   
#   locations<-as.data.frame(locs)[,c("x","y")]
#   for (ii in 1:length(sp_ascii)){
#     
#     rr<-raster(sp_ascii[ii])
#      fun <- function(x) { x[x<0.5] <- 0; return(x) }
#     rr <- calc(rr, fun)
#      fun <- function(x) { x[x>=0.5] <- 1; return(x) }
#     rr <- calc(rr, fun)
#     dat1<-as.data.frame(extract(rr,locs))
#     sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
#     colnames(dat1)<-paste(sp_name,sep=" ")
#     locations<-cbind(locations,dat1)
#     message(ii)
#     }
#   #write output table
#   write.csv(locations,paste(out.dir,scen[i],"_0.5_Presence_absence_lat_long_all_apecies_27_04_3013.csv"))
#     }
  
}

#make long format tables of species presence
{
  
  
#  scen<-c("current","average_rcp45_2035","average_rcp45_2065","average_rcp85_2035","average_rcp85_2065")
#  
# scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
# 
#   all_scen<-list()
#   
# for(i in 1:length(scen)){
# 
# dat<-read.csv(paste(out.dir,scen[i],"_0.5_Presence_absence_lat_long_all_apecies_27_04_3013.csv",sep=""))
# locs2<-SpatialPoints(cbind(dat$x,dat$y))
# 
# #find out which states point occurs in 
# aa<-readShapePoly(paste(grid.dir,"NRM_Australia_states.shp",sep=""))
# dat$states<-over(locs2,aa)[,"STATE"]
# b<-area(r1)
# dat$grid_size<-extract(b,locs2)
# 
# # for each state
# st<-as.vector(unique(dat$states))
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
#   year<-rep(scenario[i],292)
#   presence_suitable<-apply(sp_dat,2,FUN=max)
#   state<-rep(st[j],292)
#   all_state[[j]]<-cbind(year,species,state,percent_suitable,presence_suitable)
# }
# all_scen[[i]]<-do.call("rbind",all_state)
# }
# 
# final_dat<-do.call("rbind",all_scen)
# 
# write.csv(final_dat,"c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\state_percent_suitable_highly_habitat.csv",row.names=FALSE)

}

}
}
############### for Australia
{
# all_scen<-list()
# 
# for(i in 1:length(scen)){
#   
#   dat<-read.csv(paste(out.dir,scen[i],"_0.5_Presence_absence_lat_long_all_apecies_27_04_3013.csv",sep=""))
#   locs2<-SpatialPoints(cbind(dat$x,dat$y))
#   #find areas
#   b<-area(r1)
#   dat$grid_size<-extract(b,locs2)
#   st_area<-sum(dat$grid_size)
#   sp_dat<-dat[,4:295]
#   #calculate percent suitable habitat
#   percent_suitable<-sp_dat*dat$grid_size
#   percent_suitable<-(colSums(percent_suitable)/st_area)*100
#   species<-dimnames(dat)[[2]][4:295]
#   year<-rep(scenario[i],292)
#   presence_suitable<-apply(sp_dat,2,FUN=max)
#   Nation<-rep("Australia",292)
#   all_state<-cbind(year,species,Nation,percent_suitable,presence_suitable)
#   
#   all_scen[[i]]<-all_state
#   message(i)
# }
# 
# final_dat<-do.call("rbind",all_scen)
# 
# write.csv(final_dat,"F:\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australia_percent_suitable_highly_habitat.csv",row.names=FALSE)
# 

}
