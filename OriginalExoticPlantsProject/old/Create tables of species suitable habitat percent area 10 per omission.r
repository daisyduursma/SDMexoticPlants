


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

#if you do not already have a table of species observations do the following
 {
  
#   r1<-raster("f:/Current_Projects/exotic_plants/outputs/Acanthocereus tetragonus_final2/thresholded_current_10.asc")
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







































# 

# #get directories where data located
# 
# 
# #read in all polygon file
# aa<-readShapePoly(paste(grid.dir,grids[i]".shp",sep=""))
# LGA<-readShapePoly(paste(grid.dir,"LGA.shp",sep=""))
# NRM<-readShapePoly(paste(grid.dir,"NRM.shp",sep=""))
# CAPAD<-readShapePoly(paste(grid.dir,"CAPAD.shp",sep=""))
# CAPAD_restricted<-readShapePoly(paste(grid.dir,"CAPAD_restrictedt.shp",sep=""))
# ramsar<-readShapePoly(paste(grid.dir,"ramsar.shp",sep=""))
# 
# 
# 
 a<-over(locations,aa)
# 
# 
# #list of all species files     
#   sp_maps<-list.files(paste(out.dir,"Threshold Maps\\sub_Eur_USA_11_10_2012\\",sep=""),pattern="cumulative_5",full.names=TRUE)
#   
#   sp1<-stack(paste(sp_maps[1:10],sep=","))
# 
# 
# #extract data for raster stack
# 
#             #aa<-extract(sp1,ibra7,fun=max
#             #aa<-extract(sp1,ibra7,fun=function(x,...)sum(x,na.omit(x))/length(x))
#             aa<-extract(sp1,ibra7)
# # 
# # shap_max<-fun=function(aa,...)max(aa,na.omit(aa))
# # 
# # shap_max<-fun=function(aa,...)max(aa,na.omit(aa))
# # 
# 
# #fun <-  )
# 
# 
# area_max<-lapply(aa, function(x)apply(x,2,max,na.rm=TRUE))
# area_max<-do.call("rbind",area_max)
# 
# bb<-dimnames(area_max)[[2]]
# cc<-strsplit(bb,"_")
# n_col<-list()
# for (i in 1:length(cc)){
#   n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
# }
# names_col<-as.vector(do.call("rbind",n_col))
# ibra7_ID<-as.data.frame (ibra7@data$REG_NAME_7)
# colnames(ibra7_ID)<-"IBRA7_ID"
# 
# colnames(area_max)<-names_col
# area_max[is.infinite(area_max)] <- NA 
# area_max<-as.data.frame(cbind(ibra7_ID,area_max))
# 
# 
# write.csv(area_max,paste(out.dir,"website\\IBRA7_species_presence.csv",sep=""))
# 
# 
# 
# 
# prop<-lapply(aa, function(x)apply(x,2,function(y)length(y[y==1])/length(y)))
# 
# prop<-do.call("rbind",prop)
# 
# bb<-dimnames(prop)[[2]]
# cc<-strsplit(bb,"_")
# n_col<-list()
# for (i in 1:length(cc)){
#   n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
# }
# names_col<-as.vector(do.call("rbind",n_col))
# ibra7_ID<-as.data.frame (ibra7@data$REG_NAME_7)
# colnames(ibra7_ID)<-"IBRA7_ID"
# 
# colnames(prop)<-names_col
# prop[is.infinite(prop)] <- NA 
# prop<-as.data.frame(cbind(ibra7_ID,prop))
# 
# 
# write.csv(prop,paste(out.dir,"website\\IBRA7_species_proportion.csv",sep=""))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # #                 lapply(aa, function(x)apply(x,2,max,na.omit=TRUE))
# # 
# # #                  sum(x,na.omit(x))/length(x))
# #                 
# # #                 bb<-na.omit(aa)
# # #                 
# # # #get correct column names
# #   bb<-dimnames(aa)[[2]]
# #   cc<-strsplit(bb,"_")
# #   n_col<-list()
# #   for (i in 1:length(cc)){
# #         n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
# #     }
# #   names_col<-as.vector(do.call("rbind",n_col))
# #   ibra7_ID<-as.matrix (ibra7@data$REG_NAME_7)
# #   colnames(ibra7_ID)<-"IBRA7_ID"
# #    
# # #assign the column names
# # 
# # 
# # write.csv(aa,"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\IBRA7_species.csv")
# # 
# 
#   

# r1<-raster(sp_ascii[1])
# locs<-rasterToPoints(aa,spatial=TRUE)
# #normal data.frame of locs
# locations<-as.data.frame(locs)[,c("x","y")]
# locs2<-SpatialPoints(cbind(locations$x,locations$y))
# 
sp_ascii<-list.files("f:/Current_Projects/exotic_plants/outputs",
                     pattern="(thresholded_current_10|thresholded_rcp45_2035_10|thresholded_rcp45_2065_10|thresholded_rcp85_2035_10|thresholded_rcp85_2065_10)"                     ,recursive=TRUE,full.names=TRUE)
# 
# 
# for (ii in 1:length(sp_ascii)){
#   
#   rr<-raster(sp_ascii[ii])
#   dat1<-as.data.frame(extract(rr,locs))
#   sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
#   run<-strsplit(strsplit(sp_ascii[ii],"thresholded_")[[1]][2],"_10")[[1]][1]
#   colnames(dat1)<-paste(sp_name,run,sep=" ")
#   locations<-cbind(locations,dat1)
#   message(ii)
# }
# #write output table
# write.csv(locations,paste(out.dir,"Presence_absence_lat_long_all_apecies_27_04_3013.csv"))

