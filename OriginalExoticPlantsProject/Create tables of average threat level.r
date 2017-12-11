


#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
library(maptools)
library(raster)
#get directories where data located
  grid.dir<-"H:\\Current_Projects\\exotic_plants\\data\\GRIDS\\"
  out.dir<-"H:\\Current_Projects\\exotic_plants\\outputs\\"

#map of ibra areas
  ibra7<-readShapePoly(paste(grid.dir,"data_WGS_1984\\IBRA7_regions.shp",sep=""))

#map of LGA
  LGA<-readShapePoly(paste(grid.dir,"LGA\\LGA11aAust.shp",sep=""))


NRM<-readShapePoly(paste(grid.dir,"NRM_Regions_2010\\NRM_Regions_2010.shp",sep=""))



#list of all summed species files     
  sp_maps<-list.files(paste(out.dir,"all species\\",sep=""),pattern="rescaled",full.names=TRUE)
  sp_names<-list.files(paste(out.dir,"all species\\",sep=""),pattern="rescaled")
# 
# #reclassify maps so 0 to 100
# 
# for (i in 1:length(sp_maps)){
#   
#   r1<-raster(sp_maps[i])*100/292
#   
#   name1<-strsplit(paste(sp_maps[i]),".asc")[[1]][1]
#   writeRaster(r1,paste(name1,"_rescaled.asc",sep=""))
#   
#   
# }
  
  sp1<-stack(raster(sp_maps[1]),raster(sp_maps[2]),raster(sp_maps[3]),raster(sp_maps[4]),raster(sp_maps[5]))



#extract data for raster stack

aa<-extract(sp1,ibra7,fun=mean,na.rm=TRUE)
bb<-extract(sp1,ibra7,fun=sd,na.rm=TRUE)

cc<-extract(sp1,LGA,fun=mean,na.rm=TRUE)
dd<-extract(sp1,LGA,fun=sd,na.rm=TRUE)

jj<-extract(sp1,NRM,fun=mean,na.rm=TRUE)
kk<-extract(sp1,NRM,fun=sd,na.rm=TRUE)


ee<-strsplit(sp_maps,"all species\\\\")
n_col<-list()
for (i in 1:length(ee)){
  n_col[[i]]<-strsplit(paste(ee[[i]][2]),"_sum_all_species_rescaled.asc")
}
names_col<-as.vector(do.call("rbind",n_col))
names_col<-as.vector(do.call("rbind",names_col))


ibra7_ID<-as.data.frame (ibra7@data$REG_NAME_7)
#colnames(ibra7_ID)<-"IBRA7_ID"


LGA_ID<-as.data.frame (LGA@data$LGA_NAME11)
STATE_CODE<-as.data.frame (LGA@data$STATE_CODE)

NRM_BODY<-as.data.frame (NRM@data$NRM_BODY)
STATE<-as.data.frame (NRM@data$STATE)
NRM_Region<-as.data.frame (NRM@data$NRM_REGION)




colnames(aa)<-names_col
aa<-cbind(ibra7_ID,aa)
colnames(bb)<-names_col
bb<-cbind(ibra7_ID,bb)
colnames(cc)<-names_col
cc<-cbind(LGA_ID,STATE_CODE,cc)
colnames(dd)<-names_col
dd<-cbind(LGA_ID,STATE_CODE,dd)

colnames(jj)<-names_col
jj<-cbind(NRM_Region,STATE,NRM_BODY,jj)

colnames(kk)<-names_col
kk<-cbind(NRM_Region,STATE,NRM_BODY,kk)






write.csv(aa,paste(out.dir,"all species\\IBRA7_average_threat_level.csv",sep=""))
write.csv(bb,paste(out.dir,"all species\\IBRA7_average_threat_level_sd.csv",sep=""))
write.csv(cc,paste(out.dir,"all species\\LGA_average_threat_level.csv",sep=""))
write.csv(dd,paste(out.dir,"all species\\LGA_average_threat_level_sd.csv",sep=""))

write.csv(jj,paste(out.dir,"all species\\NRM_average_threat_level.csv",sep=""))
write.csv(kk,paste(out.dir,"all species\\NRM_average_threat_level_sd.csv",sep=""))



















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
