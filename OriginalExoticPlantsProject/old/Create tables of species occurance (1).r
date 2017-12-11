


#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
library(maptools)
library(raster)
#get directories where data located
  grid.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\"
  out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"

#map of ibra areas
  ibra7<-readShapePoly(paste(grid.dir,"data_WGS_1984\\IBRA7_regions.shp",sep=""))

#list of all species files     
  sp_maps<-list.files(paste(out.dir,"Threshold Maps\\sub_Eur_USA_11_10_2012\\",sep=""),pattern="cumulative_5",full.names=TRUE)
  
  sp1<-stack(paste(sp_maps[1:10],sep=","))


#extract data for raster stack

            #aa<-extract(sp1,ibra7,fun=max
            #aa<-extract(sp1,ibra7,fun=function(x,...)sum(x,na.omit(x))/length(x))
            aa<-extract(sp1,ibra7)
# 
# shap_max<-fun=function(aa,...)max(aa,na.omit(aa))
# 
# shap_max<-fun=function(aa,...)max(aa,na.omit(aa))
# 

#fun <-  )


area_max<-lapply(aa, function(x)apply(x,2,max,na.rm=TRUE))
area_max<-do.call("rbind",area_max)

bb<-dimnames(area_max)[[2]]
cc<-strsplit(bb,"_")
n_col<-list()
for (i in 1:length(cc)){
  n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
}
names_col<-as.vector(do.call("rbind",n_col))
ibra7_ID<-as.data.frame (ibra7@data$REG_NAME_7)
colnames(ibra7_ID)<-"IBRA7_ID"

colnames(area_max)<-names_col
area_max[is.infinite(area_max)] <- NA 
area_max<-as.data.frame(cbind(ibra7_ID,area_max))


write.csv(area_max,paste(out.dir,"website\\IBRA7_species_presence.csv",sep=""))




prop<-lapply(aa, function(x)apply(x,2,function(y)length(y[y==1])/length(y)))

prop<-do.call("rbind",prop)

bb<-dimnames(prop)[[2]]
cc<-strsplit(bb,"_")
n_col<-list()
for (i in 1:length(cc)){
  n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
}
names_col<-as.vector(do.call("rbind",n_col))
ibra7_ID<-as.data.frame (ibra7@data$REG_NAME_7)
colnames(ibra7_ID)<-"IBRA7_ID"

colnames(prop)<-names_col
prop[is.infinite(prop)] <- NA 
prop<-as.data.frame(cbind(ibra7_ID,prop))


write.csv(prop,paste(out.dir,"website\\IBRA7_species_proportion.csv",sep=""))














# 
# #                 lapply(aa, function(x)apply(x,2,max,na.omit=TRUE))
# 
# #                  sum(x,na.omit(x))/length(x))
#                 
# #                 bb<-na.omit(aa)
# #                 
# # #get correct column names
#   bb<-dimnames(aa)[[2]]
#   cc<-strsplit(bb,"_")
#   n_col<-list()
#   for (i in 1:length(cc)){
#         n_col[[i]]<-paste(cc[[i]][1],cc[[i]][2],sep=" ")
#     }
#   names_col<-as.vector(do.call("rbind",n_col))
#   ibra7_ID<-as.matrix (ibra7@data$REG_NAME_7)
#   colnames(ibra7_ID)<-"IBRA7_ID"
#    
# #assign the column names
# 
# 
# write.csv(aa,"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\IBRA7_species.csv")
# 

  
