


#make sure workspace is clean
rm(list = ls())
# 
# #make list of all the ascii files wanted
# time_rcp<-c("thresholded_current_10","thresholded_rcp45_2035_10","thresholded_rcp45_2065_10","thresholded_rcp85_2035_10","thresholded_rcp85_2065_10")



# for (i in 1:length(grids)){
#   message("Creating table for ",grids[i])
#   try(makePolygonTable(i,
                 out.dir<-"F:\\Current_Projects\\exotic_plants\\outputs\\all species\\"
#,
#                       
#   ))
#   
# }
# 


sp_ascii<-list.files("f:/Current_Projects/exotic_plants/outputs",
             pattern="(thresholded_current_10|thresholded_rcp45_2035_10|thresholded_rcp45_2065_10|thresholded_rcp85_2035_10|thresholded_rcp85_2065_10)"                     ,recursive=TRUE,full.names=TRUE)


#read in first raster and get lat and longs for all land cells
aa<-raster(sp_ascii[1])
locs<-rasterToPoints(aa,spatial=TRUE)
#normal data.frame of locs
locations<-as.data.frame(locs)[,c("x","y")]


# 
# #function that needs to be read in
# makePresenceTable<- function (i, out.dir=out.dir,locations=locs2){
  
  r <- require(raster)
  if(!r)stop("Install raster")
  r <- require(sp)
  if(!r)stop("Install sp")
#   r <- require(maptools)
#   if(!r)stop("Install maptools")
#   
 
  
  for (ii in 1:length(sp_ascii)){
    
    rr<-raster(sp_ascii[ii])
    dat1<-as.data.frame(extract(rr,locs))
    sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
    run<-strsplit(strsplit(sp_ascii[ii],"thresholded_")[[1]][2],"_10")[[1]][1]
    
    colnames(dat1)<-paste(sp_name,run,sep=" ")
    locations<-cbind(locations,dat1)
    
    message(ii)
     
    
  }
  
  
  #write output table
  write.csv(locations,paste(out.dir,"Presence_absence_lat_long_all_apecies.csv"))
  
  
# }

# makeAsciiSUM <- function(i, outdir=outdir){
#   
#   r <- require(raster)
#   if(!r)stop("Install raster")
#   
#   #make list of ascii files for timestep
#   rcp_asc<- list.files(outdir, pattern=paste("thresholded_",rcp_year[i],"_10.asc",sep=""), 
#                        full.name=TRUE, recursive=TRUE)
#   #read in first ascii    
#   a<-raster(rcp_asc[1])
#   #start loop for rest of asciis
#   for(j in 2:length(rcp_asc)){
#     a<-raster(rcp_asc[j])+a
#   }
#   #write raster
#   writeRaster(a,paste(outdir,"all species\\",rcp_year[i],"_sum_all_species.asc",sep=""),
#               NAflag=-9999,overwrite=TRUE)
#   
#   plot(a)
# }
# 
# 
# 
# 
# #get lists of asciis  
# rcp_year<-  c( "rcp45_2035",
#                "rcp45_2065",
#                "rcp85_2035",
#                "rcp85_2065",
#                "current")
# 
# 
# for(i in 1:length(rcp_year)){
#   message("summing rcp and year ",rcp_year[i])
#   try(makeAsciiSUM(i, 
#                    outdir="h:\\Current_Projects\\exotic_plants\\outputs\\"
#   ))
#   
# }



# 
# #load library 
# library(dismo)
# library(sp)
# library(maptools)
# library(raster)

# 
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
# a<-over(locs,CAPAD)
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
