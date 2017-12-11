
#make sure workspace is clean
rm(list = ls())
#load library 


#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


library(raster)
#get directories where data located
work.dir<-"H:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")


# sp_a<-list.files(out.dir,pattern="final2")
# sp_b<-list.files(out.dir,pattern="_current_suitable_habitat.jpg",recursive=TRUE)
# sp_c<-strsplit(sp_b,"/")
# 
# 
# # keep only first:
# sp_c<-sapply(sp_c, "[", 1)  # shortest version
# #sapply(sp_c, function(x)x[1])  # same as this!!

# species_files<-setdiff(sp_a,sp_c)
species_files<-list.files(out.dir,pattern="final2")


#sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

#make list of species
# species <-as.vector(unique(sp_dat$species))
# species<-species[c(27,38,40,45,60,75,101,125,144,156)]
# rm(sp_dat)

aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
# aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-raster("H:\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)

ascols <- colorRampPalette(c("darkgray","yellow","orange","darkred"),
                           interpolate="linear")

a <- list()

# Sys.sleep(18000)

  for(i in 1:length(species_files))  {
    
    
    
    #get species name
    
    spec<-strsplit(paste(species_files[i]),"_")[[1]][1]
    
#     rcp45_2035<-list.files(paste(out.dir,species_files[i],sep=""),pattern="RCP45.+2035_avg",full.name=TRUE)
#     s1<-stack(raster(rcp45_2035[1]),raster(rcp45_2035[2]),raster(rcp45_2035[3]),raster(rcp45_2035[4]),raster(rcp45_2035[5]),raster(rcp45_2035[6]),raster(rcp45_2035[7]))
#     aus_r4<-crop(s1,aus_extent)
#     aus_r4<-mask(aus_r4,aus_mask)
#     sum_rcp<- calc(aus_r4, sum)/7
#     #read in maxent output file
 dat1<-read.csv(paste(out.dir,species_files[i],"//maxentResults.csv",sep=""),row.names=1)
#     #   #get threshold
thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#     #reclassify 
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     
#     
#     jpeg(filename = paste(out.dir,species_files[i],"\\",spec,"_rcp45_2035_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#     #map with of cumulative_5
#     plot(rc2,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(7),main=NULL)
#     dev.off()
#     
#   
#     #write out average raster
#     writeRaster(rc2,paste(out.dir,species_files[i],"\\","average_rcp45_2035_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     #apply threshold so it is 0 and 1 layer
#     fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
#     rc3 <- calc(rc2, fun)
#     writeRaster(rc3,paste(out.dir,species_files[i],"\\","thresholded_rcp45_2035_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     
#     #find area that is suitable
#     fun <- function(x) { x[x<1] <- NA; return(x) }
#     suitable <- calc(rc3, fun)
#     tmp<-area(suitable,na.rm=TRUE)
#     area_km2_45_2035<-cellStats(tmp,sum)
#     #remove the files
#     rm(rc2,rc3,sum_rcp,s1,tmp,suitable)
    
################################    
#     rcp85_2035<-list.files(paste(out.dir,species_files[i],sep=""),pattern="RCP85.+2035_avg",full.name=TRUE)
#     s1<-stack(raster(rcp85_2035[1]),raster(rcp85_2035[2]),raster(rcp85_2035[3]),raster(rcp85_2035[4]),raster(rcp85_2035[5]),raster(rcp85_2035[6]),raster(rcp85_2035[7]))
#     aus_r4<-crop(s1,aus_extent)
#     aus_r4<-mask(aus_r4,aus_mask)
#     sum_rcp<- calc(aus_r4, sum)/7
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     writeRaster(rc2,paste(out.dir,species_files[i],"\\","average_rcp85_2035_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     jpeg(filename = paste(out.dir,species_files[i],"\\",spec,"_rcp85_2035_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#     #map with of cumulative_5
#     plot(rc2,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(7),main=NULL)
#     dev.off()
#     fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
#     rc3 <- calc(rc2, fun)
#     fun <- function(x) { x[x<1] <- NA; return(x) }
#     suitable <- calc(rc3, fun)
#     tmp<-area(suitable,na.rm=TRUE)
#     area_km2_85_2035<-cellStats(tmp,sum)
#     writeRaster(rc3,paste(out.dir,species_files[i],"\\","thresholded_rcp85_2035_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     rm(rc2,rc3,sum_rcp,s1,tmp,suitable)
#     
########################################   
#     rcp45_2065<-list.files(paste(out.dir,species_files[i],sep=""),pattern="RCP45.+2065_avg",full.name=TRUE)
#     s1<-stack(raster(rcp45_2065[1]),raster(rcp45_2065[2]),raster(rcp45_2065[3]),raster(rcp45_2065[4]),raster(rcp45_2065[5]),raster(rcp45_2065[6]),raster(rcp45_2065[7]))
#     aus_r4<-crop(s1,aus_extent)
#     aus_r4<-mask(aus_r4,aus_mask)
#     sum_rcp<- calc(aus_r4, sum)/7
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     writeRaster(rc2,paste(out.dir,species_files[i],"\\","average_rcp45_2065_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     jpeg(filename = paste(out.dir,species_files[i],"\\",spec,"_rcp45_2065_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#     #map with of cumulative_5
#     plot(rc2,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(7),main=NULL)
#     dev.off()
#     
#     fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
#     rc3 <- calc(rc2, fun)
#     fun <- function(x) { x[x<1] <- NA; return(x) }
#     suitable <- calc(rc3, fun)
#     tmp<-area(suitable,na.rm=TRUE)
#     area_km2_45_2065<-cellStats(tmp,sum)
#     writeRaster(rc3,paste(out.dir,species_files[i],"\\","thresholded_rcp45_2065_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     rm(rc2,rc3,sum_rcp,s1,tmp,suitable)
#     
#     ##
#     
# #####################################    
#     rcp85_2065<-list.files(paste(out.dir,species_files[i],sep=""),pattern="RCP85.+2065_avg",full.name=TRUE)
#     s1<-stack(raster(rcp85_2065[1]),raster(rcp85_2065[2]),raster(rcp85_2065[3]),raster(rcp85_2065[4]),raster(rcp85_2065[5]),raster(rcp85_2065[6]),raster(rcp85_2065[7]))
#     aus_r4<-crop(s1,aus_extent)
#     aus_r4<-mask(aus_r4,aus_mask)
#     sum_rcp<- calc(aus_r4, sum)/7
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     writeRaster(rc2,paste(out.dir,species_files[i],"\\","average_rcp85_2065_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     jpeg(filename = paste(out.dir,species_files[i],"\\",spec,"_rcp85_2065_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#     #map with of cumulative_5
#     plot(rc2,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(7),main=NULL)
#     dev.off()
#     
#     fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
#     rc3 <- calc(rc2, fun)
#     fun <- function(x) { x[x<1] <- NA; return(x) }
#     suitable <- calc(rc3, fun)
#     tmp<-area(suitable,na.rm=TRUE)
#     area_km2_85_2065<-cellStats(tmp,sum)
#     writeRaster(rc3,paste(out.dir,species_files[i],"\\","thresholded_rcp85_2065_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     #  ascols = colorRampPalette(c("darkgray","yellow","orange","darkred"),interpolate="linear")
#     #   image(rc2,frame.plot=FALSE,axes = FALSE,col=ascols(1000),main="rcp4.5_2035")
#     #     
#     rm(rc2,rc3,sum_rcp,s1,tmp,suitable)
#     
#    
#     #map of current
#     jpeg(filename = paste(out.dir,species_files[i],"\\",spec,"_current_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
    ras4<-paste(out.dir,species_files[i],"\\species_current_avg.asc",sep="")
    aus_r4<-crop(raster(ras4),aus_extent)
    aus_r4<-mask(aus_r4,aus_mask)
    fun <- function(x) { x[x<thresh5] <- 0; return(x) }
    rc2 <- calc(aus_r4, fun)
#     plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(7),main=NULL)
#     dev.off()
writeRaster(rc2,paste(out.dir,species_files[i],"\\","current_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
    rc3 <- calc(rc2, fun)
    fun <- function(x) { x[x<1] <- NA; return(x) }
    suitable <- calc(rc3, fun)
    writeRaster(rc3,paste(out.dir,species_files[i],"\\","thresholded_current_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
#     tmp<-area(suitable,na.rm=TRUE)
#     area_km2_current<-cellStats(tmp,sum)  
#                 
      
#       a<-list(species=spec,
#                    area_km2_current=area_km2_current,
#                    area_km2_45_2035=area_km2_45_2035,
#                    area_km2_85_2035=area_km2_85_2035,
#                    area_km2_45_2065=area_km2_45_2065,
#                    area_km2_85_2065=area_km2_85_2065)
#       fn <- paste0("Results",i,".Rdata")
#       fn <- paste0(out.dir,fn)
#     save(a, file=fn)
#     
#     
    
#     a<-list(species=spec,
#             
#                          area_km2_85_2035=area_km2_85_2035)
#            fn <- paste0("Results",i,".Rdata")
#                fn <- paste0(out.dir,fn)
#            save(a, file=fn)
    message(i)
    
  }
  
  
#species  adf <- do.call(rbind,a)
  
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
#   mfile<-paste(out.dir,species[i],"_final2",sep="")
#   #get file with maxent results(
#   dat1<-read.csv(paste(mfile,"//maxentResults.csv",sep=""),row.names=1)
#   #get threshold
#   #thresh5<-dat1["species (average)","Fixed.cumulative.value.5.logistic.threshold"]
#   #get ascii files
#   afiles<-list.files(mfile,pattern="avg.asc" + RCP85",full.names=TRUE)
# 
#   for(ii in 1:length(afiles)){
# 
#     #read in ascii as raster
#     sp_avg<-raster(paste(afiles[ii]))
#     #apply threshold
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sp_avg, fun)
#     fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
#     rc3 <- calc(rc2, fun)
# 
#     #get file name
# sp<-strsplit(strsplit(afiles[ii],"\\\\")[[1]][6],"_")[[1]][1]
# 
# pre_rcp<-strsplit(strsplit(strsplit(afiles[ii],"/")[[1]][2],"_avg.asc")[[1]][1],"species_")[[1]][2]
# 
# 
# writeRaster(rc3,filename=paste(mfile,"/",sp,"_",pre_rcp,"_threshold_cumulative_5.asc",sep=""),overwrite=TRUE,NAflag=-9999)
# 
# 
# 
# 
# message(i)
# }
#   
# }
# 
