
#make sure workspace is clean
rm(list = ls())
#load library 


#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


library(raster)
#get directories where data located
work.dir<-"F:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")


sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

#make list of species
species <-as.vector(unique(sp_dat$species))
#species<-species[c(27,38,40,45,60,75,101,125,144,156)]
rm(sp_dat)

#aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
#aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
#aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
#aus_mask<-crop(aus_mask,aus_extent)

  
  for(i in 196:length(species) ) {
    
    rcp45_2035<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP45.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2035[1]),raster(rcp45_2035[2]),raster(rcp45_2035[3]),raster(rcp45_2035[4]),raster(rcp45_2035[5]),raster(rcp45_2035[6]),raster(rcp45_2035[7]))
   # aus_r4<-crop(s1,aus_extent)
   # aus_r4<-mask(aus_r4,aus_mask)
    sum_rcp<- calc(s1, sum)/7
    
    #dat1<-read.csv(paste(out.dir,species[i],"_final2//maxentResults.csv",sep=""),row.names=1)
    #   #get threshold
#     thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#     #writeRaster(sum_rcp,paste(out.dir,species[i],"_final2/sum_threshold_rcp45_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     
#  ascols = colorRampPalette(c("darkgray","yellow","orange","darkred"),interpolate="linear")
#   image(rc2,frame.plot=FALSE,axes = FALSE,col=ascols(1000),main="rcp4.5_2035")
# #     
#     
    writeRaster(sum_rcp,paste(out.dir,species[i],"_final2\\","average_rcp45_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
  
    rm(sum_rcp,sum_rcp,s1)
    
################################    
    rcp85_2035<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP85.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2035[1]),raster(rcp85_2035[2]),raster(rcp85_2035[3]),raster(rcp85_2035[4]),raster(rcp85_2035[5]),raster(rcp85_2035[6]),raster(rcp85_2035[7]))
    #aus_r4<-crop(s1,aus_extent)
    #aus_r4<-mask(aus_r4,aus_mask)
    sum_rcp<- calc(s1, sum)/7
#     dat1<-read.csv(paste(out.dir,species[i],"_final2//maxentResults.csv",sep=""),row.names=1)
#     thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     image(rc2,frame.plot=FALSE,axes = FALSE,col=ascols(100),main="rcp8.5_2035")
    writeRaster(sum_rcp,paste(out.dir,species[i],"_final2\\","average_rcp85_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    rm(sum_rcp,sum_rcp,s1)
    
########################################   
    rcp45_2065<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP45.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2065[1]),raster(rcp45_2065[2]),raster(rcp45_2065[3]),raster(rcp45_2065[4]),raster(rcp45_2065[5]),raster(rcp45_2065[6]),raster(rcp45_2065[7]))
    #aus_r4<-crop(s1,aus_extent)
  #  aus_r4<-mask(aus_r4,aus_mask)
    sum_rcp<- calc(s1, sum)/7
#     dat1<-read.csv(paste(out.dir,species[i],"_final2//maxentResults.csv",sep=""),row.names=1)
#     thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     image(rc2,frame.plot=FALSE,axes = FALSE,col=ascols(100),main="rcp4.5_2065")
    writeRaster(sum_rcp,paste(out.dir,species[i],"_final2\\","average_rcp45_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    rm(sum_rcp,sum_rcp,s1)
    
    ##
    
#####################################    
    rcp85_2065<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP85.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2065[1]),raster(rcp85_2065[2]),raster(rcp85_2065[3]),raster(rcp85_2065[4]),raster(rcp85_2065[5]),raster(rcp85_2065[6]),raster(rcp85_2065[7]))
    #aus_r4<-crop(s1,aus_extent)
   # aus_r4<-mask(aus_r4,aus_mask)
    sum_rcp<- calc(s1, sum)/7
#     dat1<-read.csv(paste(out.dir,species[i],"_final2//maxentResults.csv",sep=""),row.names=1)
#     thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#     fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#     rc2 <- calc(sum_rcp, fun)
#     image(rc2,frame.plot=FALSE,axes = FALSE,col=ascols(100),main="rcp8.5_2065")
     writeRaster(sum_rcp,paste(out.dir,species[i],"_final2\\","average_rcp85_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    rm(sum_rcp,sum_rcp,s1)
    
    
    message(i)
    
  }
  
  
  
  
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
