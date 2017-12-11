
#make sure workspace is clean
rm(list = ls())
#load library 


#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


library(raster)
#get directories where data located
work.dir<-"D:\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste0(work.dir,"outputs\\")
#make list of species
species <-as.vector(list.files(out.dir))
aus_extent<-extent(112.9167,153.5833,-44,-9.333333)
#mask of Australia
aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)




for(i in 1:length(species) ) {
  new.outdir<-paste0(out.dir,species[i])
 
  #get 10% omission threshold
  dat1<-read.csv(paste0(new.outdir,"//maxentResults.csv"),row.names=1)
  thresh<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
  if(is.na(thresh)) stop(paste0("Check MaxentResults for ",spec))
  
    rcp45_2035<-list.files(paste0(out.dir,species[i]),pattern="RCP45.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2035[1]),raster(rcp45_2035[2]),raster(rcp45_2035[3]),raster(rcp45_2035[4]),raster(rcp45_2035[5]),raster(rcp45_2035[6]),raster(rcp45_2035[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(out.dir,species[i],"\\average_rcp45_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    aus_r4<-crop(raster(ras4),aus_extent)
    fun <- function(x) { x[x<thresh] <- 0; return(x) }
    rc2 <- calc(aus_r4, fun)
writeRaster(rc2,paste(out.dir,species_files[i],"\\",out.name,"_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    fun <- function(x) { x[x>=thresh] <- 1; return(x) }
    rc3 <- calc(rc2, fun)
    writeRaster(rc3,paste(out.dir,species_files[i],"\\",out.name,"_thresholded_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)

  
    rm(sum_rcp,s1)
 
    rcp85_2035<-list.files(paste0(out.dir,species[i]),pattern="RCP85.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2035[1]),raster(rcp85_2035[2]),raster(rcp85_2035[3]),raster(rcp85_2035[4]),raster(rcp85_2035[5]),raster(rcp85_2035[6]),raster(rcp85_2035[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(out.dir,species[i],"\\average_rcp85_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    rm(sum_rcp,s1)

    rcp45_2065<-list.files(paste0(out.dir,species[i],sep=""),pattern="RCP45.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2065[1]),raster(rcp45_2065[2]),raster(rcp45_2065[3]),raster(rcp45_2065[4]),raster(rcp45_2065[5]),raster(rcp45_2065[6]),raster(rcp45_2065[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(out.dir,species[i],"\\average_rcp45_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    rm(sum_rcp,s1)
    
    rcp85_2065<-list.files(paste0(out.dir,species[i],sep=""),pattern="RCP85.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2065[1]),raster(rcp85_2065[2]),raster(rcp85_2065[3]),raster(rcp85_2065[4]),raster(rcp85_2065[5]),raster(rcp85_2065[6]),raster(rcp85_2065[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(out.dir,species[i],"\\average_rcp85_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    rm(sum_rcp,s1)
    
    
    message(i)
    
  }
  
  

