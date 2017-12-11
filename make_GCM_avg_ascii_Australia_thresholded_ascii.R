#make sure workspace is clean
rm(list = ls())
#load library 
library(raster)

#get directories where data located
work.dir<-"D:\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste0(work.dir,"outputs\\")

#get list of species based on folders
species <-as.vector(list.files(out.dir))

#extent and mask of Australia
aus_extent<-extent(112.9167,153.5833,-44,-9.333333)
#aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
#aus_mask<-crop(aus_mask,aus_extent)



#function to make Australian rasters
makeAUSascii<- function(sum_rcp,aus_extent,new.outdir="",f_name,thresh){

    aus<-crop(sum_rcp,aus_extent)
    writeRaster(aus,paste0(new.outdir,"Australia_",f_name,".asc",sep=""),NAflag=-9999,overwrite=TRUE)
    fun <- function(x) { x[x<thresh] <- 0; return(x) }
    aus2 <- calc(aus, fun)
writeRaster(aus2,paste0(new.outdir,"Australia_",f_name,"_10.asc"),NAflag=-9999,overwrite=TRUE)
    fun <- function(x) { x[x>=thresh] <- 1; return(x) }
    aus3 <- calc(aus2, fun)
    writeRaster(aus3,paste0(new.outdir,"Australia_",f_name,"_thresholded_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
}

#for each species process current and 4 future time periods
for(i in 254:length(species) ) {
  #make strin of output folder name
  new.outdir<-paste0(out.dir,species[i],"\\")
  #get 10% omission threshold
  dat1<-read.csv(paste0(new.outdir,"//maxentResults.csv"),row.names=1)
  thresh<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
  if(is.na(thresh)) stop(paste0("Check MaxentResults for ",spec))
  
########### rcp45_2035   
    f_name<-"average_rcp45_2035"
    rcp45_2035<-list.files(paste0(out.dir,species[i]),pattern="RCP45.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2035[1]),raster(rcp45_2035[2]),raster(rcp45_2035[3]),raster(rcp45_2035[4]),raster(rcp45_2035[5]),raster(rcp45_2035[6]),raster(rcp45_2035[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(new.outdir,f_name,".asc"),NAflag=-9999,overwrite=TRUE)
   makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name,thresh)
    rm(sum_rcp,s1,f_name)
  
 ########### rcp85_2035
  f_name<-"average_rcp85_2035"
    rcp85_2035<-list.files(paste0(out.dir,species[i]),pattern="RCP85.+2035_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2035[1]),raster(rcp85_2035[2]),raster(rcp85_2035[3]),raster(rcp85_2035[4]),raster(rcp85_2035[5]),raster(rcp85_2035[6]),raster(rcp85_2035[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(new.outdir,f_name,".asc"),NAflag=-9999,overwrite=TRUE)
    makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name,thresh)
    rm(sum_rcp,s1,f_name)
  
############# rcp45_2065
  f_name<-"average_rcp45_2065"
    rcp45_2065<-list.files(paste0(out.dir,species[i],sep=""),pattern="RCP45.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp45_2065[1]),raster(rcp45_2065[2]),raster(rcp45_2065[3]),raster(rcp45_2065[4]),raster(rcp45_2065[5]),raster(rcp45_2065[6]),raster(rcp45_2065[7]))
    sum_rcp<- calc(s1, sum)/7
     writeRaster(sum_rcp,paste0(new.outdir,f_name,".asc"),NAflag=-9999,overwrite=TRUE)
    makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name,thresh)
    rm(sum_rcp,s1,f_name)
  
############# rcp85_2065
    f_name<-"average_rcp85_2065"
    rcp85_2065<-list.files(paste0(out.dir,species[i],sep=""),pattern="RCP85.+2065_avg",full.name=TRUE)
    s1<-stack(raster(rcp85_2065[1]),raster(rcp85_2065[2]),raster(rcp85_2065[3]),raster(rcp85_2065[4]),raster(rcp85_2065[5]),raster(rcp85_2065[6]),raster(rcp85_2065[7]))
    sum_rcp<- calc(s1, sum)/7
    writeRaster(sum_rcp,paste0(new.outdir,f_name,".asc"),NAflag=-9999,overwrite=TRUE)
    makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name,thresh)
    rm(sum_rcp,s1,f_name)
  
############### current
  f_name<-"current"
  sum_rcp<-raster(paste0(out.dir,species[i],"\\species_",f_name,"_avg.asc"))
  makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name,thresh)
    rm(sum_rcp,f_name)
    
    message(i)
    
  }
  
  

