
#make sure workspace is clean
rm(list = ls())
species<-"Vinca minor"

library(raster)
#get directories where data located
work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")

species_files<-list.files(out.dir,pattern="final2")
c_files<-c("Australia_average_rcp45_2035.asc","Australia_species_current_avg.asc","Australia_average_rcp45_2065.asc","Australia_average_rcp85_2035.asc","Australia_average_rcp85_2065.asc")



aus_extent<-extent(112.9167,153.5833,-44,-9.333333)
#mask of Australia
aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)

for(i in 1:length(species_files))  {
  #get species name
  spec<-strsplit(paste(species_files[i]),"_")[[1]][1]
  dat1<-read.csv(paste(out.dir,species_files[i],"//maxentResults.csv",sep=""),row.names=1)
  thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
  if(is.na(thresh5)) stop(paste0("Check MaxentResults for ",spec))
  
  #get rasters and apply thresholds
  for(ii in 1:length(c_files)){
   out.name<-strsplit(c_files[ii],"\\.")[[1]][1]
  ras4<-paste0(out.dir,species_files[i],"\\",c_files[ii])
    aus_r4<-crop(raster(ras4),aus_extent)
    fun <- function(x) { x[x<thresh5] <- 0; return(x) }
    rc2 <- calc(aus_r4, fun)
writeRaster(rc2,paste(out.dir,species_files[i],"\\",out.name,"_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
    rc3 <- calc(rc2, fun)
    #fun <- function(x) { x[x<1] <- NA; return(x) }
    #suitable <- calc(rc3, fun)
    writeRaster(rc3,paste(out.dir,species_files[i],"\\",out.name,"_thresholded_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    
    
  }
    
 message(i)
  species<-"Vinva minor"
}