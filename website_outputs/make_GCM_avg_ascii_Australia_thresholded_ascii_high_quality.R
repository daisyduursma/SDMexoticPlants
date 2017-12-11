#make sure workspace is clean
rm(list = ls())
#load library 
library(raster)

#get directories where data located
work.dir<-"D:\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste0(work.dir,"outputs\\")

#get list of species based on folders
species <-as.vector(list.files("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs"))[2:254]

#extent and mask of Australia
aus_extent<-extent(112.9167,153.5833,-44,-9.333333)
#aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
#aus_mask<-crop(aus_mask,aus_extent)



#function to make Australian rasters
makeAUSascii<- function(sum_rcp,aus_extent,new.outdir="",f_name){
    fun <- function(x) { x[x<.5] <- 0; return(x) }
    aus2 <- calc(sum_rcp, fun)
    fun <- function(x) { x[x>=.5] <- 1; return(x) }
    aus3 <- calc(aus2, fun)
    writeRaster(aus3,paste0(new.outdir,f_name,"_thresholded_high_quality.asc",sep=""),NAflag=-9999,overwrite=TRUE)
                      
}



#for each species process current and 4 future time periods
for(i in 1:253) {
  
  new.outdir<-paste0(out.dir,species[i],"\\")
   
########### rcp45_2035   
    f_name<-"Australia_average_rcp45_2035"
       sum_rcp<-raster(paste0(new.outdir,f_name,".asc"))
   makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name)
    rm(sum_rcp,f_name)
  
 ########### rcp85_2035
  f_name<-"Australia_average_rcp85_2035"
     sum_rcp<-raster(paste0(new.outdir,f_name,".asc"))
   makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name)
    rm(sum_rcp,f_name)
   
############# rcp45_2065
  f_name<-"Australia_average_rcp45_2065"
     sum_rcp<-raster(paste0(new.outdir,f_name,".asc"))
   makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name)
    rm(sum_rcp,f_name)
  
############# rcp85_2065
    f_name<-"Australia_average_rcp85_2065"
     sum_rcp<-raster(paste0(new.outdir,f_name,".asc"))
   makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name)
    rm(sum_rcp,f_name)
  
############### current
  f_name<-"Australia_current"
  sum_rcp<-raster(paste0(out.dir,species[i],"\\",f_name,".asc"))
    makeAUSascii(sum_rcp,aus_extent,new.outdir=new.outdir,f_name)
    rm(sum_rcp,f_name)
    
    message(i)
    
  }
  
  

