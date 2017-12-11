
#make sure workspace is clean
  rm(list = ls())

#libs
  library(raster)
  
  outdir="D:\\Current_Projects\\exotic_plants\\outputs\\"
  
#time steps
  
year<-c("thresholded_current_10.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")
   for (i in 1:length(year)){
    
  files<-list.files(path=paste(outdir),pattern=year[i],recursive=TRUE,full.names=TRUE) 
  
  
    
  #read in first ascii    
  a<-raster(files[1])
  #start loop for rest of asciis
  for(j in 2:length(files)){
    #check number of files
    iter <- length(files)
    if(iter != 292) stop("wrong number of files")

    a<-raster(files[j])+a
    message(paste(year[i], "and file ",j))
  }
  #write raster
  writeRaster(a,paste0(outdir,"all species\\","AUS_",year[i],"_sum_all_species.asc"),NAflag=-9999,overwrite=TRUE)
  
}
  
    
  