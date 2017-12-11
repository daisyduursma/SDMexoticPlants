
#using only the current distributions determine species where majority of distribution is above tropic of capricorn

#make sure workspace is clean
rm(list = ls())

#libs
library(raster)

outdir="D:\\Current_Projects\\exotic_plants\\outputs\\"

#time steps

#year<-c("thresholded_current_10.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")

  
  files<-list.files(path=paste(outdir),pattern="thresholded_current_10.asc",recursive=TRUE,full.names=TRUE) 
  
  
  
  #read in first ascii    
  r<-raster(files[1])
n<-extent(112.9167,153.5833,-23.6,-9.333333)
s<-extent(112.9167,153.5833,-43.58333 ,-23.6)
r_n<-area(crop(r,n))
r_s<-area(crop(r,s))

sp_areas<-list()
  #start loop for rest of asciis
  for(j in 1:length(files)){
    #check number of files
    iter <- length(files)
    if(iter != 292) stop("wrong number of files")
    
    a<-raster(files[j])
    b<-a*r_n
    c<-a*r_s
    area_north<-cellStats(b,stat=sum)
    area_south<-cellStats(c,stat=sum)
    
    sp_areas[[j]]<-cbind(files[j],area_north,area_south)
    message(j)
    }
sp_area_all<-do.call("rbind",sp_areas)

write.csv(sp_area_all,"D:\\Current_Projects\\exotic_plants\\paper\\manuscripts\\hotspots\\tropica_nontropica_area_by_species.csv")