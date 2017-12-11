
#make sure workspace is clean
  rm(list = ls())

#libs
  library(raster)
  #working directory
  work.dir<-"F:\\daisy\\Current_Projects\\exotic_plants\\"
  out.dir=paste0(work.dir,"outputs\\")
  
  
  #read in csv with details of tropical species
trop_dat<- read.csv(paste0(work.dir,"paper\\manuscripts\\hotspots\\tropica_nontropica_area_by_species.csv"))
  
  species<-as.vector(subset(trop_dat, majority_tropical == 1)$Species)
  
#time steps
  
year<-c("thresholded_current_10.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")
   for (i in 1:length(year)){
        
  #read in first ascii    
  a<-raster(paste0(out.dir,species[1],"_final2\\",year[i]))
  #start loop for rest of asciis
  for(ii in 2:length(species)){
    #check number of files
    
   a<-raster(paste0(out.dir,species[ii],"_final2\\",year[i]))+a
    
   
    message(paste(year[i], "and species ",ii))
  }
  plot(a)
  #write raster
  writeRaster(a,paste0(out.dir,"all species\\","Tropical_",year[i],"_sum_50per_species.asc"),NAflag=-9999,overwrite=TRUE)
  
}  
  
  
  ########################
  
  sp_50<-list.files(paste0(out.dir,"all species\\"),pattern="_sum_50per_species.asc",full.names=TRUE)
 
  ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"), interpolate="linear")
  
  
  for (i in 1:length(sp_50)){
    
    a<-raster(sp_50[i])
    names<-strsplit(strsplit(sp_50[i],"thresholded_")[[1]][2],"_10.asc")[[1]][1]
    plot(a,col=ascols(10),asp=1,main=paste(names),zlim=c(0, 34))
    hist(a,main=paste(names),xlab = "number of species")
    
    
    
    
  }
    
  
  n<-extent(112.9167,153.5833,-23.6,-9.333333)
  
  a<-crop(raster(sp_50[1]),n)
  b<-crop(raster(sp_50[2]),n)
  c<-crop(raster(sp_50[3]),n)
  d<-crop(raster(sp_50[4]),n)
  e<-crop(raster(sp_50[5]),n)
  
  
  
  
  #make maps showing quantiles and determin km2 that falls withing each percentile
  
  #quantiles to divide data
  qu <- c(0,0.75,0.80,0.85,0.9,0.95,1)
  #area of all grid cells
  
  map_area<-na.omit(as.vector(area(a)))
   
  #current
  xcut <- quantile(a[a>0], qu,na.rm=TRUE)
  xv <- cut(a, unique(xcut),include.lowest=TRUE)
  
  plot(xv,col=ascols(6),main="Current")
  j<-na.omit(cbind(as.vector(xv),map_area))
  colnames(j)[1]<-"percentile"
  dat<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
  colnames(dat)[2]<-"Current_km2"
  #rcp 45 2035
  xv <- cut(b, unique(xcut),include.lowest=TRUE)
  plot(xv,col=ascols(6),main="RCP45 2035")
  j<-na.omit(cbind(as.vector(xv),map_area))
  colnames(j)[1]<-"percentile"
  dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
  colnames(dat2)[2]<-"RCP45 2035_km2"
  dat<-merge(dat,dat2,by="Group.1")
  #RCP45 2065
  xv <- cut(c, unique(xcut),include.lowest=TRUE)
  plot(xv,col=ascols(6),main="RCP45 2065")
  j<-na.omit(cbind(as.vector(xv),map_area))
  colnames(j)[1]<-"percentile"
  dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
  colnames(dat2)[2]<-"RCP45 2065_km2"
  dat<-merge(dat,dat2,by="Group.1")
  #rcp 85 2035
  xv <- cut(d, unique(xcut),include.lowest=TRUE)
  plot(xv,col=ascols(6),main="RCP85 2035")
  j<-na.omit(cbind(as.vector(xv),map_area))
  colnames(j)[1]<-"percentile"
  dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
  colnames(dat2)[2]<-"RCP85 2035_km2"
  dat<-merge(dat,dat2,by="Group.1")
  #RCP85 2065
  xv <- cut(e, unique(xcut),include.lowest=TRUE)
  plot(xv,col=ascols(6),main="RCP85 2065")
  j<-na.omit(cbind(as.vector(xv),map_area))
  colnames(j)[1]<-"percentile"
  dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
  colnames(dat2)[2]<-"RCP85 2065_km2"
  dat<-merge(dat,dat2,by="Group.1")
  
  
  