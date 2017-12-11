
#make sure workspace is clean
  rm(list = ls())

#libs
  library(raster)
  
  #colors for maps
  ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"), interpolate="linear")
  
  outdir="D:\\Current_Projects\\exotic_plants\\outputs\\all species\\"
    
  files<-list.files(path=paste(outdir),pattern="AUS",recursive=TRUE,full.names=TRUE) 
  
  
#read in first ascii    
  
  a<-raster(files[1])
  cellStats(a,stat="max")
  b<-raster(files[4])
  cellStats(b,stat="max")
  c<-raster(files[5])
  cellStats(c,stat="max")
  
#make histograms
  
  hist(a,ylim=c(0,20000))
  hist(b,ylim=c(0,20000))
  hist(c,ylim=c(0,20000))
  
  #make plots with same colours
  
  plot(a,col=ascols(250),asp=1,main="Current",zlim=c(0, 250))
  plot(b,col=ascols(250),asp=1,main="RCP85 2035",zlim=c(0, 250))
  plot(c,col=ascols(250),asp=1,main="RCP85 2065",zlim=c(0, 250))
 
########  
#make maps showing quantiles and determin km2 that falls withing each percentile
  
  #quantiles to divide data
    qu <- c(0,0.75,0.80,0.85,0.9,0.95,1)
  #area of all grid cells
   map_area<-as.vector(area(a))
  #current
    xcut <- quantile(a[a>0], qu,na.rm=TRUE)
    xv <- cut(a, unique(xcut),include.lowest=TRUE)
    plot(xv,col=ascols(6),main="Current")
    j<-na.omit(cbind(as.vector(xv),map_area))
    colnames(j)[1]<-"percentile"
    dat<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
    colnames(dat)[2]<-"Current_km2"
  #rcp 85 2035
    xv <- cut(b, unique(xcut),include.lowest=TRUE)
    plot(xv,col=ascols(6),main="RCP85 2035")
    j<-na.omit(cbind(as.vector(xv),map_area))
    colnames(j)[1]<-"percentile"
    dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
    colnames(dat2)[2]<-"RCP85 2035_km2"
    dat<-merge(dat,dat2,by="Group.1")
  #RCP85 2065
    xv <- cut(c, unique(xcut),include.lowest=TRUE)
    plot(xv,col=ascols(6),main="RCP85 2065")
    j<-na.omit(cbind(as.vector(xv),map_area))
    colnames(j)[1]<-"percentile"
    dat2<-aggregate(j[,"map_area"], list(j[,"percentile"]), sum)
    colnames(dat2)[2]<-"RCP85 2065_km2"
    dat<-merge(dat,dat2,by="Group.1")
  
  
  
  
  
  
  
  