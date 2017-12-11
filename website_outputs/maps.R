rm(list = ls())
#load library 
library(raster)
library(maptools)
library(fields)
#map of world  
data(wrld_simpl)


#-------------------------------------------------------------------#
# Preparation stuff, has to be run once.

#get directories where data located
work.dir<-"c:\\daisy\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
data.dir<-paste(work.dir,"data\\",sep="")
grid.dir<-"D:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"

#read in observation SWD file  
sp_dat<- read.csv(paste0(data.dir,"Final_observation_bias_removed_21_10_13.csv"),header=TRUE,)[,c(1:3)]
#get list of species wanted
spec <-list.files(out.dir)[2:254]

# #Extent wanted for Aus maps
aus_extent<-extent(112.9167, 153.5833, -43.66667, -9.333333)
aus_mask<-raster("d:\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)

 states<-readShapePoly(paste0(grid.dir,"Australian_States.shp"))
simp_states<-readShapePoly(paste0(grid.dir,"NRM_Australia_States.shp"))


for(i in 1:length(spec)){
  message("Writing species nr. ",i)
  try(makeMAPS(i, 
                 outdir=out.dir))
  

}





makeMAPS <- function(i, outdir=getwd(), 
                       method="rcom"){
  
  #get species and set working directory to its folder
  Species<- spec[i]
  oldwd <- getwd()
  dir<-paste(out.dir,Species,sep="")
  setwd(dir)
  ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"),interpolate="linear")
  
  
  #make map of global observations
  jpeg(filename = paste(Species,"_global_observations.jpg",sep=""),
       width = 15, height = 10, units = "cm", res=300, bg = "transparent")
  par(mar=c(0,0,0,1),mfrow=c(1,1))
  dat<-subset(sp_dat, species==Species)
  plot(wrld_simpl,axes=FALSE,col="darkgray", asp=1)
  points(dat$lon,dat$lat,
         col="red",pch=20,cex=.5)
  dev.off()

  
  
  #make map of Australian observations
  jpeg(filename = paste(Species,"_Australia_observations.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  locs<-SpatialPoints(cbind(dat$lon,dat$lat))
  dat1<-over(locs,simp_states)
  dat1<-cbind(dat1,dat)
  dat1<-na.omit(dat1)
  rc2 <- raster("species_current_avg.asc")
  rc2<-crop(rc2,aus_extent)
  rc2<-mask(rc2,aus_mask)
  plot(rc2,col="grey",frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
#   image(aus_mask,frame.plot=FALSE,axes = FALSE,asp=1,
#         col="grey",main=NULL)
  points(dat1$lon,dat1$lat,
         col="red",pch=20,cex=5)
  dev.off()
  
 
  
  #make map of current suitable habitat
  
  jpeg(filename = paste(Species,"_current_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  rc2 <- raster("Australia_current.asc")
  rc2<-crop(rc2,aus_extent)
  rc2<-mask(rc2,aus_mask)
   ######  TRIM   ######
  
  plot(rc2,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
  dev.off()
   
  # maps of future projections
  fut <- c("Australia_average_rcp45_2035.asc",
           "Australia_average_rcp85_2035.asc",
           "Australia_average_rcp45_2065.asc",
           "Australia_average_rcp85_2065.asc")
  #crop to right size
  rr1<-crop(raster(fut[1]),aus_extent)
  rr1<-mask(rr1,aus_mask)
  
  rr2<-crop(raster(fut[2]),aus_extent)
  rr2<-mask(rr2,aus_mask)
    
  rr3<-crop(raster(fut[3]),aus_extent)
  rr3<-mask(rr3,aus_mask)
    
  rr4<-crop(raster(fut[4]),aus_extent)
  rr4<-mask(rr4,aus_mask)
    
  jpeg(filename = paste(Species,"_rcp45_2035_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
    plot(rr1,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
  dev.off()
  
  jpeg(filename = paste(Species,"_rcp85_2035_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  plot(rr2,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
  dev.off()
  
  jpeg(filename = paste(Species,"_rcp45_2065_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  plot(rr3,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
  dev.off()
  
  jpeg(filename = paste(Species,"_rcp85_2065_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  plot(rr4,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
  dev.off()
  
  
  
}











