

#make sure workspace is clean
rm(list = ls())

r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
library(maptools)

aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)

#### already have this
# r_all<-raster("f:/Current_Projects/exotic_plants/outputs/Phytolacca americana_final2/species_Current_avg.asc")
# 
# jpeg(filename = paste(Species,"_current_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
# par(mar=c(0,0,0,0),mfrow=c(1,1))
# rc2 <- raster("species_current_avg.asc")
# rc2<-crop(rc2,aus_extent)
# rc2<-mask(rc2,aus_mask)
# plot(rc2,col=ascols(10),frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
# dev.off()

jpeg(filename = paste("C:\\Daisy\\Current_Projects\\exotic_plants\\workshop\\images\\Phytolacca americana_current_suitable_habitat_10_omission.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")

r10<-raster("f:/Current_Projects/exotic_plants/outputs/Phytolacca americana_final2/thresholded_current_10.asc")

# par(mar=c(0,0,0,0),mfrow=c(1,1))
# rc2 <- raster("species_current_avg.asc")
# rc2<-crop(rc2,aus_extent)
# rc2<-mask(rc2,aus_mask)
plot(r10,frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
dev.off()



r10b<-raster("f:/Current_Projects/exotic_plants/outputs/Phytolacca americana_final2/current_10.asc")


thresh5<-0.5

  #apply threshold
  fun <- function(x) { x[x<thresh5] <- 0; return(x) }
  rc2 <- calc(r10b, fun)
  fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
  rc3 <- calc(rc2, fun)
  
jpeg(filename = paste("C:\\Daisy\\Current_Projects\\exotic_plants\\workshop\\images\\Phytolacca americana_current_suitable_habitat_.5thresh.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")

plot(rc3,frame.plot=FALSE,box=FALSE,axes = FALSE,asp=1,main="",zlim=c(0, 1),legend=FALSE)
dev.off()


