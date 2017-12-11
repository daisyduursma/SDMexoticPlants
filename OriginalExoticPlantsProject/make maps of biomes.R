
#make sure workspace is clean
rm(list = ls())

library(raster)
library(maptools)
#get directories where data located
work.dir<-"c:\\daisy\\Current_Projects\\exotic_plants\\outputs\\all species\\"
out.dir<-"C:\\daisy\\Current_Projects\\exotic_plants\\paper\\NCCARF Reports\\tables_figures\\"
poly_states<-states<-readShapePoly("c:\\daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\Bioregions.shp")

#rasterize shapefile
r<-raster(paste(work.dir,"AUS_current_sum_all_species.asc",sep=""))
a<-rasterize(poly_states,r,field="ECO_NAME")
#get a list of uniue regions
regions<-unique(a)

time<-c("current_10","rcp85_2035_10","rcp85_2065_10","rcp85_2035_10","rcp85_2065_10")
########### trim rasters to Biomes #########
{
#MAKE rasters for Biomes  
for(i in 1:length(time)){}
  
 
state_mask<-a
for(ii in 1:length(regions)){
  state_mask<-a
 state_mask[state_mask != regions[ii]] <- NA
 state<-mask(r,state_mask)
 state<-trim(state,  padding=1)
  plot(state)
  
  writeRaster(state,paste(work.dir,regions[1],"_RCP85_2035_sum_all_species_trim.asc",sep=""),overwrite=TRUE,NAflag=-9999)
}

##########

for (i in 2:8){
  r<-raster(paste(work.dir,regions[i],"_RCP85_2035_sum_all_species.asc",sep=""))
  state_mask<-a
  state_mask[state_mask != i+1] <- NA
  state<-mask(r,state_mask)
  state<-trim(state,padding=1)
  writeRaster(state,paste(work.dir,regions[i],"_RCP85_2035_sum_all_species_trim.asc",sep=""),overwrite=TRUE,NAflag=-9999)
  plot(state)
# }

}
###############################
  ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"), interpolate="linear")
  ascols2 <- colorRampPalette(c("yellow","darkgray","red"), interpolate="linear")


for(i in 1:8){
  a<-raster(paste(work.dir,regions[i],"_current_sum_all_species_trim.asc",sep=""))
  b<-raster(paste(work.dir,regions[i],"_RCP85_2035_sum_all_species_trim.asc",sep=""))
  print(regions[i])
  print(cellStats(a, stat=mean))
  print(cellStats(b, stat=mean))
  print(cellStats(a, stat=max))
  print(cellStats(b, stat=max))
  print(cellStats(a, stat=min))
  print(cellStats(b, stat=min))
}

for(i in 1:8){
  
a<-raster(paste(work.dir,regions[i],"_current_sum_all_species_trim.asc",sep=""))
b<-raster(paste(work.dir,regions[i],"_RCP85_2035_sum_all_species_trim.asc",sep=""))
c<-a-b
  
#asc_max<-max(c(cellStats(c, stat='max'),cellStats(b, stat='max')))
asc_max<-cellStats(c, stat='max')
asc_min<-asc_max-(2*asc_max)
  
jpeg(filename = paste(out.dir,regions[i],".jpg",sep=""),
  width = 20, height = 12, units = "cm", res=600, bg = "transparent")

par(mfrow = c(1, 2),oma=c(0,0,0,3))
plot(a,col=ascols(8),asp=1,main="Current")
plot(c,col=ascols2(8),asp=1,main="Future",zlim=c(asc_min, asc_max))

dev.off()
}
  
  
#Australia
i<-9
    
    a<-raster(paste(work.dir,regions[i],"_current_sum_all_species.asc",sep=""))
    b<-raster(paste(work.dir,regions[i],"_RCP85_2035_sum_all_species.asc",sep=""))
    c<-a-b
    
    #asc_max<-max(c(cellStats(c, stat='max'),cellStats(b, stat='max')))
    asc_max<-cellStats(c, stat='max')
    asc_min<-asc_max-(2*asc_max)
    
    jpeg(filename = paste(out.dir,regions[i],".jpg",sep=""),
         width = 19,height = 27, units = "cm", res=600, bg = "transparent")
    
    par(mfrow = c(2, 1))
    plot(a,col=ascols(8),asp=1,main="Current")
    plot(c,col=ascols2(15),asp=1,main="Future",zlim=c(asc_min, asc_max))
    
    dev.off()
 
  
  
  
# dev.off()
# 
# 
# # jpeg(filename = paste(out.dir,"NSW.jpg",sep=""),
# #      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[2]),col=ascols(8),asp=1,main="NSW")
# 
# dev.off()
# 
# jpeg(filename = paste(out.dir,"NT.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[3]),col=ascols(8),asp=1,main="NT")
# 
# dev.off()
# 
# jpeg(filename = paste(out.dir,"QLD.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[4]),col=ascols(7),asp=1,main="QLD")
# 
# dev.off()
# 
# jpeg(filename = paste(out.dir,"SA.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[5]),col=ascols(10),asp=1,main="SA")
# 
# dev.off()
# 
# jpeg(filename = paste(out.dir,"TAS.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[6]),col=ascols(5),asp=1,main="TAS")
# 
# dev.off()
# 
# 
# jpeg(filename = paste(out.dir,"VIC.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[7]),col=ascols(7),asp=1,main="VIC")
# dev.off()
# 
# jpeg(filename = paste(out.dir,"WA.jpg",sep=""),
#      width = 15, height = 15, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[8]),col=ascols(10),asp=1,main="WA")
# dev.off()
#   
#   
#  
#   
#   
# 
# jpeg(filename = paste(out.dir,"AUS.jpg",sep=""),
#      width = 20, height = 20, units = "cm", res=600, bg = "transparent")
# plot(raster(asc[9]),col=ascols(10),asp=1,main="AUS")
# dev.off()
# 
# 
# 
#   
# #   
# # 
# # jpeg(filename = paste(work.dir,"RCP4.5 2035_rescaled.jpg",sep=""),
# #      width = 12, height = 10, units = "cm", res=300, bg = "transparent")
# # 
# # plot(raster(asc[2]),box=FALSE,frame.plot=FALSE,axes = FALSE,col=ascols(10),asp=1,main="RCP4.5 2035",breaks=brk, axis.args=arg)
# # 
# # dev.off()
# # 
# # 
# # jpeg(filename = paste(work.dir,"RCP4.5 2065_rescaled.jpg",sep=""),
# #      width = 12, height = 10, units = "cm", res=300, bg = "transparent")
# # 
# # plot(raster(asc[3]),box=FALSE,frame.plot=FALSE,axes = FALSE,col=ascols(10),asp=1,main="RCP4.5 2065",breaks=brk, axis.args=arg)
# # 
# # dev.off()
# # 
# # 
# # jpeg(filename = paste(work.dir,"RCP8.5 2035_rescaled.jpg",sep=""),
# #      width = 12, height = 10, units = "cm", res=300, bg = "transparent")
# # 
# # plot(raster(asc[4]),box=FALSE,frame.plot=FALSE,axes = FALSE,col=ascols(10),asp=1,main="RCP8.5 2035",breaks=brk, axis.args=arg)
# # 
# # dev.off()
# # 
# # 
# # jpeg(filename = paste(work.dir,"RCP8.5 2065_rescaled.jpg",sep=""),
# #      width = 12, height = 10, units = "cm", res=300, bg = "transparent")
# # 
# # plot(raster(asc[5]),box=FALSE,frame.plot=FALSE,axes = FALSE,col=ascols(10),asp=1,main="RCP8.5 2065",breaks=brk, axis.args=arg)
# # 
# # dev.off()
# # 
# # 
# # 
