
#make sure workspace is clean
rm(list = ls())
#load library 
# library(dismo)
# library(sp)
# 
# library(maptools)
# 
library(raster)
#get directories where data located


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"#	work.dir<-"F:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
thresh.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Eur_USA_11_10_2012\\", sep="")
#sub_Eur.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Europe\\",sep="")
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Australia_maps\\",sep="")

#read in observation SWD file  

#	sp_dat1<- read.table(paste(data.dir," observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
sp_dat<- read.table(paste(data.dir," observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
#aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")


xy <- cbind(sp_dat$Longitude,sp_dat$Latitude)
sp_dat$AUS<-extract(aus_mask, xy)

sp_dat_AUS<-subset(sp_dat,AUS==0)

spec<-as.vector(unique(sp_dat$species))

for(i in 1 :length(spec)){

# # jpeg(filename = paste(work.dir,"outputs\\website\\",spec[i],"_observations.jpg",sep=""),
#       width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  dat<-subset(sp_dat, species==spec[i])
  aus_dat<-subset(sp_dat_AUS, species==spec[i])
  

plot(dat$bio_1,dat$bio_12,main=paste(spec[i]),xlab="Annual Mean Temperature",ylab="Annual Precipitation",xlim=c(0,300),ylim=c(0,2500))
     
     points(aus_dat$bio_1,aus_dat$bio_12, col="red",pch=19)


dev.copy2pdf(file=paste(out.dir,"Aus vs Global precip and temp\\",spec[i],"_aus_global_precip_temp.pdf",sep=""))

message(i)
}
