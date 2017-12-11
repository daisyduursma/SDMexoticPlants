
#make sure workspace is clean
rm(list = ls())
#load library 
library(raster)
library(ply)
#get directories where data located


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\logistic\\"
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Global_maps\\",sep="")



#read in observation SWD file
  sp_dat<- read.table(paste(data.dir,"observation_SWD_8_8_12.csv", sep=""),header=TRUE, sep=",")
#species
  spec <-as.vector(unique(sp_dat$species))[1:40]
#maxent output
  maxent<-read.csv(paste(out.dir,"Summary_MaxentResults_paramameter_tests.csv",sep=""))
  maxent$run<-paste(maxent$species,maxent$env.variable.set,maxent$betamultiplier)


land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")

#read in raster of Koeppen values. The resolution of the KG data is .5degrees but I have resampled it to 5 arc minute.  
r <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
#expanded it to the extent of world clim data and mask the land area to world clim data.
r<-expand(r, land_mask) 
r<-mask(r,land_mask)






for (i in 1:length(spec)){

  dat<-subset(sp_dat, species==spec[i])
  locs<- dat[,c("Longitude","Latitude")]
  dat$koeppengei<-extract(r,locs)
  kn_cnt<-as.data.frame(table(dat$koeppengei))
  arrange(kn_cnt,desc(Freq))
  
  