
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
#get directories where data located


work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
thresh.dir<-paste(work.dir,"outputs\\Threshold Maps\\set1_bgRK_r1.0_nhnt_10000\\", sep="")
sub_Eur.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Europe\\",sep="")
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Australia_maps\\",sep="")
  
#read in observation SWD file  

	sp_dat<- read.table(paste(data.dir,"sub_Europe_observations_500obsminimum_50perofallobs_19_09_2012.csv", sep=""),header=TRUE, sep=",")
#species
	spec <-as.vector(unique(sp_dat$species))

  sp_dat2<-read.table(paste(data.dir,"observations_10_09_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")
	
#Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
	aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
	aus_mask<-crop(aus_mask,aus_extent)
  

  #mask of land
	fun <- function(x) { x[x>0] <- 0; return(x) }
	rc2 <- calc(raster("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\Threshold Maps\\sub_Europe\\Carex disticha_current_threshold_sub_Europe.asc"), fun)
	
  
for (i in 31:length(spec)){
  
   par(mfrow = c(3,2),mar = c(1,1,2,1))
   dat<-subset(sp_dat, species==spec[i])
  #first maps of previous runs
      sp_files<-list.files(paste(thresh.dir),pattern=spec[i],full.names = TRUE)
       
      plot(raster(sp_files[1]),frame.plot=FALSE,main=paste(spec[i]," all_obs",sep=""),axes = FALSE)
      #Australia
      aus_r2<-crop(raster(sp_files[1]),aus_extent)
      plot(aus_r2,frame.plot=FALSE,axes = FALSE)
     points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
   
      
   #maps of sub Europe
      
      sp_files_sub<-list.files(paste(sub_Eur.dir),pattern=spec[i],full.names = TRUE)
      #global distribution
      plot(raster(sp_files_sub[1]),frame.plot=FALSE,main=paste("subset of Europe",sep=""),axes = FALSE)
      #Australia
      aus_r2<-crop(raster(sp_files_sub[1]),aus_extent)
      plot(aus_r2,frame.plot=FALSE,axes = FALSE)
   points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
   
  #maps with points
   
   
# all obs   
   
   plot(rc2,frame.plot=FALSE,main=paste("all observations",sep=""),axes = FALSE)
   
   #add australian observations
   #get species data
   dat2<-subset(sp_dat2, species==spec[i])
   obs<-nrow(dat2)
   
   #add observations 
   points(dat2$Longitude,dat2$Latitude,col="red",pch=20,cex=.3)
   text(-170,130,paste("# of obs= ",obs),cex=1,pos=4)
   
      
#sub Europe

   plot(rc2,frame.plot=FALSE,main=paste("sub_europe",sep=""),axes = FALSE)
   
   #add australian observations
   #get species data
   
   obs<-nrow(dat)
   
   #add observations 
   points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.3)
   text(-170,130,paste("# of obs= ",obs),cex=1,pos=4)
	
	dev.copy2pdf(file=paste(out.dir,"Suitable habitat pdf sub Europe\\",spec[i],"_suitable_habitat.pdf",sep=""))	
  
  
  message(i)
  
  
	
}
  

  