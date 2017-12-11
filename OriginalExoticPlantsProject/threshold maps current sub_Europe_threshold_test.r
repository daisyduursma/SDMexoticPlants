
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
#get directories where data located


 work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
#	work.dir<-"F:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
thresh.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Eur_USA_11_10_2012\\", sep="")
#sub_Eur.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Europe\\",sep="")
data.dir<-paste(work.dir,"data\\",sep="")
ascii.dir<- paste(out.dir,"Australia_maps\\",sep="")
  
#read in observation SWD file  

	sp_dat<- read.table(paste(data.dir," observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
  thresh_dat<-read.csv(paste(out.dir,"Summary_MaxentResults_14_10_2011.csv",sep=""),header=TRUE)
	 
  #species
	spec <-as.vector(unique(sp_dat$species))

#Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
	aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
  #aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
	aus_mask<-crop(aus_mask,aus_extent)
  

#mask of land
  fun <- function(x) { x[x>0] <- 0; return(x) }
  rc2 <- calc(raster(list.files(paste(thresh.dir),full.names=TRUE)[1]), fun)
	
#make loop for each species  
for (i in 1:length(spec)){
  
  #get summary data for species
  
  
  #set up plot
   par(mfrow = c(6,2),mar = c(1,1,2,1))
   dat<-subset(sp_dat, species==spec[i])
  #map with global obs
    plot(rc2,frame.plot=FALSE,main=paste(spec[i],"sub eur usa",sep=""),axes = FALSE,legend=FALSE)
    points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
    obs<-nrow(dat)
    text(-170,-50,paste("# of obs= ",obs),cex=1,pos=4)
    
  #map with Australia obs
   aus_r<-crop(rc2,aus_extent)
   plot(aus_r,frame.plot=FALSE,axes = FALSE,legend=FALSE)
   points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
        
   #maps 10 percent
      #global thresholded
      ras1<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_10_percentile.asc",sep="")
      plot(raster(ras1),frame.plot=FALSE,main=paste("10_percentile",sep=""),axes = FALSE,legend=FALSE)
      #threshold value
      t_dat<-subset(thresh_dat,species==spec[i],select=X10.percentile.training.presence.logistic.threshold)
      text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
      #Australia
      aus_r1<-crop(raster(ras1),aus_extent)
      plot(aus_r1,frame.plot=FALSE,axes = FALSE,legend=FALSE)
   
    #Balance_training
      ras2<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Balance_training.asc",sep="")
      plot(raster(ras2),frame.plot=FALSE,main=paste("Balance_training",sep=""),axes = FALSE,legend=FALSE)
      t_dat<-subset(thresh_dat,species==spec[i],select=Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold)
      text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
      aus_r2<-crop(raster(ras2),aus_extent)
      plot(aus_r2,frame.plot=FALSE,axes = FALSE,legend=FALSE)
  
  
    #Equal_training
      ras3<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Equal_training.asc",sep="")
      plot(raster(ras3),frame.plot=FALSE,main=paste("Equal_training",sep=""),axes = FALSE,legend=FALSE)
      t_dat<-subset(thresh_dat,species==spec[i],select=Equal.training.sensitivity.and.specificity.logistic.threshold)
      text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
      aus_r3<-crop(raster(ras3),aus_extent)
      plot(aus_r3,frame.plot=FALSE,axes = FALSE,legend=FALSE)
      
    
    #Maximum.training
      ras4<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Max_training.asc",sep="")
      plot(raster(ras4),frame.plot=FALSE,main=paste("Max_training",sep=""),axes = FALSE,legend=FALSE)
      t_dat<-subset(thresh_dat,species==spec[i],select=Maximum.training.sensitivity.plus.specificity.logistic.threshold)
      text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
      aus_r4<-crop(raster(ras4),aus_extent)
      plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE)
   
   #5% omission
   ras4<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_cumulative_5.asc",sep="")
   plot(raster(ras4),frame.plot=FALSE,main=paste("cumulative_5",sep=""),axes = FALSE,legend=FALSE)
   t_dat<-subset(thresh_dat,species==spec[i],select=Fixed.cumulative.value.5.logistic.threshold)
   text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
   aus_r4<-crop(raster(ras4),aus_extent)
   plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE)
      
   
  
	dev.copy2pdf(file=paste(out.dir,"Suitable habitat pdf sub Europe and USA 21_10_2011\\",spec[i],"_suitable_habitat.pdf",sep=""))	
  
  
  message(i)
  
  
	
}
  

  