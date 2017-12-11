
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
#get directories where data located


 work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
external.dir<-"E:\\Exotics_Maxent_output\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
thresh.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Eur_USA_11_10_2012\\", sep="")
#sub_Eur.dir<-paste(work.dir,"outputs\\Threshold Maps\\sub_Europe\\",sep="")
data.dir<-paste(work.dir,"data\\",sep="")
# ascii.dir<- paste(out.dir,"Australia_maps\\",sep="")
  
#read in observation SWD file  

#	sp_dat1<- read.table(paste(data.dir," observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
  
	sp_dat<- read.table(paste(data.dir,"observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
	
#   thresh_dat<-read.csv(paste(out.dir,"Summary_MaxentResults_14_10_2011.csv",sep=""),header=TRUE)
	 
  #species
	spec <-as.vector(unique(sp_dat$species))

#Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#mask of Australia
	aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
  #aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
	aus_mask<-crop(aus_mask,aus_extent)
  

# #mask of land
#   fun <- function(x) { x[x>0] <- 0; return(x) }
#   rc2 <- calc(raster(list.files(paste(thresh.dir),full.names=TRUE)[1]), fun)
	
#make loop for each species  
for (i in 1:length(spec)){
  
  #map of observations
  
  jpeg(filename = paste(work.dir,"outputs\\website\\",spec[i],"_observations.jpg",sep=""),
       width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  dat<-subset(sp_dat, species==spec[i])
  #map with Australia obs
  #aus_r<-crop(rc2,aus_extent)
  plot(aus_mask,frame.plot=FALSE,axes = FALSE,legend=FALSE,col="darkgrey",main=NULL,box=FALSE)
  points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=3)
  
   dev.off()
}
  
#map of current suitable habitat
  
  cur<-list.files(external.dir,pattern="current_threshold_cumulative_5",recursive=TRUE,full.names=TRUE)
  
	ascols = colorRampPalette(c("darkgray","yellow","orange","darkgreen"),interpolate="linear")

  
  for (i in 1:length(cur)){
    
   file_name<-strsplit(cur[i],"/")[[1]][3]
   spec<-strsplit(file_name,"_current")[[1]][1]

 
    
  jpeg(filename = paste(work.dir,"outputs\\website\\",spec,"_suitable_habitat.jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  #map with of cumulative_5
  ras4<-cur[i]
  aus_r4<-crop(raster(ras4),aus_extent)
  aus_r4<-mask(aus_r4,aus_mask)
  plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(100),main=NULL)
  dev.off()
  

}

  
#maps of future
  
	ascols = colorRampPalette(c("darkgray","yellow","orange","red"),interpolate="linear")
  
  
  fut<-list.files(external.dir,pattern="agreement_72",recursive=TRUE,full.names=TRUE)
  
  

	ascols = colorRampPalette(c("darkgray","yellow","orange","red"),interpolate="linear") 
  
  for (i in 1:length(fut)){
  
   file_name<-strsplit(fut[i],"/")[[1]][2]
   spec<-strsplit(file_name,"_final")[[1]][1]
   file_name2<-strsplit(fut[i],"/")[[1]][3]
   run<-strsplit(file_name2,".asc")[[1]][1]
 
    
  jpeg(filename = paste(work.dir,"outputs\\website\\",spec,"_",run,".jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  #map with of cumulative_5
  ras4<-fut[i]
  aus_r4<-crop(raster(ras4),aus_extent)
  aus_r4<-mask(aus_r4,aus_mask)
  plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(100),main=NULL)
  dev.off()
  

}

  
#maps of combined species
  
	jpeg(filename = paste(work.dir,"\\ESA\\current_suitability.jpg",sep=""),width = 1800, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
  
	a<-raster("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\SUMMARY_ASCII\\57_sp_cur_75_agree.asc")+raster("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\SUMMARY_ASCII\\26_sp_cur_75_agree.asc")
  
	plot(a,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(100),main=NULL)
	
	
	
	dev.off()
	
  
  
  
  
  
  
  
  
  
  
  
  
  
  fut
  #set up plot
   par(mfrow = c(1,1),mar = c(1,1,2,1))
   dat<-subset(sp_dat, species==spec[i])
#   #map with global obs
#     plot(rc2,frame.plot=FALSE,main=paste(spec[i],"sub eur usa",sep=""),axes = FALSE,legend=FALSE)
#     points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
#     obs<-nrow(dat)
#     text(-170,-50,paste("# of obs= ",obs),cex=1,pos=4)
#     
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
  
   
  
	dev.copy2pdf(file=paste(out.dir,"Suitable habitat pdf sub Europe and USA 21_10_2011\\",spec[i],"_suitable_habitat.pdf",sep=""))	
  
  
  message(i)
  
  
	
}
  

  