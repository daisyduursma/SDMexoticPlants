
#make sure workspace is clean
	rm(list = ls())
#load library 
	library(raster)
	library(maptools)
#map of world  
	data(wrld_simpl)
  
  
#-------------------------------------------------------------------#
# Preparation stuff, has to be run once.
  
  #get directories where data located
  work.dir<-"f:\\Current_Projects\\exotic_plants\\"
  #external.dir<-"E:\\Exotics_Maxent_output\\"
  out.dir<-paste(work.dir,"outputs\\",sep="")
  #thresh.dir<-paste(work.dir,"outputs\\Threshold Maps 12_29_12\\", sep="")
  #dir.create(thresh.dir)
  data.dir<-paste(work.dir,"data\\",sep="")
    
  #read in observation SWD file  
	sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")[,c(1:3)]
  
  #replace species names
  
  
	#Cleome houtteana to Tarenaya hassleriana and Lycopersicon esculentum to Solanum lycopersicon 
  #trait data
	trait<-read.csv(paste(data.dir,"Data traits updated.csv", sep=""), header=TRUE)
  
  
	levels(trait$species)[levels(trait$species)=="Tarenaya hassleriana"] <- "Cleome houtteana"
	levels(trait$species)[levels(trait$species)== "Solanum lycopersicon"] <- "Lycopersicon esculentum"
  
  # 	
  	#sp_dat<- read.table("observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv",header=TRUE, sep=",")
    
  #species
  	spec <-as.vector(unique(sp_dat$species))
 
# #Extent wanted for Aus maps
	aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
# #mask of Australia
	#aus_mask<-raster("Australia.asc")
# aus_mask<-raster("C:\\Daisy\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
aus_mask<-crop(aus_mask,aus_extent)
  
#---------------------------------------------------------------------#

# Make report for all 292 species (source function below first!)  
for(i in 1:292){
  message("Writing species nr. ",i)
  try(makeReport(i, 
             outdir="h:\\Current_Projects\\exotic_plants\\paper\\NCCARF Reports\\species_profiles\\", 
             method="rcom",
             closeWord=TRUE))
  Sys.sleep(5)
}
 
  

  
makeReport <- function(i, closeWord=FALSE, outdir=getwd(), 
                       method="RDComClient"){

  if(!exists("trait"))stop("Make sure to load object \'trait\' first!")
  Family <- as.character(trait[trait$species==spec[i] ,"Family"])
  Growthform<- as.character(trait[trait$species==spec[i], "growth_forms"])
  Longevity<- as.character(trait[trait$species==spec[i],"longevity"])
  Dispersal <- as.character(trait[trait$species==spec[i],"Dispersal.mode"])
  Habitat <- as.character(trait[trait$species==spec[i],"habitat_types"])
  area<- round(((as.numeric(trait[trait$species==spec[i],"Extent.of.suitable.habitat.under.RCP8.5.conditions.for.2035.of.Australia.km2"])/as.numeric(trait[trait$species==spec[i],"Extent.of.suitable.habitat.under.current.conditions.of.Australia.km2"]))*100)-100,1)
  
 Observations <-as.character(trait[trait$species==spec[i],"Number.of.known.infestations.in.Australia"])
  

  r <- require(R2wd)
  if(!r)stop("Install package R2wd first!")
  
  # species name
  SP <- spec[i]

  oldwd <- getwd()
  dir<-paste(out.dir,SP,"_final2\\",sep="")
  setwd(dir)
  
  # open word document, write title.
  wdGet(method=method)

  wdHeading(text=SP)
#   wdWrite("\n")
  
  writeField <- function(label,  txt){  #, ital=FALSE){
    wdNormal(label, FALSE)
#     if(ital)wdSetFont(italic=TRUE)
    wdWrite(txt, FALSE)
    wdWrite("\n")
#     if(ital)wdsetFont(italic=FALSE)
#     wdWrite("\t\n")
  }
  writeField("Family: ",Family)
  writeField("Growth form: ", Growthform)
#   wdWrite("\n")
  writeField("Habitat: ",Habitat)
  writeField("Dispersal mode: ", Dispersal)
#   wdWrite("\n")
  writeField("Longevity: ",Longevity)
  writeField("Gridded Observations in Australia (excluding gardens): ",Observations)
  writeField("Change in area of suitable habitat between current conditions and RCP8.5 2023: ",area," %")
#   wdWrite("\n")
  #map of observations
  #observations for species

  wdHeading(level=3, text="\t\tObservations\t\t\t\t\tCurrent suitable habitat")
  
  plot1 <- function(Species){
    par(mar=c(0,0,0,1),mfrow=c(1,1))
    
    dat<-subset(sp_dat, species==Species)
  	plot(wrld_simpl,axes=FALSE,col="darkgray", asp=1)
    #add observations 
  	points(dat$Longitude,dat$Latitude,
           col="red",pch=20,cex=1)
  }
  wdPlot(SP, plotfun=plot1, height=2, width=3, paragraph=FALSE)
  
    
  #map of current Australia
    #ascii of current thresholded
  #   cur<-list.files(paste(out.dir,SP,"_final2\\",sep=""),
  #                   pattern="current_threshold_cumulative_5",
  #                   recursive=TRUE,full.names=TRUE)
  cur <- "current_10.asc"
  
  wdBody("\t", paragraph=FALSE)
  
  # wrap in a plotting function
  plotCurrent <- function(...){
    
#     par(mar=c(0,0,0,1),mfrow=c(1,1))
    #color ramp
  	ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"),interpolate="linear")
    #clip to australia
    aus_r4<-raster(cur)
    #aus_r4<-mask(aus_r4,aus_mask)
    #apply threshold
        #get file with maxent results(
#         dat1<-read.csv("maxentResults.csv",row.names=1)
#         #get threshold
#         thresh5<-dat1["species (average)","Fixed.cumulative.value.10.logistic.threshold"]
#         fun <- function(x) { x[x<thresh5] <- 0; return(x) }
#         rc2 <- calc(aus_r4, fun)
#     
    
    #plot
    par(mar=c(0,0,0,0),mfrow=c(1,1))
    image.plot(rc2,frame.plot=FALSE,axes = FALSE,
          col=ascols(5),asp=1,main="",zlim=c(0, 1))
  }    
    
    #plot
#   	par(mar=c(0,0,1,1),mfrow=c(1,1))
#     plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE,main="",
#          box=FALSE,col=ascols(100)) #,main="Current")

  wdPlot(plotfun=plotCurrent, height=2, width=2.5, #method="bitmap", pointsize=6,
         paragraph=FALSE)
  wdBody("\n")
  
  
  # maps of future projections
  
  #color ramp
  ascols <- colorRampPalette(c("darkgray","yellow","orange","red","black"),interpolate="linear")
  #list of files
  #fut<-list.files(paste(out.dir,spec[i],"_final2\\",sep=""),pattern="100_per",recursive=TRUE,full.names=TRUE)
  
  fut <- c("average_rcp45_2035_10.asc",
  "average_rcp85_2035_10.asc",
  "average_rcp45_2065_10.asc",
  "average_rcp85_2065_10.asc")
  
  wdHeading(level=2, text="Future suitable habitat")
#   wdWrite("\n")
  
  #plot all 4 future scen
  plotFuture1 <- function(...){
  	par(mar=c(0,0,1,0),cex.main=0.8)
    rep<-4.5
    yr<-"  2035"
    image(raster(fut[1]),frame.plot=FALSE,axes = FALSE,asp=1,
         col=ascols(5),main=paste("RCP",rep," ",yr,sep=""),zlim=c(0, 1))
  }
  plotFuture2 <- function(...){
    par(mar=c(0,0,1,0),cex.main=0.8)
    rep<-8.5
    yr<-"  2035"
    image(raster(fut[2]),frame.plot=FALSE,axes = FALSE,asp=1,
          col=ascols(5),main=paste("RCP",rep," ",yr,sep=""),zlim=c(0, 1))
  }
  plotFuture3 <- function(...){
    par(mar=c(0,0,1,0),cex.main=0.8)
    rep<-4.5
    yr<-"  2065"
    image(raster(fut[3]),frame.plot=FALSE,axes = FALSE,asp=1,
          col=ascols(5),main=paste("RCP",rep," ",yr,sep=""),zlim=c(0, 1))
  }
  plotFuture4 <- function(...){
    par(mar=c(0,0,1,0),cex.main=0.8)
    rep<-8.5
    yr<-"  2065"
    image(raster(fut[4]),frame.plot=FALSE,axes = FALSE,asp=1,
          col=ascols(5),main=paste("RCP",rep," ",yr,sep=""),zlim=c(0, 1))
  }
  fw <- 1.8 # figure width (inches)
  fh <- 1.8 # figure height(inches) 
  wdPlot(plotfun=plotFuture1, height=fh, width=fw, paragraph=FALSE)
  wdWrite("\t\t\t\t")
  wdPlot(plotfun=plotFuture2, height=fh, width=fw, paragraph=FALSE)
  wdWrite("\n")
  wdPlot(plotfun=plotFuture3, height=fh, width=fw, paragraph=FALSE)
  wdWrite("\t\t\t\t")
  wdPlot(plotfun=plotFuture4, height=fh, width=fw, paragraph=FALSE)

  # save document
#   if(!file.exists(outdir))stop("Create output directory first!")
    speciesnospace <- gsub(" ", "_", SP)
    wdSave(Name=paste0(outdir,"/",speciesnospace,"_INFO.doc"))
    Sys.sleep(2)
    if(closeWord)wdQuit()  
  
  
  setwd(oldwd)
}
  
  
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
# 	ascols = colorRampPalette(c("darkgray","yellow","orange","red"),interpolate="linear") 
#   
#   for (i in 1:length(fut)){
#   
#    file_name<-strsplit(fut[i],"/")[[1]][2]
#    spec<-strsplit(file_name,"_final")[[1]][1]
#    file_name2<-strsplit(fut[i],"/")[[1]][3]
#    run<-strsplit(file_name2,".asc")[[1]][1]
#  
#     
#   jpeg(filename = paste(work.dir,"outputs\\website\\",spec,"_",run,".jpg",sep=""),width = 1400, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#   #map with of cumulative_5
#   ras4<-fut[i]
#   aus_r4<-crop(raster(ras4),aus_extent)
#   aus_r4<-mask(aus_r4,aus_mask)
#   plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(100),main=NULL)
#   dev.off()
#   
# 
# }
# 
#   
# #maps of combined species
#   
# 	jpeg(filename = paste(work.dir,"\\ESA\\current_suitability.jpg",sep=""),width = 1800, height = 1400, units = "px", pointsize = 12,quality = 300, bg = "transparent")
#   
# 	a<-raster("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\SUMMARY_ASCII\\57_sp_cur_75_agree.asc")+raster("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\SUMMARY_ASCII\\26_sp_cur_75_agree.asc")
#   
# 	plot(a,frame.plot=FALSE,axes = FALSE,legend=FALSE,box=FALSE,col=ascols(100),main=NULL)
# 	
# 	
# 	
# 	dev.off()
# 	
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   #set up plot
#    par(mfrow = c(1,1),mar = c(1,1,2,1))
#    dat<-subset(sp_dat, species==spec[i])
# #   #map with global obs
# #     plot(rc2,frame.plot=FALSE,main=paste(spec[i],"sub eur usa",sep=""),axes = FALSE,legend=FALSE)
# #     points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
# #     obs<-nrow(dat)
# #     text(-170,-50,paste("# of obs= ",obs),cex=1,pos=4)
# #     
#   #map with Australia obs
#    aus_r<-crop(rc2,aus_extent)
#    plot(aus_r,frame.plot=FALSE,axes = FALSE,legend=FALSE)
#    points(dat$Longitude,dat$Latitude,col="red",pch=20,cex=.7)
#         
#    #maps 10 percent
#       #global thresholded
#       ras1<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_10_percentile.asc",sep="")
#       plot(raster(ras1),frame.plot=FALSE,main=paste("10_percentile",sep=""),axes = FALSE,legend=FALSE)
#       #threshold value
#       t_dat<-subset(thresh_dat,species==spec[i],select=X10.percentile.training.presence.logistic.threshold)
#       text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
#       #Australia
#       aus_r1<-crop(raster(ras1),aus_extent)
#       plot(aus_r1,frame.plot=FALSE,axes = FALSE,legend=FALSE)
#    
#     #Balance_training
#       ras2<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Balance_training.asc",sep="")
#       plot(raster(ras2),frame.plot=FALSE,main=paste("Balance_training",sep=""),axes = FALSE,legend=FALSE)
#       t_dat<-subset(thresh_dat,species==spec[i],select=Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold)
#       text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
#       aus_r2<-crop(raster(ras2),aus_extent)
#       plot(aus_r2,frame.plot=FALSE,axes = FALSE,legend=FALSE)
#   
#   
#     #Equal_training
#       ras3<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Equal_training.asc",sep="")
#       plot(raster(ras3),frame.plot=FALSE,main=paste("Equal_training",sep=""),axes = FALSE,legend=FALSE)
#       t_dat<-subset(thresh_dat,species==spec[i],select=Equal.training.sensitivity.and.specificity.logistic.threshold)
#       text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
#       aus_r3<-crop(raster(ras3),aus_extent)
#       plot(aus_r3,frame.plot=FALSE,axes = FALSE,legend=FALSE)
#       
#     
#     #Maximum.training
#       ras4<-paste(thresh.dir,spec[i],"_current_threshold_sub_Europe_Max_training.asc",sep="")
#       plot(raster(ras4),frame.plot=FALSE,main=paste("Max_training",sep=""),axes = FALSE,legend=FALSE)
#       t_dat<-subset(thresh_dat,species==spec[i],select=Maximum.training.sensitivity.plus.specificity.logistic.threshold)
#       text(-170,-50,paste("thresh = ",t_dat),cex=1,pos=4)
#       aus_r4<-crop(raster(ras4),aus_extent)
#       plot(aus_r4,frame.plot=FALSE,axes = FALSE,legend=FALSE)
#    
#    #5% omission
#   
#    
#   
# 	dev.copy2pdf(file=paste(out.dir,"Suitable habitat pdf sub Europe and USA 21_10_2011\\",spec[i],"_suitable_habitat.pdf",sep=""))	
#   
#   
#   message(i)
#   
#   
# 	
# }
#   
# 
#   
# 	#   thresh_dat<-read.csv(paste(out.dir,"Summary_MaxentResults_14_10_2011.csv",sep=""),header=TRUE)
# 	
  