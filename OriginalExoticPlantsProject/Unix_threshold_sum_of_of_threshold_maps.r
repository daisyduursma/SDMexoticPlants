
#make sure workspace is clean
rm(list = ls())
#load library 


library(raster)
#get directories where data located
work.dir<-"H:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"/outputs/",sep="")


sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

#make list of species
species <-as.vector(unique(sp_dat$species))

rm(sp_dat)
for(i in 40){
  
  
  mfile<-paste(out.dir,species[i],"_final",sep="")
  #get file with maxent results(
  dat1<-read.csv(paste(mfile,"//maxentResults.csv",sep=""),row.names=1)
  #get threshold
  thresh5<-dat1["species (average)","Fixed.cumulative.value.5.logistic.threshold"]
  #get ascii files
  afiles<-list.files(mfile,pattern="avg.asc",full.names=TRUE)

  for(ii in 1:length(afiles)){

    #read in ascii as raster
    sp_avg<-raster(paste(afiles[ii]))
    #apply threshold
    fun <- function(x) { x[x<thresh5] <- 0; return(x) }
    rc2 <- calc(sp_avg, fun)
    fun <- function(x) { x[x>=thresh5] <- 1; return(x) }
    rc3 <- calc(rc2, fun)

    #get file name
sp<-strsplit(strsplit(afiles[ii],"/")[[1]][8],"_")[[1]][1]
gen<-strsplit(strsplit(afiles[ii],"/")[[1]][8],"_")[[1]][2]

pre_rcp<-strsplit(strsplit(strsplit(afiles[ii],"/")[[1]][9],"_avg.asc")[[1]][1],"species_")[[1]][2]


writeRaster(rc3,filename=paste(mfile,"/",sp,"_",gen,"_",pre_rcp,"_threshold_cumulative_5.asc",sep=""),overwrite=TRUE,NAflag=-9999)




message(i)
}
  
}

