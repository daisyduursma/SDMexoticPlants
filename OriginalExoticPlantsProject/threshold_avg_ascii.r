
#make sure workspace is clean
rm(list = ls())
#load library 


#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


library(raster)
#get directories where data located
work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")


sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

#make list of species
species <-as.vector(unique(sp_dat$species))

rm(sp_dat)
for(i in 195){
  
  
  mfile<-paste(out.dir,species[i],"_final2",sep="")
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
sp<-strsplit(strsplit(afiles[ii],"\\\\")[[1]][6],"_")[[1]][1]

pre_rcp<-strsplit(strsplit(strsplit(afiles[ii],"/")[[1]][2],"_avg.asc")[[1]][1],"species_")[[1]][2]


writeRaster(rc3,filename=paste(mfile,"/",sp,"_",pre_rcp,"_threshold_cumulative_5.asc",sep=""),overwrite=TRUE,NAflag=-9999)




message(i)
}
  
}

