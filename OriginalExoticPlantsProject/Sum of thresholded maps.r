rm(list = ls())
#load library 

#.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")

library(raster)
#get directories where data located
work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")
#read in observation SWD file
sp_dat<- read.table(paste(work.dir,"data\\", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
#all_background<-read.table(paste(work.dir,"data/","RK_background_10000_05_11_12_8km_grid.csv", sep=""),header=TRUE, sep=",")
#make list of species
species <-as.vector(unique(sp_dat$species))

rm(sp_dat)

for(i in 195)  {
  
  rcp45_2035<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP45.+2035_threshold_cumulative_5",full.name=TRUE)
    s1<-stack(raster(rcp45_2035[1]),raster(rcp45_2035[2]),raster(rcp45_2035[3]),raster(rcp45_2035[4]),raster(rcp45_2035[5]),raster(rcp45_2035[6]),raster(rcp45_2035[7]))
    sum_rcp<- calc(s1, sum)
      writeRaster(sum_rcp,paste(out.dir,species[i],"_final2/sum_threshold_rcp45_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    fun <- function(x) { x[x<7] <- 0; return(x) }
    rc2 <- calc(sum_rcp, fun)
    fun <- function(x) { x[x>=7] <- 1; return(x) }
    rc3 <- calc(rc2, fun)
    writeRaster(rc3,paste(out.dir,species[i],"_final2/agreement_100_per_rcp45_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)

  rcp85_2035<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP85.+2035_threshold_cumulative_5",full.name=TRUE)
    s1<-stack(raster(rcp85_2035[1]),raster(rcp85_2035[2]),raster(rcp85_2035[3]),raster(rcp85_2035[4]),raster(rcp85_2035[5]),raster(rcp85_2035[6]),raster(rcp85_2035[7]))
    sum_rcp<- calc(s1, sum)
    writeRaster(sum_rcp,paste(out.dir,species[i],"_final2/sum_threshold_rcp85_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
    fun <- function(x) { x[x<7] <- 0; return(x) }
    rc2 <- calc(sum_rcp, fun)
    fun <- function(x) { x[x>=7] <- 1; return(x) }
    rc3 <- calc(rc2, fun)
    writeRaster(rc3,paste(out.dir,species[i],"_final2/agreement_100_per_rcp85_2035.asc",sep=""),NAflag=-9999,overwrite=TRUE)
   
  rcp45_2065<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP45.+2065_threshold_cumulative_5",full.name=TRUE)
    s1<-stack(raster(rcp45_2065[1]),raster(rcp45_2065[2]),raster(rcp45_2065[3]),raster(rcp45_2065[4]),raster(rcp45_2065[5]),raster(rcp45_2065[6]),raster(rcp45_2065[7]))
    sum_rcp<- calc(s1, sum)
      writeRaster(sum_rcp,paste(out.dir,species[i],"_final2/sum_threshold_rcp45_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
      fun <- function(x) { x[x<7] <- 0; return(x) }
      rc2 <- calc(sum_rcp, fun)
      fun <- function(x) { x[x>=7] <- 1; return(x) }
      rc3 <- calc(rc2, fun)
      writeRaster(rc3,paste(out.dir,species[i],"_final2/agreement_100_per_rcp45_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
      
  rcp85_2065<-list.files(paste(out.dir,species[i],"_final2/",sep=""),pattern="RCP85.+2065_threshold_cumulative_5",full.name=TRUE)
    s1<-stack(raster(rcp85_2065[1]),raster(rcp85_2065[2]),raster(rcp85_2065[3]),raster(rcp85_2065[4]),raster(rcp85_2065[5]),raster(rcp85_2065[6]),raster(rcp85_2065[7]))
    sum_rcp<- calc(s1, sum)
      writeRaster(sum_rcp,paste(out.dir,species[i],"_final2/sum_threshold_rcp85_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)
      fun <- function(x) { x[x<7] <- 0; return(x) }
      rc2 <- calc(sum_rcp, fun)
      fun <- function(x) { x[x>=7] <- 1; return(x) }
      rc3 <- calc(rc2, fun)
      writeRaster(rc3,paste(out.dir,species[i],"_final2/agreement_100_per_rcp85_2065.asc",sep=""),NAflag=-9999,overwrite=TRUE)

message(i)

}

