rm(list = ls())



.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("rJava")


#libraries
#library(RGraphics)
library(raster)
library(dismo)
#library(fields)



jar <-paste("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/dismo/java/maxent.jar")
if (file.exists(jar)) {}

work.dir<-"/data2/home/dduursma/exotic_plants/data/background images/"
out.dir<-paste(work.dir,"outputs/",sep="")
dir.create(paste(out.dir))


#background data
#db<-read.csv(paste(work.dir,"distance_buffers_SWD.csv",sep=""))
#kso<-read.csv(paste(work.dir,"Koeppen_Species_Observation_SWD.csv",sep=""))
#rg<-read.csv(paste(work.dir,"Random_Global_SWD.csv",sep=""))
#rk_density<-read.csv(paste(work.dir,"Random_Koeppen_SWD.csv",sep=""))
rk_10000<-read.csv(paste(work.dir,"RK_background_10000_2_10_12_8km_grid.csv",sep=""))
rk_40000<-read.csv(paste(work.dir,"RK_background_SWD_40000.csv",sep=""))



#sp_dat<-read.csv(paste(work.dir,"Observation_SWD.csv",sep=""))
sp_dat2<-read.csv("/data2/home/dduursma/exotic_plants/data/ observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv")
species<-as.vector(unique(rk_10000$species))[c(5,10,30,45,120,160,175)]


{

all_background<-rk_10000
for(spec in species){

#get occurance points for one species
remove_dat<-sp_dat$species==spec
occurence<-sp_dat[remove_dat,]
occurence$p<-rep(1,nrow(occurence))
occurence<-occurence[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2","p")]
#background points
remove_dat<-all_background$species==spec
background<-all_background[remove_dat,]
background$p<-rep(0,nrow(background))
background<-background[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2","p")]
#bind the dat so it is in form needed by maxent
env_dat<-rbind(background,occurence)
#list of environmental variables

ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15","clay_5min2")]
#ev<-env_dat[,c("bio_1","bio_5","bio_6","bio_12","bio_15")]
#list of 0 and 1 values to show if species is present or absent at location
pres<-as.vector(env_dat[,"p"])

#SEND TO MAXENT	

#path to write out files
new.out.dir<-(paste(out.dir,spec,"rk_density",sep=""))
#maxent runs

maxent(x=ev,p=pres,path=paste(new.out.dir),args=c("betamultiplier=1","nohinge","nothreshold","replicates=5","randomseed","projectionlayers=/data2/home/dduursma/exotic_plants/data/maxent.cache/"))

       
       
       min_files<-list.files(paste(new.out.dir),pattern="_min.asc",full.names = TRUE)
       max_files<-list.files(paste(new.out.dir),pattern="_max.asc",full.names = TRUE)
       med_files<-list.files(paste(new.out.dir),pattern="_median.asc",full.names = TRUE)
       zz1<-list.files(paste(new.out.dir),pattern="_avg.csv",full.names = TRUE)
       zz2<-list.files(paste(new.out.dir),pattern="_stdev.csv",full.names = TRUE)
       zz3<-list.files(paste(new.out.dir),pattern="_max.csv",full.names = TRUE)
       zz4<-list.files(paste(new.out.dir),pattern="_median.csv",full.names = TRUE)
       zz5<-list.files(paste(new.out.dir),pattern="_min.csv",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_1",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_2",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_3",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_4",full.names = TRUE)
       file.remove(zz5)	
       zz5<-list.files(paste(new.out.dir),pattern="_5",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_6",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_7",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_8",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_9",full.names = TRUE)
       file.remove(zz5)
       zz5<-list.files(paste(new.out.dir),pattern="_0",full.names = TRUE)
       file.remove(zz5)
       file.remove(min_files)
       file.remove(max_files)
       file.remove(med_files)
#        file.remove(zz1)
#        file.remove(zz2)
#        file.remove(zz3)
#        file.remove(zz4)

}




}

###################################################################################
#Get list of files
main<-list.files(out.dir,full.names=TRUE)[1:36]
avg_r<-list.files(main,full.names=TRUE,pattern="_maxent.cache_avg.asc")



#make australian ascii with 5% threshold applied
aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)

for (i in 1:length(main)){
sp<-strsplit(paste(main[i]),split="outputs//")[[1]][2]

#get average ascii, maxent results, threshold
avg_r<-raster(list.files(main[i],full.names=TRUE,pattern="_maxent.cache_avg.asc"))
result<-read.csv(paste(list.files(main[i],full.names=TRUE,pattern="maxentResults.csv")))
thresh<-subset(result,Species=="species (average)",select=Fixed.cumulative.value.5.logistic.threshold)[1,1]


#apply threshold value
fun <- function(x) { x[x<thresh] <- 0; return(x) }
rc2 <- calc(avg_r, fun)
fun <- function(x) { x[x>=thresh] <- 1; return(x) }
rc3 <- calc(rc2, fun)

aus_r<-crop(rc3,aus_extent)
writeRaster(aus_r,paste(work.dir,"thresholded_grids//aus_",sp,".asc",sep=""),overwrite=TRUE)

writeRaster(rc3,paste(work.dir,"thresholded_grids//global_",sp,".asc",sep=""),overwrite=TRUE)

}



       
       