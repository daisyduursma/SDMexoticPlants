
#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
#get directories where data located

work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"
wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"


#get the species to rerun 

sum_dat<-read.csv(paste(work.dir,"Eur_NAmer_observation_summary.csv",sep=""))
#more or equal to 50% obs occur in Europe
sum_dat<-subset(sum_dat,X._of_obs_Eur>=50)
#has more than 500 obs 
sum_dat<-subset(sum_dat,Eur_obs>=500)

species<-as.vector(sum_dat$species)



# 
r <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
r[]<-1
r<-aggregate(r,fact=4)


#extent of europe
#x<-extent(-10,35,34,57)


#read in SWD files

sp_dat<- read.table(paste(work.dir,"observations_10_09_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")
all_background<-read.table(paste(work.dir,"RK_background_10000_10_09_12_8km_grid.csv", sep=""),header=TRUE, sep=",")
#make list of species
#species <-as.vector(unique(sp_dat$species))

sub_obs <- list()

for (i in 1:length(species)){
  
  #get occurance points for one species
  remove_dat<-sp_dat$species==species[i]
  occurence<-sp_dat[remove_dat,]
  occurence$p<-rep(1,nrow(occurence))
  #get xy locations
  x<-occurence$Longitude
  y<-occurence$Latitude
  #find out if points fall in bounding box for Europe
  occurence$Europe<-point.in.polygon(x ,y ,c(-10,-10,35,35,-10),c(34,74,74,34,34))
  #get points in Europe
  eur<-subset(occurence,Europe>=1)
  #get non Europe points
  noneur<-subset(occurence,Europe==0)[1:24]
  
#   #if making a map
#   #show all points
#   points(occurence$Longitude,occurence$Latitude,cex=1,col="red")
#   points(eur$Longitude,eur$Latitude,cex=1,col="black")
# 
  #find out cell value (resolution  : 0.3333334, 0.3333334)
  locs<-cbind(eur$Longitude,eur$Latitude)
  eur$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  eur$dup<-duplicated(eur$cell)
  #recombine the europe and not europew observations
  subeur<-subset(eur,dup=="FALSE")[,1:24]
  occur<-rbind(subeur,noneur)
  
#   points(occur$Longitude,occur$Latitude,cex=1,col="purple")
#   points(subeur$Longitude,subeur$Latitude,cex=1,col="blue")
#   
  
  
  
  
  #join data to list
  sub_obs[[i]] <-  occur
  
}



#combine lists to a dataframes
sub_obs_SWD<-do.call("rbind",sub_obs)
# observation_SWD<-do.call("rbind",obs_SWD)
# observation_summary<-do.call("rbind",obs_SWD)

#write out SWD file for background points
write.csv(sub_obs_SWD,row.names=FALSE,file=paste(work.dir,"sub_Europe_observations_500obsminimum_50perofallobs_19_09_2012.csv",sep=""))
  
  
  
  
  
  
  
  
  
  
  
  
  
