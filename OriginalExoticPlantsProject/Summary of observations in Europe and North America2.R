
#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
#get directories where data located

work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\"
out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"

wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"


# 
r <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
r[]<-1
r<-aggregate(r,fact=4)

plot(r)


#extent of europe
x<-extent(-11,30,36,74)
plot(x,add=TRUE,col="red")
abline(h=36, v=30, col = "gray60")

am<-extent(-125,-75,20,50)
plot(am,add=TRUE,col="red")
#read in SWD files

sp_dat<- read.table(paste(work.dir,"observations_10_09_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")

species<-as.vector(unique(sp_dat$species))


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
  occurence$Europe<-point.in.polygon(x ,y ,c(-11,-11,30,30,-11),c(36,74,74,36,36))
  #get points in Europe
  eur<-subset(occurence,Europe>=1)
  #get non Europe points
  noneur<-subset(occurence,Europe==0)[1:24]
  Eur_obs<-nrow(eur)
  non_Eur_obs<-nrow(noneur)
  locs<-cbind(eur$Longitude,eur$Latitude)
  eur$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  eur$dup<-duplicated(eur$cell)
  #recombine the europe and not euopre observations
  subeur<-subset(eur,dup=="FALSE")[,1:24]
  subset_Eur_obs<-nrow(subeur)
  
  #find out if points fall in bounding box for North America  -125,-75,20,50
  occurence$NorthAmerica<-point.in.polygon(x ,y ,c(-125,-125,-75,-75,-125),c(20,50,50,20,20))
  #get points in Europe
  N_A<-subset(occurence,NorthAmerica>=1)
  #get non Europe points
  nonN_A<-subset(occurence,NorthAmerica==0)[1:24]
  N_A_obs<-nrow(N_A)
  non_N_A_obs<-nrow(nonN_A)
  locs<-cbind(N_A$Longitude,N_A$Latitude)
  N_A$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  N_A$dup<-duplicated(N_A$cell)
  #recombine the europe and not euopre observations
  subN_A<-subset(N_A,dup=="FALSE")[,1:24]
  subset_N_A_obs<-nrow(subN_A)
  all_obs<-nrow(occurence)
  
  
  ob<-cbind(species[i],all_obs,Eur_obs,non_Eur_obs,subset_Eur_obs,N_A_obs,non_N_A_obs,subset_N_A_obs)
  
  sub_obs[[i]]<-paste(ob)
  
}




Eur_NAmer_observation_summary<-do.call("rbind",sub_obs)
colnames(Eur_NAmer_observation_summary)<-c("species","all_obs","Eur_obs","non_Eur_obs","subset_Eur_obs","N_A_obs","non_N_A_obs","subset_N_A_obs")
  

write.csv(Eur_NAmer_observation_summary,paste(work.dir,"Eur_NAmer_observation_summary.csv",sep=""))


####################################################################################################





