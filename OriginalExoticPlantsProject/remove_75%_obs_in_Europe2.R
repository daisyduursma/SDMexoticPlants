
#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
library(maptools)
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


#map of countries and get country size

countries<-readShapePoly("C:\\Daisy\\trash\\Countries.shp")
aa<-countries@data
aa<-aggregate(Shape_Area ~ COUNTRY,data=aa, FUN="sum")

#extent of europe
#x<-extent(-10,35,34,57)


#read in SWD files

sp_dat<- read.table(paste(work.dir,"observations_27_09_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")



#make table of observations and calculate density of obs per country
obs<-as.data.frame(table(sp_dat$cntr))
obs<-merge(obs,aa, by.x="Var1",by.y="COUNTRY")
obs$obs_per_area<-round(obs$Freq/obs$Shape_Area,digits=4)
obs<-subset(obs,Freq>0)

obs<-obs[order(-obs$obs_per_area),]

#above 500 obs per area
sub1<=c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden")
sub2
sub3



sub_obs <- list()

for (i in 1:length(species)){
  
  #get occurance points for one species
  remove_dat<-sp_dat$species==species[i]
  occurence<-sp_dat[remove_dat,]
#   occurence$p<-rep(1,nrow(occurence))
#   #get xy locations
#   x<-occurence$Longitude
#   y<-occurence$Latitude
  
#make table of observations and calculate density of obs per country
 obs<-as.data.frame(table(occurence$cntr))
  obs<-merge(cntr_obs,aa, by.x="Var1",by.y="COUNTRY")
  obs$obs_per_area<-round(obs$Freq/obs$Shape_Area,digits=4)
  obs<-subset(obs,Freq>0)
  
  obs<-obs[order(-obs$obs_per_area),]
  
  
  
  #make spatial point dataframe
  coordinates(occurence)<-~Longitude+Latitude
  class(wrld_simple)
  ov<-overlay(occurence,wrld_simpl)
  cntr<-as.character(wrld_simpl@data$NAME[ov])
  i<-is.na(cntr)
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
