
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

sp_dat<- read.table(paste(work.dir,"observations_27_09_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")

species<-as.vector(unique(sp_dat$species))


sub_obs <- list()

for (i in 1:length(species)){
  
  #get occurance points for one species
  remove_dat<-sp_dat$species==species[i]
  occurence<-sp_dat[remove_dat,]
  all_obs<-nrow(occurence)
  #get xy locations
  x<-occurence$Longitude
  y<-occurence$Latitude
  
  #find out if points fall in bounding box for Europe
  occurence$Europe<-point.in.polygon(x ,y ,c(-11,-11,30,30,-11),c(36,74,74,36,36))
  #get points in Europe
  eur<-subset(occurence,Europe>=1)
  #get non Europe points
  noneur<-subset(occurence,Europe==0)[1:24]
  Eur_obs_box<-nrow(eur)
  non_Eur_obs_box<-nrow(noneur)
  locs<-cbind(eur$Longitude,eur$Latitude)
  eur$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  eur$dup<-duplicated(eur$cell)
  #recombine the europe and not euopre observations
  subeur<-subset(eur,dup=="FALSE")[,1:24]
  subset_Eur_obs_box<-nrow(subeur)
  
  #method 2, seperate by country
  
  #above 500 obs per area across all species and places with NA observations
  sub1<-c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden")
  
  #find out if points fall in countries of Europe or are NA
  eur_obs_sub1_dat<-subset(eur,cntr %in% paste(sub1) | is.na(cntr) )
  
  #get non Europe points
  
  non_eur_x<-setdiff(occurence$X,eur_obs_sub1_dat$X)
  noneur_sub1_dat<-subset(occurence,X %in% paste(non_eur_x))
  Eur_obs_sub1<-nrow( eur_obs_sub1_dat)
  non_Eur_obs_sub1<-nrow(noneur_sub1_dat)
  #locs<-cbind(eur$Longitude,eur$Latitude)
  #eur$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  #eur$dup<-duplicated(eur$cell)
  #recombine the europe and not euopre observations
  #subeur<-subset(eur,dup=="FALSE")[,1:24]
  #subset_Eur_obs_box<-nrow(subeur)
  
  
  #above 200 obs per area across all species and places with NA observations
  sub2=c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden","Finland", "Norway","Greece")

  
  #find out if points fall in countries of Europe or are NA
  eur_obs_sub2_dat<-subset(eur,cntr %in% paste(sub2) | is.na(cntr) )
  
  #get non Europe points
  
  non_eur_x2<-setdiff(occurence$X,eur_obs_sub2_dat$X)
  noneur_sub2_dat<-subset(occurence,X %in% paste(non_eur_x2))
  Eur_obs_sub2<-nrow( eur_obs_sub2_dat)
  non_Eur_obs_sub2<-nrow(noneur_sub2_dat)
  
  #Continent of Europe
  sub3= "Europe"
  
  #find out if points fall in countries of Europe or are NA
  eur_obs_sub3_dat<-subset(occurence,cont %in% paste(sub3) | is.na(cont) )
  
  #get non Europe points
  
  non_eur_x3<-setdiff(occurence$X,eur_obs_sub3_dat$X)
  noneur_sub3_dat<-subset(occurence,X %in% paste(non_eur_x3))
  Eur_obs_sub3<-nrow( eur_obs_sub3_dat)
  non_Eur_obs_sub3<-nrow(noneur_sub3_dat)
  
  
  
  
  ###############################################
  
#USA OBSERVATION'
  
  #find out if points fall in bounding box for usa
  occurence$USA<-point.in.polygon(x ,y ,c(-130,-130,-65,-65,-130),c(20,50,50,20,20))
  
  #get points in usa
  usa<-subset(occurence,USA>=1)
  #get non usa points
  nonusa<-subset(occurence,USA==0)
  USA_obs_box<-nrow(usa)
  non_USA_obs_box<-nrow(nonusa)
#   locs<-cbind(usa$Longitude,usa$Latitude)
#   eur$cell<-cellFromXY(r,locs)
#   #remove duplicate cell observations
#   eur$dup<-duplicated(eur$cell)
#   #recombine the europe and not euopre observations
#   subeur<-subset(eur,dup=="FALSE")[,1:24]
#   subset_Eur_obs_box<-nrow(subeur)
  
#usa as cntr
  
  #find out if points fall in countries of USA or are NA
  usa_obs_sub2_dat<-subset(usa,cntr =="United States" | is.na(cntr) )
  
  #get non Europe points
  
  non_usa_x2<-setdiff(occurence$X,usa_obs_sub2_dat$X)
  nonusa_sub2_dat<-subset(occurence,X %in% paste(non_usa_x2))
  USA_obs_sub2<-nrow( usa_obs_sub2_dat)
  non_USA_obs_sub2<-nrow(nonusa_sub2_dat)
  

  
  
  
  
  
  
  
  
  
    ob<-cbind(species[i],all_obs,Eur_obs_box,non_Eur_obs_box,Eur_obs_sub1,non_Eur_obs_sub1,Eur_obs_sub2,non_Eur_obs_sub2,Eur_obs_sub3,non_Eur_obs_sub3, USA_obs_box,non_USA_obs_box,USA_obs_sub2,non_USA_obs_sub2)
  
  sub_obs[[i]]<-paste(ob)
  
}




Eur_USA_observation_summary<-do.call("rbind",sub_obs)
colnames(Eur_USA_observation_summary)<-c("species","all_obs","Eur_obs_box","non_Eur_obs_box","Eur_obs_sub1","non_Eur_obs_sub1","Eur_obs_sub2","non_Eur_obs_sub2","Eur_obs_sub3","non_Eur_obs_sub3","USA_obs_box","non_USA_obs_box","USA_obs_sub2","non_USA_obs_sub2")
  

write.csv(Eur_USA_observation_summary,paste(work.dir,"Eur_USA_observation_summary_2_10_12.csv",sep=""))


####################################################################################################





