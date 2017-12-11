
#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
library(maptools)
#get directories where data located


work.dir<-"C:\\Users\\dduursma\\Google Drive\\weeds\\"
#out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"
wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"

r <- raster("c:\\daisy\\Raw Data\\Koeppen\\KG_masked")
r[]<-1
r<-aggregate(r,fact=4)

#get species rerun data
sum_dat<-read.csv(paste(work.dir,"Eur_USA_observation_summary_21_10_13.csv",sep=""))
#get the observation data
all_dat<- read.table(paste(work.dir,"more_than_30_obs_final_obs_OnePerCell_17102013_with_country.csv", sep=""),header=TRUE, sep=",")

#list of all species
spec<-as.vector(unique(all_dat$species))
#get species name for each group from subsetting
# 
# sub_dat<-subset(sum_dat,eur_sub1_per>=50)
# eur_1<-as.vector(subset(sub_dat,all_obs>=500,select="species"))
# eur_1<-as.vector(eur_1$species)

sub_dat<-subset(sum_dat,eur_sub2_per>=50)
eur_2<-(subset(sub_dat,all_obs>=500,select="species"))
eur_2<-as.vector(eur_2$species)

sub_dat<-subset(sum_dat,eur_sub3_per>=50)
eur_3<-(subset(sub_dat,all_obs>=500,select="species"))
eur_3<-as.vector(eur_3$species)

sub_dat<-subset(sum_dat,USA_sub1_per>=50)
usa_1<-(subset(sub_dat,all_obs>=500,select="species"))
usa_1<-as.vector(usa_1$species)


#Countries in different groups, groups defined as:
#     #above 500 obs per area across all species and places with NA observations
# sub1<-c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden")
#     #above 200 obs per area across all species and places with NA observations
sub2=c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden","Finland", "Norway","Greece")
    #Continent of Europe
sub3= "Europe"
    #Counrty of USA
sub_usa_1="United States"


###################
#for species that need to be subset based on sub2 of Europe
  sub_all_dat<-subset(all_dat,species %in% paste(eur_2,sep=""))
#observations occuring in area of interset
  sub_all_dat2<-subset(sub_all_dat,cntr %in% paste(sub2,sep="") | is.na(cntr) )
#species and obs not of interest
  non_id<-setdiff(as.vector(all_dat$X),as.vector(sub_all_dat2$X))
  non_sub_all_dat<-subset(all_dat,X %in% paste(non_id,sep=""))[,1:29]

for (i in 1:length(eur_2)){
  
  sp_dat<-subset(sub_all_dat2,species==eur_2[i])
  #find out cell value (resolution  : 0.3333334, 0.3333334)
  locs<-cbind(sp_dat$Longitude,sp_dat$Latitude)
  sp_dat$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  sp_dat$dup<-duplicated(sp_dat$cell)
  #recombine the europe and not europew observations
  subeur<-subset(sp_dat,dup=="FALSE")[,1:29]
  non_sub_all_dat<-rbind(subeur,non_sub_all_dat)
 
  message(i)
 }
  eur_clean<-non_sub_all_dat
#for species that need to be subset based on USA
sub_all_dat<-subset(eur_clean,species %in% paste(usa_1,sep=""))
#observations occuring in area of interset
sub_all_dat2<-subset(sub_all_dat,cntr %in% paste(sub_usa_1,sep="") | is.na(cntr) )
#species and obs not of interest
non_id<-setdiff(as.vector(eur_clean$X),as.vector(sub_all_dat2$X))
non_sub_all_dat_usa<-subset(eur_clean,X %in% paste(non_id,sep=""))[,1:29]

for (ii in 1:length(usa_1)){
  
  sp_dat_usa<-subset(sub_all_dat2,species==usa_1[ii])
  #find out cell value (resolution  : 0.3333334, 0.3333334)
  locs<-cbind(sp_dat_usa$Longitude,sp_dat_usa$Latitude)
  sp_dat_usa$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  sp_dat_usa$dup<-duplicated(sp_dat_usa$cell)
  #recombine the europe and not europew observations
  subusa<-subset(sp_dat_usa,dup=="FALSE")[,1:29]
  non_sub_all_dat_usa<-rbind(subusa,non_sub_all_dat_usa)
  
  message(ii)
}

eur_usa_sub<-non_sub_all_dat_usa[,5:27]


write.csv(eur_usa_sub,paste(work.dir,"observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv",sep=))

  



































#join data to list
  sub_obs[[i]] <-  occur
  
  #######   non: observations occuring in area of interset
  
  #get subset of obs
  dat<-subset(sp_dat,cntr %in% get(paste("sub",i,sep="")) | is.na(cntr) )
  
  #   #non-subset obs
  #   #######################non_dat<-subset(sp_dat,cntr %in% get(paste("sub",i,sep="")) | is.na(cntr) )
  
  
  
  for(i in 1:length(spec)){}
  
  sp_dat<-subset(all_dat,species==spec[i])
  

  
  
}

# 
# #find out if points fall in countries of Europe or are NA
# eur_obs_sub2_dat<-subset(eur,cntr %in% paste(sub2) | is.na(cntr) )

# #find out if points fall in countries of Europe or are NA
# eur_obs_sub1_dat<-subset(eur,cntr %in% paste(sub1) | is.na(cntr) )


# 
# 
# #map of countries and get country size
# 
# countries<-readShapePoly("C:\\Daisy\\trash\\Countries.shp")
# aa<-countries@data
# aa<-aggregate(Shape_Area ~ COUNTRY,data=aa, FUN="sum")
# 
#extent of europe
#x<-extent(-10,35,34,57)


#read in SWD files

sp_dat<- read.table(paste(work.dir,"observations_2_10_12_8km_grid_all_worldclimvar.csv", sep=""),header=TRUE, sep=",")



#make table of observations and calculate density of obs per country
obs<-as.data.frame(table(sp_dat$cntr))
obs<-merge(obs,aa, by.x="Var1",by.y="COUNTRY")
obs$obs_per_area<-round(obs$Freq/obs$Shape_Area,digits=4)
obs<-subset(obs,Freq>0)

obs<-obs[order(-obs$obs_per_area),]



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
  
  
  
  
  
  
  
  
  
  
  
  
  
