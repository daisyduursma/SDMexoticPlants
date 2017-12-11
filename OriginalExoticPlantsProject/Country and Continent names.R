#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
library(maptools)
#get directories where data located

work.dir<-"C:\\Users\\dduursma\\Google Drive\\weeds\\"
#out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"

wclim.dir<-"f:\\daisy\\Raw Data\\Current Climate\\"


# work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\data\\"
# out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\"
# wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"


#map of countries
countries<-readShapePoly("C:\\Daisy\\trash\\Countries.shp")

#read in SWD files
sp_dat<- read.table(paste(work.dir,"more_than_30_obs_final_obs_OnePerCell_17102013.csv", sep=""),header=TRUE, sep=",")

#make something to divide up the data
sep<-rep(c(1:174),each=2000)
sp_dat$sep<-sep[1:nrow(sp_dat)]

separator<-as.vector(unique(sp_dat$sep))

  #make empty list, there is to many observations to do this all at once
country <- list()


#for each specis
for (i in 1:length(separator)){
  
  #get occurance points for one separator 
  
  #get occurance points for one species
  remove_dat<-sp_dat$sep==separator[i]
  sep_dat<-sp_dat[remove_dat,]
  occurence<-sep_dat
   
  
  #make spatial point dataframe and find out countries observations occur in
  coordinates(occurence)<-~lon+lat
  class(countries)
  ov<-overlay(occurence,countries)
  #extract the country name
  sep_dat$cntr<-as.character(countries@data$COUNTRY[ov])
  sep_dat$cont<-as.character(countries@data$CONTINENT[ov])
  #add to list
  country[[i]]<-sep_dat

  #return the species number, so progress can be followed
  message(i)
} 


#combine lists to a dataframes
sp_dat2<-do.call("rbind",country)


#write out dataframe with the two new columns
write.csv(sp_dat2,paste(work.dir,"more_than_30_obs_final_obs_OnePerCell_17102013_with_country.csv", sep=""))





# 
# 
# ###################################
# 
# #% observations per 1000km2 per country
# sp_dat<-read.csv(paste(work.dir,"old\\observations_25_09_12_8km_grid_all_worldclimvar.csv",sep=""))
# 
# dd<-as.data.frame(table(sp_dat$cntr))
# 
# 
# 
# dd[order(-dd$Freq,dd$Var1),]
# 
# 
# ee<-aggregate(Shape_Area ~ COUNTRY,data=aa, FUN="sum")
# 
# ff<-merge(dd,ee, by.x="Var1",by.y="COUNTRY")
# ff$obs_per_area<-round(ff$Freq/ff$Shape_Area,digits=4)
# 
# gg<-ff[order(-ff$obs_per_area),]
# 
# write.csv(gg,paste(work.dir,"country_observations_summary_27_09_2012.csv", sep=""))
# 
