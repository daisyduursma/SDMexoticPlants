
rm(list = ls())

dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\east_west_percent_suitable_habitat.csv")

locs<-as.vector(unique(dat$state))

for (i in 1:length(locs)){
  locdat<-subset(dat,state==locs[i])
  curwest<-subset(subset(locdat,year=="current"),presence_suitable==1)
  rcp2035<-subset(subset(locdat,year=="rcp85_2035"),presence_suitable==1)
  rcp2065<-subset(subset(locdat,year=="rcp85_2065"),presence_suitable==1)
  
}


b<-unique(c(cur$species,rcp2035$species,rcp2065$species))

c<-intersect(a,b)

reshape(dat,timevar="year",v.names="species",direction="wide")

write.csv(all,"C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\east_west_cur_species.csv",row.names=FALSE)