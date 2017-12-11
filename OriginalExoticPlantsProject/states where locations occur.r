

#make sure workspace is clean
rm(list = ls())

#list of polygon files ($ means that it is the end of a string)
grids<-unlist(strsplit(list.files("f:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\",pattern=".shp$"),".shp"))
states<-readShapePoly("f:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\Australian_States.shp")


work.dir<-"f:\\Current_Projects\\exotic_plants\\"
sp_dat<- read.table(paste(work.dir,"data\\", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")

#locs<-sp_dat[,c("Longitude","Latitude")]
locs<-SpatialPoints(cbind(sp_dat$Longitude,sp_dat$Latitude))
#locs$state<-overlay(locs,states)
dat1<-over(locs,states)

dat2<-na.omit(cbind(dat1,sp_dat))

dat3<-dat2[,c("STATE","species")]

dat3$dups<-duplicated(dat3)

sp_state<-subset(dat3,dups==FALSE)

write.csv (sp_state,"f:\\Current_Projects\\exotic_plants\\data\\old\\states with species observations.csv")

#keep only unique



?

