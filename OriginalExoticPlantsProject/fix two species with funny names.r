
# 1) Add Australia lat and long
# 2)remove rows with NA values in lat and long
# 3)randomly select one obs per grid cell
# 4)for each species plot observations to be used, 2 maps - global and Australia




rm(list = ls())

#load packages
library(raster)



#directorys
work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
dat.dir<-paste(work.dir,"data\\",sep="")

dat3<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value.csv",sep=""))

#get values that fall inside common land area
land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
wclim.dir<-"C:\\Daisy\\Raw Data\\Current Climate\\"
land_mask<-raster("C:\\Daisy\\Raw Data\\Koeppen\\kg_wc.asc")
r <- raster(paste(wclim.dir,"clay_5min2.asc",sep=""))
r<-expand(r, land_mask) 
r<-mask(r,land_mask)

locs<-dat3[,c("Longitude","Latitude")]
obs_swd<-extract(r,locs)
all_sp_dat<-cbind(dat3,obs_swd)
#remove observations with NA values
all_sp_dat<-na.omit(all_sp_dat)


agro_stol<-subset(all_sp_dat,species=="Agrostis stolonifer")

agro_stol$species<-rep("Agrostis stolonifera",nrow(agro_stol))

all_sp_dat2<-rbind(all_sp_dat,agro_stol)

all_sp_dat2<-subset(all_sp_dat2,species !="Agrostis stolonifer")



cic_in<-subset(all_sp_dat2,species=="Cichorium intybu")

cic_in$species<-rep("Cichorium intybus",nrow(cic_in))

all_sp_dat3<-rbind(all_sp_dat2,cic_in)

all_sp_dat3<-subset(all_sp_dat3,species !="Cichorium intybu")


write.csv(all_sp_dat3,(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_8km_cell_value.csv",sep="")))

