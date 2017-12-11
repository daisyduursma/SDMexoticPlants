#Clean the species data that comes from GBIF, remove duplicate data, make new column with grid cell number of global 5 arc minute map.


rm(list = ls())


library(dismo)
library(raster)


dat.dir<-"C:\\Daisy\\Current Projects\\Grass Paper\\species data\\"

#load data

dat1<-read.csv(paste(dat.dir,"grasses_observations.csv",sep=""))

############### remove records which are duplicates 
	dups <- duplicated(dat1)
	# keep the records that are _not_ duplicate
	dat2 <- dat1[!dups,]

	
############### subsample so there is one observation per BioClim gridcell (5 minute or )


# create a RasterLayer with the extent from worldClim data
r<-raster(nrows=1800, ncols=4320, xmn=-180, xmx=180.000018775, ymn=-60, ymx=90.0000078231, crs="+proj=longlat +datum=WGS84")
	#asign unique value to each cell
r[]=1:ncell(r)


# get the cell number for each point
loc<-dat2[c("Longitude","Latitude")]
cell <- cellFromXY(r, loc)

dat3<-cbind(dat2,cell)
