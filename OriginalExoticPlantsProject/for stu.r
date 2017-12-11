
#make sure workspace is clean
rm(list = ls())
#load library 
#library to work with shapefiles
library(maptools)
#library to work with rasters also allows for plotting of shapefiles
library(raster)


#map of ibra areas
ibra7<-readShapePoly(paste(grid.dir,"data_WGS_1984\\IBRA7_regions.shp",sep=""))



#view the attribute table

ibra7@data

#get a single column of data from attribute table

ibra7@data$REG_NAME_7
