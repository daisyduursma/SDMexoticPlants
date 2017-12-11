
#make sure workspace is clean
rm(list = ls())

library(raster)
library(maptools)
#get directories where data located
work.dir<-"c:\\daisy\\Current_Projects\\exotic_plants\\outputs\\all species\\"
out.dir<-"C:\\daisy\\Current_Projects\\exotic_plants\\paper\\NCCARF Reports\\tables_figures\\"
bioregions<-states<-readShapePoly("f:\\daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\Bioregions.shp")
