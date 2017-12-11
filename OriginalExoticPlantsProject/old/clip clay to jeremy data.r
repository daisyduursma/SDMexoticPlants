library(raster)

clay<-raster("C:\\Daisy\\Raw Data\\van der Wal data\\clay_5min2.asc")

bio1<-raster("E:\\future_5arcminute\\current\\bioclim\\bioclim_01.asc")

a<-extent(bio1)

clay2<-crop(clay,a)

writeRaster(clay2,"C:\\Daisy\\Raw Data\\van der Wal data\\clay_5min2.asc",overwrite=TRUE)
