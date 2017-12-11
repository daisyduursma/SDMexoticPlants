


#make sure workspace is clean
rm(list = ls())

library(raster)

#list of polygon files ($ means that it is the end of a string)
grids<-unlist(strsplit(list.files("c:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\",pattern=".shp$"),".shp"))

aus_mask<-raster("d:\\Raw Data\\Australia masks\\Australia.asc")
aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
#crop mask to extent of Austrlia
aus_mask<-crop(aus_mask,aus_extent)
#aus_mask<-disaggregate(aus_mask,fact=3)


#loop to make summary tables for polygons
for (i in 1:length(grids)){
  message("Creating table for ",grids[i])
     try(makePolygonTable(i,
                          grid.dir<-"c:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\",
                          out.dir<-"c:\\Current_Projects\\exotic_plants\\outputs\\Land_Catagories\\",
                          #landmask of Australia
                          aus_mask<-aus_mask
                          
     ))
  
    }


#function that needs to be read in
makePolygonTable<- function (i, grid.dir=grid.dir, out.dir=out.dir,aus_mask=aus_mask){
  
  r <- require(raster)
  if(!r)stop("Install raster")
  r <- require(sp)
  if(!r)stop("Install sp")
  r <- require(maptools)
  if(!r)stop("Install maptools")
  
  #mask and extent of Australia
  
  #get points for each cell (SpatialPointsDataFrame),Cells with NA are not converted
  locs<-rasterToPoints(aus_mask,spatial=TRUE)
  #normal data.frame of locs
  locs2<-as.data.frame(locs)
  
  #read in polygon file
  aa<-readShapePoly(paste(grid.dir,grids[i],".shp",sep=""))
  dat1<-over(locs,aa)
  dat2<-cbind(locs2,dat1)
  #determine rows in dat1 where all columns in a row have NA values, these will be removed
  dat2<-cbind(dat2,all_na<-apply(dat1,1,function(x)any(!is.na(x))))
  PolygonTable<-subset(dat2,all_na <- apply(dat1, 1, function(x) any(!is.na(x)))==TRUE)
  
  #################ASK REMKO HOW TO MAKE THE TABLE AN OBJECT IN MY WORKING ENVIRONMENT############## 
  #write output table
  write.csv(PolygonTable,paste(out.dir,grids[i],"_point_polygon_summary.csv"))
  
  
}

