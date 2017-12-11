Hello,

I am trying to optimize a process where the goal is to summarize raster files based on the cells inside shape files. I need to repeat the following process 45 times and currently each time I run the following example (although with much larger datasets and 292 raster files) it takes about 28 hours. Luckily I have access to multi-core Unix server but I think there must be a way to optimize this process. Here is a small axample of what I am doing


library(raster)

#5 example rasters
r1 <- raster(nrows=200, ncols=200, xmn=-180, xmx=120)
values(r1) <- runif(ncell(r1))
r2 <- raster(nrows=200, ncols=200, xmn=-180, xmx=120)
values(r2) <- runif(ncell(r2))
r3 <- raster(nrows=200, ncols=200, xmn=-180, xmx=120)
values(r3) <- runif(ncell(r3))
r4 <- raster(nrows=200, ncols=200, xmn=-180, xmx=120)
values(r4) <- runif(ncell(r4))
r5 <- raster(nrows=200, ncols=200, xmn=-180, xmx=120)
values(r5) <- runif(ncell(r5))


cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))

# plot(r5)
# plot(polys, add=TRUE)

