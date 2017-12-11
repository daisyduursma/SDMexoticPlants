#find out full dispersal, verses not dispersal

rm(list = ls())

r <- require(raster)

out.dir<-"D:\\Current_Projects\\exotic_plants\\outputs\\"

sp_files<-list.files(out.dir, pattern="final2",full.name=TRUE)

a<-area(raster(paste0(sp_files[1],"\\Australia_species_current_avg_thresholded_10.asc")))

dat<-list()
for( i in 1:length(sp_files)){
  
  cur<-raster(paste0(sp_files[i],"\\Australia_species_current_avg_thresholded_10.asc"))
  f_35<-raster(paste0(sp_files[i],"\\Australia_average_rcp85_2035_thresholded_10.asc"))
  f_65<-raster(paste0(sp_files[i],"\\Australia_average_rcp85_2065_thresholded_10.asc"))
  
  no_disp_35<-cellStats((f_35*cur*a),stat="sum")
  no_disp_65<-cellStats((f_65*cur*a),stat="sum")
  cur<-cellStats((cur*a),stat="sum")
  full_disp_35<-cellStats((f_35*a),stat="sum")
  full_disp_65<-cellStats((f_65*a),stat="sum")
  
  sp<-strsplit(strsplit(sp_files[i],"\\\\")[[1]][5],"_")[[1]][1]
  
  dat[[i]]<-cbind (sp,cur,full_disp_35,full_disp_65, no_disp_35,no_disp_65)
message(i)  
}

al_dat<-do.call("rbind",dat)
write.csv(al_dat,"D:\\Current_Projects\\exotic_plants\\paper\\manuscripts\\hotspots\\dispersal.csv")





#######################################


rm(list = ls())

r <- require(raster)


obs<-read.csv("D:\\Current_Projects\\exotic_plants\\data\\observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv")


#raster of hotspot under current
cur<-raster("D:\\Current_Projects\\exotic_plants\\outputs\\all species\\AUS_thresholded_rcp85_2065_10_sum_all_species.asc")

 fun <- function(x) { x[x<85] <- 0; return(x) }
    rc2 <- calc(cur, fun)
    fun <- function(x) { x[x>=85] <- 1; return(x) }
    rc3 <- calc(rc2, fun)

locs<-SpatialPoints(cbind(obs$Longitude,obs$Latitude))
obs$occ<-extract(rc3,locs)
obs<-subset(obs,occ==1)

cur_sp<-length(unique(obs$species))


