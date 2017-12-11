
#make sure workspace is clean
rm(list = ls())
#load library 


.libPaths("/data1/dduursma/R/x86_64-redhat-linux-gnu-library/")
#install.packages("dismo")


#library(dismo)
#get directories where data located
work.dir<-"/data2/home/dduursma/exotic plants/"
out.dir<-paste(work.dir,"/outputs/",sep="")


#read in observation SWD file
sp_dat<- read.table(paste(work.dir,"data/", " observations_8_10_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
species <-as.vector(unique(sp_dat$species))


#for each species
for (i in 1:length(species)){
#   
#   file.copy(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Max_training.asc",sep=""), paste(out.dir,"Threshold Maps/",species[i],"_current_threshold_sub_Europe_Max_training.asc",sep=""), overwrite = TRUE)
#    
# file.copy(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Equal_training.asc",sep=""), paste(out.dir,"Threshold Maps/",species[i],"_current_threshold_sub_Europe_Equal_training.asc",sep=""), overwrite = TRUE)
# 
# file.copy(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_Balance_training.asc",sep=""), paste(out.dir,"Threshold Maps/",species[i],"_current_threshold_sub_Europe_Balance_training.asc",sep=""), overwrite = TRUE)
#   
#   file.copy(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_10_percentile.asc",sep=""), paste(out.dir,"Threshold Maps/",species[i],"_current_threshold_sub_Europe_10_percentile.asc",sep=""), overwrite = TRUE)
#   

  file.copy(paste(out.dir,species[i],"_set1_bgRK_r1.0_nhnt_sub_Europe/",species[i],"_current_threshold_sub_Europe_10_percentile.asc",sep=""), paste(out.dir,"Threshold Maps2/",species[i],"_current_threshold_sub_Europe_cumulative_5.asc",sep=""), overwrite = TRUE)
  
  
  
  message(i)
  
}







###############remove files

a<-list.files(paste(out.dir),recursive=TRUE,full.names=TRUE)


file.remove(paste(a))




