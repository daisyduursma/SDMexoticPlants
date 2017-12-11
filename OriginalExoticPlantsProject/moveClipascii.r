

#make sure workspace is clean
	rm(list = ls())
					
library(raster)

#get directories where data located
	work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\"
	out.dir<-paste0(work.dir,"outputs\\")
	keep.dir<-paste0(work.dir,"AsciiForOthers\\")
	
#list of the folders
  all_files<-list.files(paste(out.dir),full.names = TRUE,pattern="final2")

#list of the files to move  
  t_files<-c("Australia_average_rcp85_2065.asc","Australia_average_rcp85_2065_thresholded_10.asc")
  

#move thresholded ascii's
for(i in 1:length(all_files)){
  #get species name
  sp_name<-strsplit(strsplit(all_files[i],"\\\\")[[1]][[6]],"_final2")[[1]][1]
  #make new folder
  sp.dir<-paste0(keep.dir,sp_name,"\\")
  dir.create(sp.dir,recursive=TRUE)
  
  #move files
  for(ii in 1: length(t_files)){
    file.copy(paste0(all_files[i],"\\",t_files[ii]), paste0(sp.dir,t_files[ii]),overwrite=TRUE)
    }
}
  
# 
# #clip continuous ascii's 
# #get extent and mask  
# aus_extent<-extent(112.9167,153.5833,-44,-9.333333)
# aus_mask<-raster("d:\\Raw Data\\Australia masks\\Australia.asc")
# aus_mask<-crop(aus_mask,aus_extent)
# 
#   
#   #list of the files to clip and move
#   c_files<-c("average_rcp45_2035.asc","species_current_avg.asc","average_rcp45_2065.asc","average_rcp85_2035.asc","average_rcp85_2065.asc")
# 
#  for(i in 1:length(all_files)){
#   #get species name
#   sp_name<-strsplit(strsplit(all_files[i],"\\\\")[[1]][[6]],"_final2")[[1]][1]
#   #make new folder
#   sp.dir<-paste0(keep.dir,sp_name,"\\")
# 
#   
#   #clip files and write in new folder
#   for(ii in 1: length(c_files)){
#     a<-raster(paste0(all_files[i],"\\",c_files[ii]))
#     rr1<-crop(a,aus_extent)
#     rr1<-mask(rr1,aus_mask)
#     writeRaster(rr1,paste0(all_files[i],"\\Australia_",c_files[ii]),overwrite=TRUE)
#     file.copy(paste0(all_files[i],"\\Australia_",c_files[ii]), paste0(sp.dir,"Australia_",c_files[ii]),overwrite=TRUE)
#    
#   }
#   message(i)
# }
#   
#   #remove non-current
#   
#   r_files<-c("Australia_average_rcp45_2035.asc","Australia_average_rcp45_2065.asc","Australia_average_rcp85_2035.asc","Australia_average_rcp85_2065.asc","thresholded_rcp45_2035_10.asc","thresholded_rcp45_2065_10.asc","thresholded_rcp85_2035_10.asc","thresholded_rcp85_2065_10.asc")
#   
#    
#   #clip files and write in new folder
#   for(ii in 1: length(r_files)){
#   
#     dat<-list.files(keep.dir,pattern=paste(r_files[ii]),recursive=TRUE,full.names=TRUE)
#     
#     file.remove(dat)
#    
#   }
#  
#  
#   
#   