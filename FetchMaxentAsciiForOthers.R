#move only the files that are needed to a new directory

rm(list = ls())

#get directories
  #in.dir<-'/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants2/outputs'
  in.dir<-'/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants/outputs'
  out.dir<- '/Users/daisy/Google Drive/OtherPeople/MattSheehan/'

#Xanthium strumarium is Xanthium occidentale
#Brachiaria mutica is Urochloa mutica
species<-c("Acanthocereus tetragonus","Hylocereus undatus")
          #  "Cylindropuntia fulgida var. mamillata",
          #  "Cylindropuntia imbricata",
          #  "Harrisia martinii",
          # "Hylocereus undatus",
          #  "Opuntia aurantiaca",
          #  "Opuntia elata",
          #  "Opuntia engelmannii",
          #  'Opuntia ficus-indica',
          #  "Opuntia humifusa",
          #  "Opuntia monacantha",
          #  "Opuntia puberula",
          #  "Opuntia robusta",
          #  "Opuntia stricta",
          #  "Opuntia tomentosa" )


for(i in 1:length(species)){
  #make a new output directory
  dir.create(paste0(out.dir,'/',species[i]))
  
  #get list of files
  aus<-list.files(paste0(in.dir,'/',species,"_final2")
,pattern="Australia",full.names=TRUE)
  pics<-list.files(paste0(in.dir,'/',species[i],"_final2")
                 ,pattern=".jpg",full.names=TRUE)
  #move ascii
  for (ii in 1:length(aus)){
    file.copy(aus[ii], paste0(out.dir,'/',species[i],"/",strsplit(aus[ii],"/")[[1]][10]),overwrite=TRUE)
  }
  
  #move jpg
  for (j in 1:length(pics)){
    file.copy(pics[j], paste0(out.dir,'/',species[i],"/",strsplit(pics[j],"/")[[1]][10]),overwrite=TRUE)
  }

  #   a<-raster(paste0(in.dir,'/',species[i],"/Australia_current.asc"))
  #   plot(a)
  #   b<-raster(paste0(in.dir,'/',species[i],"/Australia_current_thresholded_10.asc"))
  #   plot(b)
  #   c<-raster(paste0(in.dir,'/',species[i],"/Australia_current_10.asc"))
  #   plot(c)

}




#############
#get all plant unthresholded ascii files for Jess O'donell
###########

rm(list = ls())

#get directories
in.dir1<-'/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants_2/outputs'
in.dir2<-'/Volumes/Seagate Expansion Drive/Seagate/Daisy/Current_Projects/exotic_plants/outputs'
out.dir<- '/Users/daisy/Google Drive/OtherPeople/JessODonnell/weedDistributions'
dir.create(out.dir)


 #get list of folders
  f1<-list.files(in.dir2,pattern="_final2")
  
  for(i in 2:length(f1)){
    #make a new output directory
    sp<-strsplit(f1[i],"_final2")[[1]][1]
    dir.create(paste0(out.dir,'/',gsub(" ","_",sp)))
    #get list of files
    dat<-list.files(paste0(in.dir2,'/',f1[i])
                    ,pattern="Australia_species_current_avg.asc",full.names=TRUE)

    file.copy(dat, paste0(out.dir,'/',gsub(" ","_",sp),'/'),overwrite=TRUE)
}


  #get list of folders
f2<-list.files(in.dir1)[2:length(list.files(in.dir1))]
  
  for(ii in 1:length(f2)){
    #make a new output directory
    dir.create(paste0(out.dir,'/',gsub(" ","_",f2[ii])))
    #get list of files
    dat<-list.files(paste0(in.dir1,'/',f2[ii])
                    ,pattern="Australia_current.asc",full.names=TRUE)
    
    file.copy(dat, paste0(out.dir,'/',gsub(" ","_",f2[ii]),'/'),overwrite=TRUE)
}
  
