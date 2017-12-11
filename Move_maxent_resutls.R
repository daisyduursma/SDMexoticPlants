#move only the files that are needed to a new directory

rm(list = ls())

#get directories
  in.dir<-"D:\\Current_Projects\\exotic_plants_2\\outputs\\"
  out.dir<- "C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\"

#get the summarized maxent outfiles
maxent<-read.csv(paste0(in.dir,"Summary_Maxent_19_11_13.csv"))


#get the species with good AUC and binomial probability
species<- subset(maxent, X10.percentile.training.presence.binomial.probability < 0.05,select=species,drop=TRUE)
species<- droplevels(species)

for (i in 1:length(species)){
  #make new directory in c folder
    dir.create(paste0(out.dir,species[i]))
  #get all the australian files
    aus<-list.files(paste0(in.dir,species[i]),pattern="Australia")
    all<-c(aus,"average_rcp85_2065.asc", "average_rcp85_2035.asc" ,"average_rcp45_2065.asc", "average_rcp45_2035.asc","maxentResults.csv","species_current_avg.asc")
  #copy all the files to new location    
    for (ii in 1:length(all)){
        file.copy(paste0(in.dir,species[i],"\\",all[ii]), paste0(out.dir,species[i],"\\",all[ii]),overwrite=TRUE)
    }
}
