# get ALA and GBIF observations


rm(list = ls())

#read in species list
spec<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants_2\\trait database\\ala_gbif.csv")


spec<-spec$species

spec<-c("Salix sepulcralis")

g.dir<-"C:\\Users\\mq20097681\\Google Drive\\"
#g.dir<-"C:\\Users\\dduursma\\Google Drive\\"

#C:\Users\mq20097681\Google Drive\R_scripts\exoticPlants2\sourceCode

#get source code for ALA and GBIF download
source(paste0(g.dir,"R_scripts\\exoticPlants2\\sourceCode\\getGBIF.r"))


       
    for(i in 1:length(spec)){
      
      
      
      sp_dat<-getGBIF(paste(spec[i]),outdir="c:\\daisy\\Current_Projects\\exotic_plants_2\\old\\observationsRaw\\")
      
      
      
    }

