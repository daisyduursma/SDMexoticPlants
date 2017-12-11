# get ALA and GBIF observations


rm(list = ls())

#read in species list
Tdat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version3.csv",header = TRUE)

spec<-Tdat$scientific_name

g.dir<-"C:\\Users\\mq20097681\\Google Drive\\R_scripts\\exoticPlants2\\"
#g.dir<-"C:\\Users\\dduursma\\Google Drive\\R_scripts\\exoticPlants2\\"

#get source code for ALA and GBIF download
source(paste0(g.dir,"sourceCode\\getALA.r"))
source(paste0(g.dir,"sourceCode\\getGBIF.r"))

#sp_dat<-getALA(paste(spec))

for(i in 341:length(spec)){
  dat<-getALA(paste(spec[i]),outdir="C:\\Daisy\\Current_Projects\\exotic_plants_2\\observations\\")
  
}



