
rm(list = ls())

traits<-read.csv("c:\\Daisy\\Current_Projects\\exotic_plants\\data\\Data traits updated vers 1.csv")



########soils
soils<-as.data.frame(traits$soil.types..invasive.)

newdat<-list()
for(i in 1:nrow(soils)){
  
  a<-as.character(soils[i,])
  a<-gsub(" ", "", a)
  b<-strsplit(a,",")[[1]]
  b[b == "AC"] <-  "Acidic"
  b[b == "AL"] <-  "Alkaline"
  b[b == "C"] <-  "Clay"
  b[b == "F"] <-  "Fertile"
  b[b == "G"] <-  "Gravel/stony"
  b[b == "H"] <-  "Moist soils"
  b[b == "I"] <-  "Infertile – low fertility"
  b[b == "L"] <-  "Loamy"
  b[b == "LAT"] <-  "Laterite"
  b[b == "M"] <-  "Most"
  b[b == "MI"] <-  "Medium infertile / moderate poor"
  b[b == "P"] <-  "Peat"
  b[b == "PD"] <-  "Poorly drained"
  b[b == "SA"] <-  "Sandy"
  b[b == "SL"] <-  "Silt"
  b[b == "WD"] <-  "Well drained"
  
  c<-paste(b,collapse=", ")
  
  newdat[[i]]<-c
  
}

traits$soil.types..invasive.<-do.call("rbind",newdat)

###############habitat
habitat<-as.data.frame(traits$"habitat.type..invasive.")

newdat<-list()
for(i in 1:nrow(habitat)){
  
  a<-as.character(habitat[i,])
  a<-gsub(" ", "", a)
  b<-strsplit(a,",")[[1]]
  

  b[b == "A"] <-  "Agricultural land"
  b[b == "AB"] <-  "Abandoned homesteads/cultivated land"
  b[b == "B"] <-  "Bushland"
  b[b == "C"] <-  "Coastal vegetation/sand dunes"
  b[b == "CL"] <-  "Cliffs and bluffs"
  b[b == "DSF"] <-  "Dry sclerophyll forest"
  b[b == "D"] <-  "Disturbed areas (including trails)"
  b[b == "DE"] <-  "Desert"
  b[b == "F"] <-  "Floodplains"
  b[b == "FOR"] <-  "Forest"
  b[b == "G"] <-  "Grasslands"
  b[b == "GE"] <-  "Garden"
  b[b == "H"] <-  "Heathlands"
  b[b == "M"] <-  "Mangroves"
  b[b == "P"] <-  "Pastures"
  b[b == "R"] <-  "Roadsides"
  b[b == "RA"] <-  "Rangelands"
  b[b == "RS"] <-  "Riparian systems"
  b[b == "S"] <-  "Scrublands/shrublands"
  b[b == "T"] <-  "Thickets"
  b[b == "U"] <-  "Urban areas, gardens and parks"
  b[b == "W"] <-  "Woodlands"
  b[b == "WE"] <-  "Wetlands"
  b[b == "WB"] <-  "Water bodies"
  b[b == "WL"] <-  "Wastelands"
  b[b == "WSF"] <-  "Wet sclerophyll forest"
  
  c<-paste(b,collapse=", ")
  
  newdat[[i]]<-c
  
}

traits$habitat.type..invasive.<-do.call("rbind",newdat)


###################################

disp<-as.data.frame(traits$"dispersal.morphology")

newdat<-list()
for(i in 1:nrow(disp)){
  
  a<-as.character(disp[i,])
  a<-gsub(" ", "", a)
  b<-strsplit(a,",")[[1]]
  
  
  b[b == "A"] <-  "Adhesive (burs, sticky, awns, hairs etc)"
  b[b == "B"] <-  "Ballistically dispersed"
  b[b == "E"] <-  "Elaiosome"
  b[b == "F"] <-  "Fleshy fruit/Edible/Aril"
  b[b == "H"] <-  "Hydrochorus (buoyant seed)"
  b[b == "T"] <-  "Tumbleweed"
  b[b == "U"] <-  "Unassisted"
  b[b == "(U/H)"] <-  "Hydrochorus (buoyant seed)"
  b[b == "W"] <-  "Winged"
   
  c<-paste(b,collapse=", ")
  
  newdat[[i]]<-c
  
}

traits$dispersal.morphology<-do.call("rbind",newdat)

########################################


gform<-as.data.frame(traits$growth.form)

newdat<-list()
for(i in 1:nrow(gform)){
  
  a<-as.character(gform[i,])
  a<-gsub(" ", "", a)
  b<-strsplit(a,",")[[1]]
  
  
  b[b == "H"] <-  "Herb"
  b[b == "V"] <-  "Vine/scrambler/climber"
  b[b == "T"] <-  "Tree"
  b[b == "S"] <-  "Shrub"
  b[b == "G"] <-  "Graminoid"
  b[b == "SU"] <-  "Succulent"
  b[b == "SU-S"] <-  "Succulent shrub"
  
  c<-paste(b,collapse=", ")
  
  newdat[[i]]<-c
  
}

traits$growth.form<-do.call("rbind",newdat)


write.csv(traits,"c:\\Daisy\\Current_Projects\\exotic_plants\\data\\Data traits updated vers 2.csv",row.names=FALSE)
