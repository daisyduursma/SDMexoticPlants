

dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version6.csv")
aus<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\aa_all_species\\PrcntSUITregion\\Australia_percent_suitable_habitat.csv")

extent_of_suitable_habitat_under_current_conditions_of_australia<-subset(aus,year=="current",select=c(species,percent_suitable))
extent_of_suitable_habitat_under_RCP4.5_conditions_for_2035_of_australia<-subset(aus,year=="rcp45_2035",select=c(species,percent_suitable))
extent_of_suitable_habitat_under_RCP4.5_conditions_for_2065_of_australia<-subset(aus,year=="rcp45_2065",select=c(species,percent_suitable))
extent_of_suitable_habitat_under_RCP8.5_conditions_for_2035_of_australia<-subset(aus,year=="rcp85_2035",select=c(species,percent_suitable))
extent_of_suitable_habitat_under_RCP8.5_conditions_for_2065_of_australia<-subset(aus,year=="rcp85_2065",select=c(species,percent_suitable))

a<-merge(extent_of_suitable_habitat_under_current_conditions_of_australia,extent_of_suitable_habitat_under_RCP4.5_conditions_for_2035_of_australia,by="species",allx=TRUE)
a<-merge(a,extent_of_suitable_habitat_under_RCP4.5_conditions_for_2065_of_australia,by="species")
a<-merge(a, extent_of_suitable_habitat_under_RCP8.5_conditions_for_2035_of_australia,by="species")
a<-merge(a, extent_of_suitable_habitat_under_RCP8.5_conditions_for_2065_of_australia,by="species")

colnames(a)<-c("species","extent_of_suitable_habitat_under_current_conditions_of_australia","extent_of_suitable_habitat_under_RCP4.5_conditions_for_2035_of_australia","extent_of_suitable_habitat_under_RCP4.5_conditions_for_2065_of_australia","extent_of_suitable_habitat_under_RCP8.5_conditions_for_2035_of_australia","extent_of_suitable_habitat_under_RCP8.5_conditions_for_2065_of_australia")

write.csv(a,"C:\\Daisy\\Current_Projects\\exotic_plants_2\\trait database\\species_extent.csv")