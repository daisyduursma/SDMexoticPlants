rm(list = ls())

library(car)

#read in ecodata and subset for what is needed
eco<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\ECOREGION_percent_suitable_habitat.csv")

eco<-eco[,c("year","species", "state","percent_suitable","ECO_NAME") ]
#get c3 and c4 plants
cpath<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\paper\\manuscripts\\C3 C4\\Poacea C3 C4 Pathways.csv")[,c("species","Photsynthetic.pathway")]

#get the unique region names
a<-unique(eco$ECO_NAME)

for(i in 1:length(a)){
  
  dat<-subset(eco,ECO_NAME==paste(a[i]))
  dat_cur<-subset(dat,year=="current")
  dat_fut<-subset(dat,year=="rcp85_2065")
  
  dat2<-merge(dat_cur,dat_fut,by="species",all=TRUE)
  ##################GET TO WORK###########
  dat2$percent_suitable.x <- recode(dat2$percent_suitable.x, 'NA=0')
  dat2$percent_suitable.y <- recode(dat2$percent_suitable.y, 'NA=0')
  dat2$change<- dat2$percent_suitable.y-dat2$percent_suitable.x
  dat3<-dat2[c("species","ECO_NAME.x","change","percent_suitable.x","percent_suitable.y")]
  colnames(dat3)<-c("species","ECO_NAME","change","percent_suitable_current","percent_suitable_rcp85_2065")
  dat4<-merge(dat3,cpath,by="species")
  
  c3<-subset(dat4,Photsynthetic.pathway=="C3")
  c4<-subset(dat4,Photsynthetic.pathway=="C4")
  
  
}

headbio<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\BIOME_percent_suitable_habitat.csv")
bio<-bio[,c("year","species", "state","percent_suitable","BIOME") ]



"year"              "species"           "state"            
[25] "percent_suitable" 









#fix names in files


dat_files<-list.files("f:\\daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\",full.names=TRUE)[c(3,6)]
