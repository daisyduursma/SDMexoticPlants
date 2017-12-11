library(car)

data.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\"
a<-list.files(data.dir,full.names=TRUE,pattern="_percent_suitable_habitat.csv")

#capad and capad restricted are good


#LGA
dat<-read.csv(paste(a[4]))

colnames(dat)[2]<-"STATE"

dat$STATE <- recode(dat$STATE, '1="NSW"; 2="VIC"; 3="QLD"; 4="SA"; 5="WA"; 6="TAS" ;7="NT"; 8="ACT"')

unique(dat$STATE)

write.csv(dat,paste(a[4]),row.names = FALSE)

#NRM -good

#RAMSAR - good
dat<-read.csv(paste(a[6]))

unique(dat$STATE)

#States
dat<-read.csv(paste(a[7]))
colnames(dat)[3]<-"STATE"
write.csv(dat,paste(a[7]),row.names = FALSE)




unique(dat$STATE)

