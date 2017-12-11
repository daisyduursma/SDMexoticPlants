dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australia_percent_suitable_habitat.csv")

a<-subset(dat,year=="current",select="percent_suitable")
b<-subset(dat,year=="rcp45_2035",select="percent_suitable")
c<-subset(dat,year=="rcp45_2065",select="percent_suitable")
d<-subset(dat,year=="rcp85_2035",select="percent_suitable")
e<-subset(dat,year=="rcp85_2065",select="percent_suitable")

ee<-(e-a)>5
summary(ee)
