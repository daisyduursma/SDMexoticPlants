rm(list = ls())


work.dir<-"f:\\daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")

dat<-read.csv("f:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_18_04_2013.csv")

states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA"))

#log10 if suitable habitat
# x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
species<-as.vector(dat$spec)

species<-dat$spec


suit<-list()

for ( i in 1:length( states)){
state<-rep(states[i],292)
a<-data.frame(state=state,species=species,
              percent_occupied=dat[,paste("X10per_suitible.",states[i],"_rcp85_2035",sep="")],year="rcp85_2035")
b<-data.frame(state=state,species=species,
              percent_occupied=dat[,paste("X10per_suitible.",states[i],"_rcp85_2065",sep="")],year="rcp85_2065")
c<-data.frame(state=state,species=species,
              percent_occupied=dat[,paste("X10per_suitible.",states[i],"_rcp45_2035",sep="")],year="rcp45_2035")
d<-data.frame(state=state,species=species,
              percent_occupied=dat[,paste("X10per_suitible.",states[i],"_rcp45_2065",sep="")],year="rcp45_2065")
e<-data.frame(state=state,species=species,
              percent_occupied=dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")],year="current")
f<-rbind(a,b,c,d,e)

suit[[i]]<-f
}


state_suit<-do.call("rbind",suit)

write.csv(state_suit,"C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\States_long_format_presence_occupancy.csv")


i<-"AUS"

a<-data.frame(species=species,
              percent_occupied=dat[,paste("X10per_suitible.",i,"_rcp85_2035",sep="")],year="rcp85_2035")
b<-data.frame(species=species,
              percent_occupied=dat[,paste("X10per_suitible.",i,"_rcp85_2065",sep="")],year="rcp85_2065")
c<-data.frame(species=species,
              percent_occupied=dat[,paste("X10per_suitible.",i,"_rcp45_2035",sep="")],year="rcp45_2035")
d<-data.frame(species=species,
              percent_occupied=dat[,paste("X10per_suitible.",i,"_rcp45_2065",sep="")],year="rcp45_2065")
e<-data.frame(species=species,
              percent_occupied=dat[,paste("X10per_suitible.",i,"_current_10",sep="")],year="current")
f<-rbind(a,b,c,d,e)

write.csv(f,"C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\area_suitability\\Australian_long_format_presence_occupancy.csv")








