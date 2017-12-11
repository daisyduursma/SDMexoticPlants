
rm(list = ls())

dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_20_03_2013.csv")

states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))

par(mfrow = c(3, 3),xaxs="i",yaxs="i") 


#make histogram for number of observations
zz<-dat[,paste(states[1],"_gridcell_pres_100000km",sep="")]
obs<-zz[zz>0]
obs<-log10(obs)
cnt_0<-length(zz)-length(obs)
# d <- density(dat[,paste(states[i],"_gridcell_pres",sep="")]) # returns the density data 
# plot(d)
hist(obs,main=NULL,breaks=seq(-2,4.5,by=.2),xlab=expression(paste("Observations [per 100,000 km"^2," (log"[10],")]")))
#hist(obs,main=paste("Gridded obs ",states[i],sep=""),,xlab="obs per 100000 km2")
mtext(paste(states[1]), side=3,cex=.6,font=2)

zz<-dat[,paste(states[2],"_gridcell_pres_100000km",sep="")]
obs<-zz[zz>0]
obs<-log10(obs)
cnt_0<-length(zz)-length(obs)
# d <- density(dat[,paste(states[i],"_gridcell_pres",sep="")]) # returns the density data 
# plot(d)
hist(obs,main="Gridded observations",breaks=seq(-2,4.5,by=.2),xlab=expression(paste("Observations [per 100,000 km"^2," (log"[10],")]")))
#hist(obs,main=paste("Gridded obs ",states[i],sep=""),,xlab="obs per 100000 km2")
mtext(paste(states[2]), side=3,cex=.6,font=2)


for (i in 3:length(states)){

#make histogram for number of observations
zz<-dat[,paste(states[i],"_gridcell_pres_100000km",sep="")]
obs<-zz[zz>0]
obs<-log10(obs)
cnt_0<-length(zz)-length(obs)
# d <- density(dat[,paste(states[i],"_gridcell_pres",sep="")]) # returns the density data 
# plot(d)
hist(obs,main=NULL,breaks=seq(-2,4.5,by=.2),xlab=expression(paste("Observations [per 100,000 km"^2," (log"[10],")]")))
#hist(obs,main=paste("Gridded obs ",states[i],sep=""),,xlab="obs per 100000 km2")
mtext(paste(states[i]), side=3,cex=.6,font=2)
}


###################################################
#make histogram for suitability



suit<-as.vector(dat[,paste(states[1],".current_suitability",sep="")])
obs<-na.omit(suit)
cnt_0<-length(suit)-length(obs)
hist(obs,breaks=seq(0,1,by=.03), xlab = "Suitability",main=NULL)
mtext(paste(states[1]), side=3,cex=.6,font=2)

suit<-as.vector(dat[,paste(states[2],".current_suitability",sep="")])
obs<-na.omit(suit)
cnt_0<-length(suit)-length(obs)
hist(obs,breaks=seq(0,1,by=.03), xlab = "Suitability",main="Suitability of gridded observations")
mtext(paste(states[2]), side=3,cex=.6,font=2)

for (i in 3:length(states)){
  suit<-as.vector(dat[,paste(states[i],".current_suitability",sep="")])
  obs<-na.omit(suit)
  cnt_0<-length(suit)-length(obs) 
  hist(suit,main=NULL,breaks=seq(0,1,by=.03),xlab="Suitability")
  mtext(paste(states[i]), side=3,cex=.6,font=2) 
}


##########################################################



#make histogram for suitability area 

s_area<-as.vector(dat[,paste("X10per_suitible.",states[1],"_current_10",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main=NULL,breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[1]), side=3,cex=.6,font=2)

s_area<-as.vector(dat[,paste("X10per_suitible.",states[2],"_current_10",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main="Suitable habitat",breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[2]), side=3,cex=.6,font=2)

for (i in 3:length(states)){
s_area<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main=NULL,breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[i]), side=3,cex=.6,font=2)
}


#####################################
#make histogram for high suitability area 


s_area<-as.vector(dat[,paste(states[1],".current.suit..0.5",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main=NULL,breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[1] ), side=3,cex=.6,font=2)

s_area<-as.vector(dat[,paste(states[2],".current.suit..0.5",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main="Highly suitable habitat",breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[2] ), side=3,cex=.6,font=2)


for (i in 3:length(states)){
s_area<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])
obs<-s_area[s_area>0]
obs<-log10(obs)
cnt_0<-length(s_area)-length(obs)
hist(obs,main=NULL,breaks=seq(-1,2,by=.1),xlab=expression(paste("Percentage of area (log"[10],")")))
mtext(paste(states[i] ), side=3,cex=.6,font=2)

}


#####################################################

#make histogram for Minimum distance
dist<-as.vector(dat[,paste(states[1],"_current_km_observation_dist",sep="")])
obs<-na.omit(dist)
obs<-obs[obs>0]
obs<-log10(obs)
cnt_0<-length(dist)-length(obs)
hist(obs,main=NULL,breaks=seq(1,4,by=.1),xlab=expression(paste("Km (log"[10],")")))
mtext(states[1], side=3,cex=.6,font=2)

dist<-as.vector(dat[,paste(states[2],"_current_km_observation_dist",sep="")])
obs<-na.omit(dist)
obs<-obs[obs>0]
obs<-log10(obs)
cnt_0<-length(dist)-length(obs)
hist(obs,main="Minimum distance",breaks=seq(1,4,by=.1),xlab=expression(paste("Km (log"[10],")")))
mtext(states[2], side=3,cex=.6,font=2)



for (i in 3:length(states)){

dist<-as.vector(dat[,paste(states[i],"_current_km_observation_dist",sep="")])
obs<-na.omit(dist)
obs<-obs[obs>0]
obs<-log10(obs)
cnt_0<-length(dist)-length(obs)
hist(obs,main=NULL,breaks=seq(1,4,by=.1),xlab=expression(paste("Km (log"[10],")")))
mtext(states[i], side=3,cex=.6,font=2)
}