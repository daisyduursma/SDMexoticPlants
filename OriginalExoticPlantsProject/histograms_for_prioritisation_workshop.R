rm(list = ls())

#directories
work.dir<-"f:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"Prioritsation scheme\\",sep="")
#data
dat<-read.csv("f:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_18_04_2013.csv")
#states
states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))
#two new ways to divide dat
qu <- c(0,0.05,0.25,0.75,0.95,1)


i<-9


par(mfrow = c(3, 3),xaxs="i",yaxs="i") 


for (i in 1:length(states)){
#make histogram for number of observations
gridded_obs<-as.vector(dat[,paste(states[i],"_gridcell_pres_100000km",sep="")])
zz<-dat[,paste(states[i],"_gridcell_pres_100000km",sep="")]
obs<-zz[zz>0]
hist(obs,main="Gridded observations",xlab=expression(paste("Observations [per 100,000 km"^2,"]")))
x<-zz
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
abline(v=c(xcut[2],xcut[3],xcut[4],xcut[5]),col="red")
mtext(paste(states[i] ), side=3,cex=.6,font=2)

}
###################################################
#make histogram for suitability of obs
for (i in 1:length(states)){
x<-as.vector(dat[,paste(states[i],".current_suitability",sep="")])

hist(x, main = "Habitat suitability of observations ", xlab ="avg. suitability",breaks=seq(0,1,by=.05))
#x<-as.vector(dat[,paste(states[i],".rcp_",rcp[ii],"_",year[j],"_suitability",sep="")])
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
abline(v=c(xcut[2],xcut[3],xcut[4],xcut[5]),col="red")
mtext(paste(states[i] ), side=3,cex=.6,font=2)

}
#make histogram for suitability area 
for (i in 1:length(states)){
x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
hist(x,main="Area of suitable habitat",xlab="% of region",breaks=seq(0,100,by=5))
#first interval includes all zero's and the 0 to 0.5
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
abline(v=c(xcut[2],xcut[3],xcut[4],xcut[5]),col="red")
mtext(paste(states[i] ), side=3,cex=.6,font=2)

}

####################################
#make histogram for high suitability area 
for (i in 1:length(states)){
x<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])

hist(x,main="Area of highly suitable habitat",xlab="% of region")
#first interval includes all zero's and the 0 to 0.5
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
abline(v=c(xcut[2],xcut[3],xcut[4],xcut[5]),col="red")
mtext(paste(states[i] ), side=3,cex=.6,font=2)
}

#####################################################
#make histogram for Minimum distance
for (i in 1:length(states)){
x<-as.vector(dat[,paste(states[i],"_current_km_observation_dist",sep="")])
#first interval includes all zero's and the 0 to 0.5
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
hist(x,main="Minimum distance",xlab=expression(paste("Km (log"[10],")")))
abline(v=c(xcut[2],xcut[3],xcut[4],xcut[5]),col="red")
mtext(paste(states[i] ), side=3,cex=.6,font=2)
}


