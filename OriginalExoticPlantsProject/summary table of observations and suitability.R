



dat<-read.csv("F:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_12_03_2013.csv")

states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA" ))

par(mfrow = c(3, 2)) 

obs_vals<-list()

for (i in 1:length(states)){

  #select species with observations within the state
  zz<-dat
  zz$obs<-zz[,paste(states[i],"_gridcell_pres",sep="")]
  zz2<-subset(zz,obs>0)
  #number of species with gridded observations
  sp_pres<-nrow(zz2)
  zz2$suit<-zz2$obs*zz2[,paste(states[i],".rcp_85_2035_suitability",sep="")]
  
    a<-sum(zz2$suit)
    b<-sum(zz2$obs)
  
  avg_suit<-round(a/b,2)
  
  zz$dist<-zz[,paste(states[i],"_rcp85_2035_km_observation_dist",sep="")]
  
  c<-subset(zz, dist == 0)
  within<-nrow(c)
  d<-subset(zz, dist <= 25)
  within25 <-nrow(d)
  e<-subset(zz, dist <= 100)
  within100 <-nrow(e)
  
  
  state_dat<-c(states[i],sp_pres,avg_suit,within,within25,within100)
  
  obs_vals[[i]]<-state_dat
  
}
  
  aa<-do.call("cbind",obs_vals)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #average suitabuility of those observations
  
#cnt_0<-length(zz)-length(obs)
 
# d <- density(dat[,paste(states[i],"_gridcell_pres",sep="")]) # returns the density data 
# plot(d)

#hist(obs,main=paste("Gridded observations ",states[i],sep=""),breaks=seq(min(obs),max(obs),by=(max(obs)-min(obs))/100))
mtext(paste("removed species with no observations = ",cnt_0 ), side=3,cex=.6)

#make histogram for suitability
  suit<-as.vector(dat[,paste(states[i],".current_suitability",sep="")])
  obs<-na.omit(suit)
  cnt_0<-length(suit)-length(obs)
              
  hist(suit,main=paste("Suitability of gridded obs ",states[i]," current",sep=""),breaks=seq(min(obs),max(obs),by=(max(obs)-min(obs))/20))
  mtext(paste("removed species with no observations = ",cnt_0 ), side=3,cex=.6)

#make histogram for suitability area 
s_area<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
obs<-s_area[s_area>0]
cnt_0<-length(s_area)-length(obs)

hist(obs,main=paste(states[i], " Area of climatic suitability",sep=""),breaks=seq(0,100,by=5),xlab="%")
mtext(paste("removed species with NO highly suitabile area = ",cnt_0 ), side=3,cex=.6)

#make histogram for high suitability area 
s_area<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])
obs<-s_area[s_area>0]
cnt_0<-length(s_area)-length(obs)

hist(obs,main=paste(states[i], " Area of high climatic suitability ",sep=""),breaks=seq(0,100,by=5),xlab="%")
mtext(paste("removed species with NO highly suitabe area = ",cnt_0 ), side=3,cex=.6)

#make histogram for Minimum distance
dist<-as.vector(dat[,paste(states[i],"_current_km_observation_dist",sep="")])
obs<-na.omit(dist)
cnt_0<-length(dist)-length(obs)

hist(obs,main=paste(states[i], " Minimum distance ",sep=""),breaks=seq(min(obs),max(obs),by=(max(obs)-min(obs))/100))
mtext(paste("removed species with NO highly suitabile area" ), side=3,cex=.6)

plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')

}