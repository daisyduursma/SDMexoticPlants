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

#future variables
   rcp<-c("85","45")
  year<-c("2035","2065")
    # for(ii in 1:length(rcp)){
    #    for(j in 1:length(year)){

ii<-1
j<-2

#make dataframe with just species names
  dat2<-as.data.frame(dat$spec)

############START PRIORITISATION SCHEMES
  for (i in 1:length(states)){
    require(gtools)
  
  #observations
  zz<-dat[,c("spec",paste(states[i],"_gridcell_pres_100000km",sep=""))]
  colnames(zz)[2]<-"pres"
  x<-zz$pres
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(x[x>0], qu,na.rm=TRUE)
  xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
  xv <- cut(x, unique(xcut),include.lowest=TRUE)
  levels(xv) <- as.character(c(2,4,6,8,10))
  zz$pres_95per <- as.numeric(as.character(xv))
    
  ##########################################################
  
  #suitability of observations
  y<-as.vector(dat[,paste(states[i],".current_suitability",sep="")])
  x<-as.vector(dat[,paste(states[i],".rcp_",rcp[ii],"_",year[j],"_suitability",sep="")])
  #change NA to 0, NA means there are no observations and thus nothing is suitable
  x[ is.na(x) ] <- 0
    y[ is.na(y) ] <- 0
  #intercals 5%..... 95%
  
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y[y>0], qu,na.rm=TRUE)
  xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
  xv <- cut(x, unique(xcut),include.lowest=TRUE)
  levels(xv) <- as.character(c(2,4,6,8,10))
  zz$obs_suit_95per <- as.numeric(as.character(xv))
  
 ######################################################
  
  
  #suitable habitat
  y<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
  x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_rcp",rcp[ii],"_",year[j],sep="")])
    
 #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y[y>0], qu,na.rm=TRUE)
  xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
  xv <- cut(x, unique(xcut),include.lowest=TRUE)
  levels(xv) <- as.character(c(2,4,6,8,10))
  zz$suit_habt_95per <- as.numeric(as.character(xv))
 
################################################################
  
  
  #high suitability habitat 
  y<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])
  x<-as.vector(dat[,paste(states[i],".rcp",rcp[ii],"_",year[j],".suit..0.5",sep="")])
 
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y[y>0], qu,na.rm=TRUE)
  xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
  xv <- cut(x, unique(xcut),include.lowest=TRUE)
  levels(xv) <- as.character(c(2,4,6,8,10))
  zz$high_suit_hab_95per <- as.numeric(as.character(xv))
  
  
  ######################################################## 
  
  #Minimum distance
  y<-as.vector(dat[,paste(states[i],"_current_km_observation_dist",sep="")])
  x<-as.vector(dat[,paste(states[i],"_rcp",rcp[ii],"_",year[j],"_km_observation_dist",sep="")])
 
 
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y[y>0], qu,na.rm=TRUE)
  xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
  xcut[6]<-10000
  x[ is.na(x) ] <- max(na.omit(x))
  xv <- cut(x, unique(xcut),include.lowest=TRUE)
  levels(xv) <- as.character(c(10,8,6,4,2))
  zz$min_dis_95per <- as.numeric(as.character(xv))
  
    
  v <- c(-Inf,25,40,Inf) 
  
  dat2$current_threat_int_0.05<-rowSums(zz[,c("pres_95per","obs_suit_95per","suit_habt_95per","high_suit_hab_95per","min_dis_95per")])
colnames(dat2)[ncol(dat2)]<-paste(states[i],"_rcp",rcp[ii],"_",year[j])

}


fut<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\workshop\\rcp85_2965breakdown.csv")

dat<-merge(cur_aus,fut,by="spec")



write.csv(dat,"C:\\Daisy\\Current_Projects\\exotic_plants\\workshop\\scheme_brakedown.csv",row.names=FALSE)


dat3<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\workshop\\rating.csv")

par(mfrow = c(2, 1),xaxs="i",yaxs="i") 
# 
# hist(dat3$AUS_current,breaks=seq(9,51,by=2))
# hist(dat3$AUS_rcp8.5_.2065,breaks=seq(9,51,by=2))


for (i in 1:length(states)){

 par(mfrow = c(1, 3),xaxs="i",yaxs="i") 
  
  
cur<-dat3[,paste(states[i],"_current",sep="")]
fut<-dat3[,paste(states[i],"_rcp8.5_.2065",sep="")]
hist(cur,breaks=seq(9,51,by=2),main=paste(states[i],"current rating"),xlab="rating")
hist(fut,breaks=seq(9,51,by=2),main=paste(states[i],"future rating"),xlab="rating")
change<-fut-cur
hist(change)


}




#   dat2$threat_int_0.05<- findInterval(dat2$threat_int_0.05, v)
#   colnames(dat2)[ncol(dat2)]<-paste(states[i],"invasion_risk_int_0.05",sep="_")
  
   
#   v <- c(-Inf,25,40,Inf) # create 3 bins
#  
#   dat2[dat2==1] <-as.numeric(1)
#   dat2[dat2==2] <- as.numeric(2)
#   dat2[dat2==3] <- as.numeric(3)
#   
#   
  
  
}

#write.csv(zz,"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\AUS_rcp85_2035_prioritised_Appendix 3a.csv",row.names = FALSE)

colnames(dat2)[1]<-"species"
write.csv(dat2,paste(out.dir,"prioritised_species_current_3_test_schemes.csv",sep=""),row.names = FALSE)




scheme<-dimnames(dat2)[[2]][2:ncol(dat2)]
risk<-list()
for (jj in 2:ncol(dat2)){
  
  a<-dat2[,jj]
  b<-t(as.data.frame(table(a)))
  colnames(b)<-b[1,]
  #b$scheme<-as.vector(scheme[jj-1])
  
  risk[[jj]]<-b
  
  }

risk2<-do.call("rbind",risk)

risk2<-cbind(scheme,risk2)

#write.csv(dat2,paste(out.dir,"prioritised_species_rcp",rcp[ii],"_",year[j],".csv",sep=""),row.names = FALS

























#           
#           
# jpeg(filename = "F:\\Current_Projects\\exotic_plants\\paper\\NCCARF Reports\\tables_figures\\hists_of_RCP85_2035_proitisation.jpg",
#      width = 17, height = 17, units = "cm", res=600, bg = "transparent")
# 
# par(mfrow = c(3, 3),xaxs="i",yaxs="i") 
# 
# a<-subset(dat2,select=c("dat$spec",states[1]))
# hist(a[,2],main=NULL,xlim=c(10,50),breaks=15,xlab="species")
# mtext(paste(states[1]), side=3,cex=.6,font=2)
# 
# a<-subset(dat2,select=c("dat$spec",states[2]))
# hist(a[,2],xlim=c(10,50),breaks=15,xlab="species",main=paste("Prioritisation scores RCP8.5 2035 "))
# mtext(paste(states[2]), side=3,cex=.6,font=2)
# 
# for (i in 3:length(states)){
#   
#   a<-subset(dat2,select=c("dat$spec",states[i]))
#   hist(a[,2],main=NULL,xlim=c(10,50),breaks=15,xlab="species")
#   mtext(paste(states[i]), side=3,cex=.6,font=2)
#   
# }
# 
# dev.off()  
#   
