rm(list = ls())


work.dir<-"f:\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"\\Website\\Prioritisation\\",sep="")

dat<-read.csv("f:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_18_04_2013.csv")

states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))

#
#
qu <- c(0,0.05,0.25,0.75,0.95,1)
qu2 <- seq(0,1,by=0.2)

# 
# rcp<-c("85","45")
# year<-c("2035","2065")
# 
# 
# 
# for(ii in 1:length(rcp)){
#    for(j in 1:length(year)){
     
     
     dat2<-as.data.frame(dat$spec)
     
     for (i in 1:length(states)){
       
       
 require(gtools)
 
#log10 number of observations
zz<-dat[,c("spec",paste(states[i],"_gridcell_pres_100000km",sep=""))]
colnames(zz)[2]<-"pres"


 #defined
zz$pres_log10<-log10(zz$pres)
x<-zz$pres_log10
v <- c(-Inf,-1, -0.5, .75,1.8,Inf) # create 5 bins
xv <- cut(x, v, include.lowest=TRUE)
levels(xv) <- as.character(c(2,4,6,8,10))
zz$pres_log10_defined<-as.numeric(as.character(xv))


#intercals 5%..... 95%
x<-zz$pres
# whichinf <- which(is.infinite(x))
# x[whichinf] <- NA

#first interval includes all zero's and the 0 to 0.5
xcut <- quantile(x[x>0], qu,na.rm=TRUE)
xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])

 
 
xv <- cut(x, unique(xcut),include.lowest=TRUE)
levels(xv) <- as.character(c(2,4,6,8,10))

zz$pres_95per <- as.numeric(as.character(xv))
#zz$pres_95per[whichinf] <- 2

 
#equal intervals but not the 0 values
xcut <- quantile(x[x>0], qu2,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 
 qv <- cut(x, unique(xcut),include.lowest=TRUE)
#qv <-quantcut(x, qu2)
levels(qv) <- as.character(c(2,4,6,8,10))
zz$pres_equal <- as.numeric(as.character(qv))



##########################################################




#suitability of observations
 x<-as.vector(dat[,paste(states[i],".current_suitability",sep="")])
 #x<-as.vector(dat[,paste(states[i],".rcp_",rcp[ii],"_",year[j],"_suitability",sep="")])
 #change NA to 0, NA means there are no observations and thus nothing is suitable
 x[ is.na(x) ] <- 0

v <- c(0,0.2, 0.62, .72,Inf) # create 5 bins
 
 xv <- cut(x, v, include.lowest=TRUE)
 levels(xv) <- as.character(c(2,4,6,8,10))
 zz$obs_suit<-as.numeric(as.character(xv))
 
 #intercals 5%..... 95%

 #first interval includes all zero's and the 0 to 0.5
 xcut <- quantile(x[x>0], qu,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 xv <- cut(x, unique(xcut),include.lowest=TRUE)
 levels(xv) <- as.character(c(2,4,6,8,10))
 zz$obs_suit_95per <- as.numeric(as.character(xv))
 
 
 #equal intervals but not the 0 values
 xcut <- quantile(x[x>0], qu2,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 
 qv <- cut(x, unique(xcut),include.lowest=TRUE)
 #qv <-quantcut(x, qu2)
 levels(qv) <- as.character(c(2,4,6,8,10))
 zz$obs_suit_equal <- as.numeric(as.character(qv))
 
 
 
 
######################################################
 
 
 
 
 
 
 
 

#log10 if suitable habitat
 x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
#x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_rcp",rcp[ii],"_",year[j],sep="")])
x<-log10(x)
v <- c(-Inf,0.8,1.4, 1.7, 1.9,Inf) # create 5 bins
xv <- cut(x, v, include.lowest=TRUE)
levels(xv) <- as.character(c(2,4,6,8,10))
zz$suit_hab<-as.numeric(as.character(xv))
 
 
 #not log
 x<-as.vector(dat[,paste("X10per_suitible.",states[i],"_current_10",sep="")])
 #first interval includes all zero's and the 0 to 0.5
 xcut <- quantile(x[x>0], qu,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 xv <- cut(x, unique(xcut),include.lowest=TRUE)
 levels(xv) <- as.character(c(2,4,6,8,10))
 zz$suit_habt_95per <- as.numeric(as.character(xv))
 
 
 #equal intervals but not the 0 values
 xcut <- quantile(x[x>0], qu2,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 
 qv <- cut(x, unique(xcut),include.lowest=TRUE)
 levels(qv) <- as.character(c(2,4,6,8,10))
 zz$suit_habt_equal <- as.numeric(as.character(qv))
 
 
 
 
 
################################################################
 
 
 
#log10 high suitability habitat 
x<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])
#x<-as.vector(dat[,paste(states[i],".rcp",rcp[ii],"_",year[j],".suit..0.5",sep="")])
x<-log10(x)
v <- c(-Inf,-.4,.7, 1.4, 1.8,Inf) # create 5 bins
xv <- cut(x, v, include.lowest=TRUE)
levels(xv) <- as.character(c(2,4,6,8,10))
zz$high_suit_hab<-as.numeric(as.character(xv))
 
 #not log
 x<-as.vector(dat[,paste(states[i],".current.suit..0.5",sep="")])
 #first interval includes all zero's and the 0 to 0.5
 xcut <- quantile(x[x>0], qu,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 xv <- cut(x, unique(xcut),include.lowest=TRUE)
 levels(xv) <- as.character(c(2,4,6,8,10))
 zz$high_suit_hab_95per <- as.numeric(as.character(xv))
 
 
 #equal intervals but not the 0 values
 xcut <- quantile(x[x>0], qu2,na.rm=TRUE)
 xcut <- c(0, xcut[(length(xcut)-4):length(xcut)])
 qv <- cut(x, unique(xcut),include.lowest=TRUE)
 levels(qv) <- as.character(c(2,4,6,8,10))
 zz$high_suit_hab_equal <- as.numeric(as.character(qv))
 

 
 
#log10 Minimum distance
x<-as.vector(dat[,paste(states[i],"_current_km_observation_dist",sep="")])
#x<-as.vector(dat[,paste(states[i],"_rcp",rcp[ii],"_",year[j],"_km_observation_dist",sep="")])
x<-log10(x)
x[ is.na(x) ] <- Inf
v <- c(1,1.7, 2.5, 3.4,Inf) # create 5 bins
a<- findInterval(x, v)
a[a==0] <- 10
a[a==1] <- 8
a[a==2] <- 6
a[a==3] <- 4
a[a==4] <- 2

zz<-cbind(zz,a)
colnames(zz)[ncol(zz)]<-"min_dis"
 
 v <- c(-Inf,-.4,.7, 1.4, 1.8,Inf) # create 5 bins
 xv <- cut(x, v, include.lowest=TRUE)
 levels(xv) <- as.character(c(2,4,6,8,10))
 zz$high_suit_hab<-as.numeric(as.character(xv))
 
 
 
 
 

threat<-rowSums(zz[,2:6])
v <- c(-Inf,25,40,Inf) # create 3 bins
a<- findInterval(threat, v)
a[a==1] <- "Low"
a[a==2] <- "Medium"
a[a==3] <- "High"


dat2<-cbind(dat2,a)
colnames(dat2)[ncol(dat2)]<-paste(states[i])


}

#write.csv(zz,"C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\AUS_rcp85_2035_prioritised_Appendix 3a.csv",row.names = FALSE)
     
     colnames(dat2)[1]<-"species"
     write.csv(dat2,paste(out.dir,"prioritised_species_current.csv",sep=""),row.names = FALSE)
     
    
#write.csv(dat2,paste(out.dir,"prioritised_species_rcp",rcp[ii],"_",year[j],".csv",sep=""),row.names = FALSE)

}}
          
          
 


























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
