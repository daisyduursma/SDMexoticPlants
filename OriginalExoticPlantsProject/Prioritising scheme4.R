rm(list = ls())
library(car)
#directories
  work.dir<-"c:\\daisy\\Current_Projects\\exotic_plants\\"
  out.dir<-paste(work.dir,"Prioritsation scheme\\",sep="")
#data
  dat<-read.csv(paste0(out.dir,"\\species_threat_assessment_19_05_2013.csv"))

#states
  states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))

#quantiles to divide data
  qu <- c(0,0.05,0.25,0.75,0.95,1)

#future variables
   rcp<-c("current","rcp85_2035","rcp85_2065","rcp45_2035","rcp45_2065")


outputs<-list()
dat2<-as.data.frame(dat$Species)


    
#make dataframe with just species names
  #dat2<-as.data.frame(dat$Species)

  require(gtools)
############START PRIORITISATION SCHEMES
  subouts<-list()
 
  for (i in 1:length(states)){
    for(ii in 1:length(rcp)){
      
  #observations
  zz<-dat[,c("Species",paste0(states[i],"_gridcells_pres_100000km"))]
  colnames(zz)[2]<-"gridded_obs"
  x<-dat[,paste0(states[i],"_gridcells_pres_100000km")]
  #first interval includes all zero's and the 0 to 0.5
  
  xcut <- quantile(x, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
   xf <- findInterval(x,xcut,rightmost.closed=TRUE)
  
  if(length(unique(xf))==5){
    xr <- recode(xf, '1=2; 2=4; 3=6; 4=8; 5=10')}else
      if(length(unique(xf))<5){  
        xr <- recode(xf, '2=2; 3=6; 4=8; 5=10')}
  
  zz$gridded_obs <- as.numeric(as.character(xr))
    
  ##########################################################
  
  #suitability of observations
  y<-as.vector(dat[,paste0(states[i],"_current_hab_suitabiliy_of_obs")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_hab_suitabiliy_of_obs")])
  #change NA to 0, NA means there are no observations and thus nothing is suitable
    y[ is.na(y) ] <- 0
   x[ is.na(x) ] <- 0
  
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
  #set max xcut so it is 1 and min so it is 0
  xcut[6] <- 1
  xcut[1] <- 0
  
  xf <- findInterval(x,xcut,rightmost.closed=TRUE)
  if(length(unique(xf))==5){
    xr <- recode(xf, '1=2; 2=4; 3=6; 4=8; 5=10')}else
      if(length(unique(xf))< 5){  
        xr <- recode(xf, '2=2; 3=6; 4=8; 5=10')}
   zz$hab_suitability_obs <- as.numeric(as.character(xr))
    
 ######################################################
  
  
  #suitable habitat
  y<-as.vector(dat[,paste0(states[i],"_current_per_area_suitable")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_per_area_suitable")])
  
  xcut <- quantile(y, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
  xcut[6] <- 100
  xcut[1] <- 0
  
  xf <- findInterval(x,xcut,rightmost.closed=TRUE)
      xr <- recode(xf, '1=2; 2=4; 3=6; 4=8; 5=10')
        
   zz$suit_habt <- as.numeric(as.character(xr))
 
################################################################
  
  
  #high suitability habitat 
  y<-as.vector(dat[,paste0(states[i],"_current_per_area_highly_suitable")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_per_area_highly_suitable")])
  
  xcut <- quantile(y, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
  xcut[6] <- 100
  xcut[1] <- 0
  
  xf <- findInterval(x,xcut,rightmost.closed=TRUE)
  
  if(length(unique(xf))==5){
    xr <- recode(xf, '1=2; 2=4; 3=6; 4=8; 5=10')}else
      if(length(unique(xf))<5){  
        xr <- recode(xf, '2=2; 3=6; 4=8; 5=10')}
  
  zz$high_suit_habt <- as.numeric(as.character(xr))
  
  
  ######################################################## 
  
  #Minimum distance
  y<-as.vector(dat[,paste0(states[i],"_current_km_observation_dist")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_km_observation_dist")])
 
 
  #first interval includes all zero's and the 0 to 0.5
  xcut <- quantile(y, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
  xcut[6] <- 10000
  xcut[1] <- 0
  x[ is.na(x) ] <- 10000
  
  xf <- findInterval(x,xcut,rightmost.closed=TRUE)
  
  
    xr <- recode(xf, '1=10; 2=8; 3=6; 4=4; 5=2')

  zz$min_dist <- as.numeric(as.character(xr))
  
  zz$time<-rcp[ii]
  zz$region<-states[i]
    

#   
  zz$rating<-rowSums(zz[,c("gridded_obs","hab_suitability_obs","suit_habt","high_suit_habt","min_dist")])
  
  dat2$rating<-rowSums(zz[,c("gridded_obs","hab_suitability_obs","suit_habt","high_suit_habt","min_dist")])
  colnames(dat2)[ncol(dat2)]<-paste0(states[i],"_",rcp[ii])
  
#   
#  subouts[[i]]<-zz
# # 
 }
# 
# outputs[[ii]]<-do.call("rbind",subouts)
  
  
 
  
}  
  
  #final_out<-do.call("rbind",outputs)

write.csv(dat2,paste0(out.dir,"\\ratings_19_05_2013.csv"))
  
  
  
  
  