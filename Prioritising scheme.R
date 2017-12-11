#combine seperate files made during state analysis
# Calculate the 

rm(list = ls())
library(car)
#directories
  work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\aa_all_species\\scrnTOOL\\"
  out.dir<-paste(work.dir,"Prioritsation scheme\\",sep="")
  #dir.create(out.dir)

### combine data
# dat1<-read.csv(paste0(work.dir,"Distance_HIgh_stblty.csv"))
# dat2<-read.csv(paste0(work.dir,"obs_suit.csv"))
# dat3<-read.csv(paste0(work.dir,"Region_stblty_HIGH_Normal.csv"))
#  all<-merge(dat1,dat2,by="Species",all=TRUE)
# all<-merge(all,dat3,by="Species",all=TRUE)
# write.csv(all,paste0(work.dir,"combined_measures.csv"))

#make gridded obs per 100,000km

# Western Australia  2526786
# Queensland	1723936
# Northern Territory	1335742
# South Australia	978810
# New South Wales	800628
# Victoria	227010
# Tasmania	64519
# Australian Capital Territory 2358
# AUSTRALIA		7659861
# 
# all$TAS_gridcells_pres_100000km<-all$TAS_gridded_obs/64519*100000
# all$VIC_gridcells_pres_100000km<-all$VIC_gridded_obs/227010*100000
# all$SA_gridcells_pres_100000km<-all$SA_gridded_obs/978810*100000
# all$NSW_gridcells_pres_100000km<-all$NSW_gridded_obs/800628*100000
# all$WA_gridcells_pres_100000km<-all$WA_gridded_obs/2526786*100000
# all$QLD_gridcells_pres_100000km<-all$QLD_gridded_obs/1723936*100000
# all$ACT_gridcells_pres_100000km<-all$ACT_gridded_obs/2358*100000
# all$NT_gridcells_pres_100000km<-all$NT_gridded_obs/1335742*100000
# all$AUS_gridcells_pres_100000km<-all$AUS_gridded_obs/7659861*100000
# write.csv(all,paste0(work.dir,"combined_measures.csv"))

#read in data 
dat<-read.csv(paste0(work.dir,"combined_measures.csv"))

#natualised species data

nat_dat<-read.csv(paste0("D:\\Current_Projects\\exotic_plants\\Prioritsation scheme\\species_threat_assessment_19_05_2013.csv"))

#quartiles to divide data
  qu <- c(0,0.05,0.25,0.75,0.95,1)

#future variables
  rcp<-c("current","rcp85_2035","rcp85_2065","rcp45_2035","rcp45_2065")
#states
  states<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))


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
  x<-nat_dat[,paste0(states[i],"_gridcells_pres_100000km")]
  y<-dat[,paste0(states[i],"_gridcells_pres_100000km")]
 
  #first interval includes all zero's and the 0 to 0.5
  
  xcut <- quantile(x, qu,na.rm=TRUE)
  whichzero <- xcut==0
  if(sum(whichzero) > 1){
    xcut[max(which(whichzero))] <- 1E-09
  }
   xcut[6] <- 100000
  xcut[1] <- 0
   xf <- findInterval(y,xcut,rightmost.closed=TRUE)
  xf<-recode(xf,'NA=1')
  
  if(length(unique(xf))==5){
    xr <- recode(xf, '1=2; 2=4; 3=6; 4=8; 5=10')}else
      if(length(unique(xf))<5){  
        xr <- recode(xf, '2=2; 3=6; 4=8; 5=10')}
  
  zz$gridded_obs <- as.numeric(as.character(xr))
    
  ##########################################################
  
  #suitability of observations
  y<-as.vector(nat_dat[,paste0(states[i],"_current_hab_suitabiliy_of_obs")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_stblty_obs")])
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
  y<-as.vector(nat_dat[,paste0(states[i],"_current_per_area_suitable")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_stblty")])
  
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
  y<-as.vector(nat_dat[,paste0(states[i],"_current_per_area_highly_suitable")])
  x<-as.vector(dat[,paste0(states[i],"_",rcp[ii],"_HIGH_stblty")])
  
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
  y<-as.vector(nat_dat[,paste0(states[i],"_current_km_observation_dist")])
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
  #zz$rating<-rowSums(zz[,c("gridded_obs","hab_suitability_obs","suit_habt","high_suit_habt","min_dist")])
  
  dat2$rating<-rowSums(zz[,c("gridded_obs","hab_suitability_obs","suit_habt","high_suit_habt","min_dist")])
  colnames(dat2)[ncol(dat2)]<-paste0(states[i],"_",rcp[ii])
  
#   
#  subouts[[i]]<-zz
# # 
 }
# 
#outputs[[ii]]<-do.call("rbind",subouts)
  
  
 
  
}  
  
  #final_out<-do.call("rbind",outputs)

write.csv(dat2,paste0(out.dir,"\\ratings_07_01_2013.csv"
                      
a<-read.csv(paste0(out.dir,"\\ratings_07_01_2013.csv"))
                      
                      
  
  