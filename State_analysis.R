rm(list = ls())


# make table for each state with column for each analyis
r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
r <- require(maptools)
if(!r)stop("Install maptools")

work.dir<-"c:\\Daisy\\Current_Projects\\exotic_plants_2\\"
out.dir<-paste(work.dir,"outputs\\",sep="")

#background data
      #csv of center points of grid cells for each Australian state
        states<-read.csv("d:\\Current_Projects\\exotic_plants\\outputs\\Land_Catagories\\Australian_States_point_polygon_summary.csv")
      #state polygon layer
        poly_states<-readShapePoly("d:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\NRM_Australia_states.shp")


      #species observations, keep only those in Australia
        sp_dat<- read.csv(paste0(work.dir,"data\\", "Final_observation_bias_removed_21_10_13.csv"),header=TRUE, sep=",")[,c(1:3)]
        x<-(sp_dat[,"lon"])
        y<-(sp_dat[,"lat"])
        locs <- SpatialPoints(data.frame(x,y))
        locs<-over(locs,poly_states)
        prelocs<-na.omit(cbind(sp_dat,locs))[,c(1:3)]

#keep only species of interest
#get species names
spec<-list.files(paste0(work.dir,"outputs"))[2:254]
sp_dat2<-list()
for(i in 1:length(spec)){
  dat<-subset(prelocs,species==spec[i])
  sp_dat2[[i]]<-dat
  message(i) 
}
locs<-droplevels(do.call(rbind,sp_dat2))

    
######################################

##############PRESENCE, number of obs in each state, numer in Aus###############

# for each state number of 8km grid cells with known observations
    #get locations
     x<-(locs[,"lon"])
     y<-(locs[,"lat"])
     locs2 <- SpatialPoints(data.frame(x,y))
    #extract which states the locations are in
      obs_state<-sp::over(locs2,poly_states)
      obs_state<-na.omit(cbind(locs,obs_state))
    #get name of states
      state<-as.vector(unique(obs_state$STATE))
    #make dataframe fom species names
      dat<-as.data.frame(spec)
      colnames(dat)[1]<-"Species"
    #find out number of 8km grid cells species occures in
      for (i in 1:length(state)){
        sub<-subset(obs_state,STATE==state[i])
        sub<-as.data.frame(table(sub$species))
        colnames(sub)[2] <- paste(state[i],"_gridded_obs",sep="")
        dat<-merge(dat,sub,by.x="Species",by.y="Var1")
        message(state[i])
      }
    #number of observations in Australia
        sub<-as.data.frame(table(obs_state$species))
        dat$AUS_gridded_obs<-sub$Freq
      
################################################################

############## AVERAGE Australian SUITABILITY where observations occur 
##################################


    #for each species extract the current grid cell value for location, aggregate these by state
    #make blank list
      obs_vals<-list()
  for (i in 1:length(spec)){
        #get data for species
          sp_obs_state<-subset(obs_state,species==spec[i])
        #read in rurrent raster for first species
          cur<-raster(paste(out.dir,spec[i],"\\Australia_current.asc",sep=""))
          rcp_45_2035<-raster(paste(out.dir,spec[i],"\\Australia_average_rcp45_2035.asc",sep=""))
          rcp_45_2065<-raster(paste(out.dir,spec[i],"\\Australia_average_rcp45_2065.asc",sep=""))
          rcp_85_2035<-raster(paste(out.dir,spec[i],"\\Australia_average_rcp85_2035.asc",sep=""))
          rcp_85_2065<-raster(paste(out.dir,spec[i],"\\Australia_average_rcp85_2065.asc",sep=""))
          
          #Australian
          x<-subset(sp_obs_state,species==spec[i], select="lon")
          y<-subset(sp_obs_state,species==spec[i], select="lat")
          locs2<-cbind(x,y)
          dat1<-(extract(cur,locs2))
          dat2<-(extract(rcp_45_2035,locs2))
          dat3<-(extract(rcp_45_2065,locs2))
          dat4<-(extract(rcp_85_2035,locs2))
          dat5<-(extract(rcp_85_2065,locs2))
          
          AUS_current_stblty_obs<-round(mean(dat1, na.rm = TRUE),3)
          AUS_rcp45_2035_stblty_obs<-round(mean(dat2, na.rm = TRUE),3)
          AUS_rcp45_2065_stblty_obs<-round(mean(dat3, na.rm = TRUE),3)
          AUS_rcp85_2035_stblty_obs<-round(mean(dat4, na.rm = TRUE),3)
          AUS_rcp85_2065_stblty_obs<-round(mean(dat5, na.rm = TRUE),3)
          
          dat_b<-as.data.frame(cbind(spec[i], AUS_current_stblty_obs, AUS_rcp45_2035_stblty_obs,AUS_rcp45_2065_stblty_obs, AUS_rcp85_2035_stblty_obs,AUS_rcp85_2065_stblty_obs))
               
          #state level
          sp_obs_state2<-cbind(sp_obs_state,dat1,dat2,dat3,dat4,dat5)
          
    for (ii in 1:length(state)){
          
           state_obs<-subset(sp_obs_state2,STATE==state[ii]) 
     #if there are no observations     
    if(nrow(na.omit(sp_obs_state))==0){
        time_col_names<-paste(rep(state[ii],5),c("current_stblty_obs","rcp45_2035_stblty_obs","rcp45_2065_stblty_obs","rcp85_2035_stblty_obs","rcp85_2065_stblty_obs"),sep="_")
        state_dat<-t(data.frame(rep(NA,5),row.names = NULL))
        colnames(state_dat)<-time_col_names  } else { 
        time_col_names<-paste(rep(state[ii],5),c("current_stblty_obs","rcp45_2035_stblty_obs","rcp45_2065_stblty_obs","rcp85_2035_stblty_obs","rcp85_2065_stblty_obs"),sep="_")
        state_dat<-t(data.frame(colSums(state_obs[,7:11])/nrow(state_obs)))
        colnames(state_dat)<-time_col_names 
        }
      dat_b<-cbind(dat_b,state_dat)
        
  }    
          
    obs_vals[[i]]<-dat_b
    message(paste0("species number ", i))
}
   dat_c<-do.call("rbind",obs_vals)
          
          dat<-merge(dat,dat_c,by.x="Species",by.y="V1")


write.csv(dat,paste0(out.dir,"\\scrnTOOL\\obs_suit.csv",row.names=FALSE)
          

      
################################################################

############## Area of suitable habitat and highly suitable
################################################################
          
scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
          
#Austraila level

AUS_dat<-read.csv(paste0(out.dir,"aa_all_species\\PrcntSUITregion\\Australia_percent_suitable_habitat.csv"))
High_AUS_dat<-read.csv(paste0(out.dir,"aa_all_species\\PrcntSUITregion\\Australia_percent_high_quality_suitable_habitat.csv"))
#divide up dat by scenario
scen_dat<-list()          
for( i in 1: length(scenario)){
  sub_dat_a<-subset(AUS_dat,year==scenario[i],select=c(species,percent_suitable))
  sub_dat_b<-subset(High_AUS_dat,year==scenario[i],select=c(species,percent_suitable))
  sub_dat<-merge(sub_dat_a,sub_dat_b,by="species")[2:3]
  colnames(sub_dat)<-c(paste0("AUS_",scenario[i],"_stblty"),paste0("AUS_",scenario[i],"_HIGH_stblty"))
  scen_dat[[i]]<-sub_dat
  
}
          
  all_dat<-do.call(cbind,scen_dat)
  all_dat<-cbind(sub_dat_a$species,all_dat)
  colnames(all_dat)[1]<-"species"        
        
          
#State level
          
st_dat<-read.csv(paste0(out.dir,"aa_all_species\\PrcntSUITregion\\state_percent_suitable_habitat.csv"))
High_st_dat<-read.csv(paste0(out.dir,"aa_all_species\\PrcntSUITregion\\state_percent_high_quality_suitable_habitat.csv"))

   
          
for( i in 1: length(scenario)){
  sub_dat_a<-subset(st_dat,year==scenario[i])
  sub_dat_b<-subset(High_st_dat,year==scenario[i])
  state<-unique(sub_dat_b$state)
  
  
  for (ii in 1:length(state)){
  sub_dat_2a<-subset(sub_dat_a,state==state[ii],select=c(species,percent_suitable))
  all_dat<-merge(all_dat,sub_dat_2a,by="species")
  sub_dat_2b<-subset(sub_dat_b,state==state[ii],select=c(species,percent_suitable))
  all_dat<-merge(all_dat,sub_dat_2b,by="species")
  x<-ncol(all_dat)-1
  y<-ncol(all_dat)
  colnames(all_dat)[c(x,y)]<-c(paste0(state[ii],"_",scenario[i],"_stblty"),paste0(state[ii],"_",scenario[i],"_HIGH_stblty"))
}
  
} 
 write.csv(all_dat,paste0(out.dir,"aa_all_species\\scrnTOOL\\Region_stblty_HIgh_normal.csv"),row.names=FALSE)
          

##################################          
 
############## Dist: suitable habitat to known observation
          
##################################
          
######################################
#get points for each species
#make raster of with points from distance

spec<-list.files(out.dir)[2:254]
 
#function to determine distance between highly suitable habitat in each state and nearest observation
obsHABITATdist<- function (spec,r,locs,dat, states,state_name,time){
 
rst <- require(raster)
if(!rst)stop("Install raster")
s <- require(sp)
if(!s)stop("Install sp")
m <- require(maptools)
if(!m)stop("Install maptools")

if(nrow(dat)==0){ag4<-t(as.matrix(rep(NA,8),nrow=1))} else {
  if(nrow(dat)==0){ag4<-matrix(rep(NA,8),nrow=1)}
  xy <- SpatialPoints(dat[,c("lon","lat")])
   fun <- function(x) { x[x<1]<- NA; return(x) }
    r <- calc(r, fun)
  dist<-distanceFromPoints(r,xy)
  dist<-mask(dist,r)
  xy_dist<-extract(dist,states[,c("x","y")],method="simple")
  xy_dist2<-(na.omit(cbind(xy_dist,states)))
    if(nrow(xy_dist2)==0){ag4<-matrix(rep(NA,8),nrow=1)}else {
    ag3 <- (aggregate(xy_dist2$xy_dist, by=list(xy_dist2$STATE), FUN="min"))
    ag3<-merge(ag3,state_name,by.x="Group.1",by.y="unique(states$STATE)",all=FALSE,all.y=TRUE)
    ag4<-t(ag3$x/1000)
    }
  }

aa<-rep(paste(time,"_km_observation_dist",sep=""),8)
bb<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA" ))
cnames<-paste(bb,aa,sep="")
colnames(ag4)<-cnames
rownames(ag4)<-spec[jj]
ag4<-as.data.frame(ag4)
if (length(na.omit(ag3))==8){
  ag4$aus<-NA } else{
  ag4$aus<-as.vector(min(ag4,na.rm = TRUE))  
  }
colnames(ag4)[ncol(ag4)]<-paste0("AUS",time,"_km_observation_dist")
       
return(ag4)

}
   
#clip by area of 50% suitability for each state (check on polygon functions, minimum of each polygon)
#get state names
state_name<-as.data.frame(unique(states$STATE))

          #current
min_km_curent<-list()

for (jj in 1:length(spec)){
   res <- obsHABITATdist(
    spec,
    r=raster(paste("D:\\Current_Projects\\exotic_plants_2\\outputs\\",spec[jj],"\\Australia_current_thresholded_high_quality.asc",sep="")),
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_current"
  )
    min_km_curent[[jj]]<-res
   message(jj)
}


min_km_curent2<-do.call("rbind",min_km_curent)
aa<-as.data.frame(spec)
min_km_curent2<-cbind(aa,min_km_curent2)
          
  
#rcp45_2035

min_km_rcp45_2035<-list()
for (jj in 1:length(spec)){
  res <- obsHABITATdist(
    spec,
    r=raster(paste("D:\\Current_Projects\\exotic_plants_2\\outputs\\",spec[jj],"\\Australia_average_rcp45_2035_thresholded_high_quality.asc",sep="")),
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp45_2035"
  )
  min_km_rcp45_2035[[jj]]<-res
  message(jj)
}

min_km_rcp45_2035<-do.call("rbind",min_km_rcp45_2035)

min_km<-cbind(min_km_curent2,min_km_rcp45_2035)


#rcp85_2035
min_km_rcp85_2035<-list()
for (jj in 1:length(spec)){
  res <- obsHABITATdist(
    spec,
    r=raster(paste("D:\\Current_Projects\\exotic_plants_2\\outputs\\",spec[jj],"\\Australia_average_rcp85_2035_thresholded_high_quality.asc",sep="")),
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp85_2035"
  )
  min_km_rcp85_2035[[jj]]<-res
  message(jj)
}

min_km_rcp85_2035<-do.call("rbind",min_km_rcp85_2035)
min_km<-cbind(min_km,min_km_rcp85_2035)


#rcp45_2065
min_km_rcp45_2065<-list()
for (jj in 1:length(spec)){
  res <- obsHABITATdist(
    spec,
    r=raster(paste("D:\\Current_Projects\\exotic_plants_2\\outputs\\",spec[jj],"\\Australia_average_rcp45_2065_thresholded_high_quality.asc",sep="")),
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp45_2065"
  )
  min_km_rcp45_2065[[jj]]<-res
  message(jj)
}

min_km_rcp45_2065<-do.call("rbind",min_km_rcp45_2065)

min_km<-cbind(min_km,min_km_rcp45_2065)


#rcp85_2065
min_km_rcp85_2065<-list()
for (jj in 1:length(spec)){
  res<- obsHABITATdist(
    spec,
    r=raster(paste("D:\\Current_Projects\\exotic_plants_2\\outputs\\",spec[jj],"\\Australia_average_rcp85_2065_thresholded_high_quality.asc",sep="")),
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp85_2065"
  )
  min_km_rcp85_2065[[jj]]<-res
  message(jj)
}

min_km_rcp85_2065<-do.call("rbind",min_km_rcp85_2065)
min_km<-cbind(min_km,min_km_rcp85_2065)

write.csv(min_km,paste0(out.dir,"aa_all_species\\scrnTOOL\\Distance_HIgh_stblty.csv"),row.names=FALSE)

          
          
####################     
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
 