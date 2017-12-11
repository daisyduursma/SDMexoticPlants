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
          

          
 
#distance from suitable habitat within state to known observation
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
if (length(na.omit(ag4[1,]))==8){
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

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          
################################################################

############## Area of suitable habitat

          
          
          
{          
# 
# {

  # #         #get xy and observations for species
# #           x<-subset(sp_dat,species==spec[i], select="Longitude")
# #           y<-subset(sp_dat,species==spec[i], select="Latitude")
# #           locs2<-cbind(x,y)
# #           
# #           dat1<-(extract(cur,locs2))
# #           dat2<-(extract(rcp_45_2035,locs2))
# #           dat3<-(extract(rcp_45_2065,locs2))
# #           dat4<-(extract(rcp_85_2035,locs2))
# #           dat5<-(extract(rcp_85_2065,locs2))
# #         #find mean global habitat suitability values for locations and sd
# #           dat1<-(extract(cur,locs2))
# #           dat2<-(extract(rcp_45_2035,locs2))
# #           dat3<-(extract(rcp_45_2065,locs2))
# #           dat4<-(extract(rcp_85_2035,locs2))
# #           dat5<-(extract(rcp_85_2065,locs2))
# #           global_mean_current_suitability<-round(mean(dat1, na.rm = TRUE),2)
# #           global_stdev_current_suitability<-round(sd(dat1, na.rm = TRUE),2)
# #           global_mean_rcp_45_2035_suitability<-round(mean(dat2, na.rm = TRUE),2)
# #           global_stdev_rcp_45_2035_suitability<-round(sd(dat2, na.rm = TRUE),2)
# #           global_mean_rcp_45_2065_suitability<-round(mean(dat3, na.rm = TRUE),2)
# #           global_stdev_rcp_45_2065_suitability<-round(sd(dat3, na.rm = TRUE),2)
# #           global_mean_rcp_85_2035_suitability<-round(mean(dat4, na.rm = TRUE),2)
# #           global_stdev_rcp_85_2035_suitability<-round(sd(dat4, na.rm = TRUE),2)
# #           global_mean_rcp_85_2065_current_suitability<-round(mean(dat5, na.rm = TRUE),2)
# #           global_stdev_rcp_85_2065_suitability<-round(sd(dat5, na.rm = TRUE),2)
# #           
#           
# #           species<-spec[i]
# #           dat6<-as.data.frame(cbind(species, global_mean_current_suitability, global_stdev_current_suitability,global_mean_rcp_45_2035_suitability, global_stdev_rcp_45_2035_suitability,global_mean_rcp_45_2065_suitability, global_stdev_rcp_45_2065_suitability,global_mean_rcp_85_2035_suitability, global_stdev_rcp_85_2035_suitability,global_mean_rcp_85_2065_current_suitability, global_stdev_rcp_85_2065_suitability))
# #           
# }      
# ############BY STATE
#         #find state for observations
# #            locs3 <- SpatialPoints(data.frame(x,y))
# # #         #extract which states the locations are in
# #           obs_state<-sp::over(locs3,poly_states)
# #           #obs_state<-na.omit(cbind(locs2,obs_state))
#    
#           
#     
#     if(nrow(na.omit(sp_obs_state))==0){
#       dat9<-paste(rep(as.vector(unique(states$STATE)),each=5),c("current_suitability","rcp_45_2035_suitability","rcp_45_2065_suitability","rcp_85_2035_suitability","rcp_85_2065_suitability"),sep=".")
#       dat9<-as.data.frame(dat9)
#       colnames(dat9)[1]<-"Group.1"
#       dat9$x<-NA  } else {   
#       
#         dat1b<-na.omit(cbind(obs_state,dat1))
#           dat1b$time<-"current_suitability"
#         dat2b<-na.omit(cbind(obs_state,dat2))
#           dat2b$time<-"rcp_45_2035_suitability"
#         dat3b<-na.omit(cbind(obs_state,dat3))
#           dat3b$time<-"rcp_45_2065_suitability"
#         dat4b<-na.omit(cbind(obs_state,dat4))
#           dat4b$time<-"rcp_85_2035_suitability"
#         dat5b<-na.omit(cbind(obs_state,dat5))
#           dat5b$time<-"rcp_85_2065_suitability"
#         
#           colnames(dat1b)[4] <- "obs"
#           colnames(dat2b)[4] <- "obs"
#           colnames(dat3b)[4] <- "obs"
#           colnames(dat4b)[4] <- "obs"
#           colnames(dat5b)[4] <- "obs"
#          
#           dat6b<-rbind(dat1b,dat2b,dat3b,dat4b,dat5b)
#           dat6b<-dat6b[,c("STATE","obs","time")]
#           dat6b$obs<-round(dat6b$obs,2)
#           dat6b$time2<-paste(dat6b$STATE,dat6b$time,sep=".")
#           dat6b<-aggregate(dat6b[,"obs"], by=list(dat6b$time2), FUN=mean)
#           
#         dat8<-paste(rep(as.vector(unique(states$STATE)),each=5),c("current_suitability","rcp_45_2035_suitability","rcp_45_2065_suitability","rcp_85_2035_suitability","rcp_85_2065_suitability"),sep=".")
#         dat8<-as.data.frame(dat8)
#         colnames(dat8)[1]<-"Group.1"
#         dat9<-merge(dat6b,dat8,by="Group.1",all.y=TRUE)
#         
#       }
#         
#           dat7<-as.data.frame(t(dat9$x))
#           colnames(dat7)<-dat9$Group.1
#            
#           dat6<-cbind(dat6,dat7)
#       
#           obs_vals[[i]]<-dat6
#           message(i)
#           
#       }
# aa<-do.call("rbind",obs_vals)
}

#####################################################


# 
# 
# #             
# #           
# #           
# #           for (ds in 1:length(dat_sets)){}
# #           
# #           state<-unique(subset(with(dat_sets[ds]),select=STATE)
# #           #make dataframe fom species names
# #           dat<-as.data.frame(spec)
# #           
# #           #find out number of 8km grid cells species occures in
# #           for (i in 1:length(state)){
# #             sub<-subset(obs_state,STATE==state[i])
# #             sub<-as.data.frame(table(sub$species))
# #             colnames(sub)[2] <- paste(state[i],"_gridcell_pres",sep="")
# #             dat<-merge(dat,sub,by.x="spec",by.y="Var1")
# #           }
# #           
# #           
# #           
# #           
# #         global_mean_current_suitability<-mean(dat1, na.rm = TRUE)
# #           global_stdev_current_suitability<-sd(dat1, na.rm = TRUE)
# #           global_mean_rcp_45_2035_suitability<-mean(dat2, na.rm = TRUE)
# #           global_stdev_rcp_45_2035_suitability<-sd(dat2, na.rm = TRUE)
# #           global_mean_rcp_45_2065_suitability<-mean(dat3, na.rm = TRUE)
# #           global_stdev_rcp_45_2065_suitability<-sd(dat3, na.rm = TRUE)
# #           global_mean_rcp_85_2035_suitability<-mean(dat4, na.rm = TRUE)
# #           global_stdev_rcp_85_2035_suitability<-sd(dat4, na.rm = TRUE)
# #           global_mean_rcp_85_2065_current_suitability<-mean(dat5, na.rm = TRUE)
# #           global_stdev_rcp_85_2065_suitability<-sd(dat5, na.rm = TRUE)
# # 
# # 
# # 
# #           
# #           obs_vals[[i]]<-dat6
# #       }
# # obs_vals<-do.call("rbind",obs_vals)
# # #comine with previous dataframe      
# # dat<-merge(dat, obs_vals, by.x="spec",by.y="spec")
# 
# 
# 
# 
# 
# 
# ############## AVERAGE STATE SUITABILITY score of 8km grid cells where species are present under current conditions
# 
# #for each species extract the current grid cell value for location, aggregate these to find the mean (also 1 sd)
# #make blank list
# obs_vals<-list()
# for (i in 1:length(spec)){
#   #read in rurrent raster for first species
#   cur<-raster(paste(out.dir,spec[i],"_final2\\species_Current_avg.asc",sep=""))
#   #get xy and observations for species
#   x<-subset(sp_dat,species==spec[i], select="Longitude")
#   y<-subset(sp_dat,species==spec[i], select="Latitude")
#   locs2<-cbind(x,y)
#   #find mean global habitat suitability values for locations and sd
#   dat1<-(extract(cur,locs2))
#   obs_mean_current_suitability<-mean(dat1, na.rm = TRUE)
#   obs_stdev_suitability<-sd(dat1, na.rm = TRUE)
#   species<-paste(spec[i])
#   dat2<-cbind(species, obs_mean_current_suitability, obs_stdev_suitability )
#   obs_vals[[i]]<-dat2
# }
# obs_vals<-do.call("rbind",obs_vals)
# #comine with previous dataframe      
# dat<-merge(dat, obs_vals, by.x="spec",by.y="species")
# 
# # for each state number of 8km grid cells with known observations
# #get locations
# x<-(locs[,"Longitude"])
# y<-(locs[,"Latitude"])
# locs2 <- SpatialPoints(data.frame(x,y))
# #extract which states the locations are in
# obs_state<-sp::over(locs2,poly_states)
# obs_state<-na.omit(cbind(locs,obs_state))
# #get name of states
# #number of observations in Australia
# #sub<-as.data.frame(table(obs_state$species))
# #dat$AUS_gridcell_pres<-sub$Freq
# 
# 
# 
# 





# % of area that is suitbale habitat based on threshold (all time steps) (about 10% probability of presence,based on 10% omission rate (average logistic value 0.1646399, sd 0.04612315, max (.2992), min (0.0548), for each state
  
  #get the ascii files needed
  sp_ascii<-list.files("f:/Current_Projects/exotic_plants/outputs",
                       pattern="(thresholded_current_10.asc|thresholded_rcp45_2035_10.asc|thresholded_rcp45_2065_10.asc|thresholded_rcp85_2035_10.asc|thresholded_rcp85_2065_10.asc)"                     ,recursive=TRUE,full.names=TRUE,ignore.case = TRUE)
  #get centerpoint of cells for Australian states
  locs<-read.csv(paste(out.dir,"Land_Catagories\\Australian_States_point_polygon_summary.csv",sep=""))
  locations<-as.data.frame(locs)[,c("x","y")]
  locs<-locations
  
  for (ii in 1:length(sp_ascii)){
    
    rr<-raster(sp_ascii[ii])
    dat1<-as.data.frame(extract(rr,locs))
    dat1<-round(dat1,2)
    sp_name<-strsplit(strsplit(sp_ascii[ii],"outputs/")[[1]][2],"_")[[1]][1]
    run<-strsplit(strsplit(sp_ascii[ii],"final2/")[[1]][2],".asc")[[1]][1]
    
    colnames(dat1)<-paste(sp_name,run,sep=" ")
    locations<-cbind(locations,dat1)
    
    message(ii)
    
    
  }
  
  locations<-na.omit(locations)

#extract the states of x,y
  x<-(locations[,"x"])
  y<-(locations[,"y"])
  locs2 <- SpatialPoints(data.frame(x,y))
  obs_state<-sp::over(locs2,poly_states)
  locations<-cbind(obs_state,locations)

#find area of cells
  ras_area<-raster::area(raster(sp_ascii[1]))
  area_km2<-round(extract(ras_area,locs2),2)
  locations<-cbind(area_km2,locations)

state_area<-locations[,c("STATE","area_km2")]   
#write output table
write.csv(locations, "F:\\Current_Projects\\exotic_plants\\data\\website\\Australia_State_habitat_suitability_10per_omission_lat_long_all_apecies.csv",row.names = FALSE)

#multiply columns by state area
################need to do this by column


locations<-read.csv("F:\\Current_Projects\\exotic_plants\\data\\website\\Australia_State_habitat_suitability_10per_omission_lat_long_all_apecies.csv")

  
state_area <- (aggregate(locations[,"area_km2"], by=list(locations$STATE), FUN=sum))
#colnames(state_area)[2]<-"state_area"
run_name<-dimnames(locations)[[2]]

suit<-list()
for (i in 7:ncol(locations)){

a<-run_name[i]

l3<-locations[,i]*locations[,"area_km2"]
l4<-(aggregate(l3, by=list(locations$STATE), FUN=sum))
l5<-as.data.frame(round(l4$x/state_area$x*100))
colnames(l5)<-"10per_suitible"
l5$time<-paste(l4$Group.1,strsplit(strsplit(a,"\\.")[[1]][3],"_")[[1]][2],strsplit(strsplit(a,"\\.")[[1]][3],"_")[[1]][3],sep="_")

l5$species<-paste(strsplit(a,"\\.")[[1]][1],strsplit(a,"\\.")[[1]][2],sep=" ")


suit[[i]]<-l5

message(i)
}

#for species with "-" in name

for (i in c(547:551,382:386)){
  
  a<-run_name[i]
  
  l3<-locations[,i]*locations[,"area_km2"]
  l4<-(aggregate(l3, by=list(locations$STATE), FUN=sum))
  l5<-as.data.frame(round(l4$x/state_area$x*100))
  colnames(l5)<-"10per_suitible"
  l5$time<-paste(l4$Group.1,strsplit(strsplit(a,"\\.")[[1]][4],"_")[[1]][2],strsplit(strsplit(a,"\\.")[[1]][4],"_")[[1]][3],sep="_")
  
  l5$species<-paste(strsplit(a,"\\.")[[1]][1]," ", strsplit(a,"\\.")[[1]][2],"-",strsplit(a,"\\.")[[1]][3],sep="")
  
  
  suit[[i]]<-l5
  
  message(i)
}


#   
  
 locations2<-do.call("rbind",suit)
l6<-reshape(locations2, timevar="time",idvar="species",direction="wide")


dat2<-read.csv("F:\\Current_Projects\\exotic_plants\\outputs\\avg_obs_suitability.csv")


dat3<-merge(dat2,l6,by="species")


write.csv(dat3,"F:\\Current_Projects\\exotic_plants\\outputs\\avg_obs_suitability.csv",row.names = FALSE)





# % of area that is suitable habitat above 50% suitable (all time steps): "The default output is logistic, which is the easiest
#       to conceptualize: it gives an estimate between 0 and 1 of probability of presence. Note that probability
#       of presence depends on details of the sampling design, such as the plot size and (for vagile organisms)
#       observation time; logistic output estimates probability of presence assuming that the sampling design is
#       such that typical presence localities have probability of presence of about 0.5. This value of 0.5 is fairly
#       arbitrary, and can be adjusted (using the "default prevalence" parameter) if information is available on the
#       probability of presence at typical presence localities."

#
####################% of area that is suitable habitat above 50% suitable (all time steps)
#read.csv with species suitability scores
              # 
              #     suit<-read.csv("F:\\Current_Projects\\exotic_plants\\data\\website\\Australia_State_habitat_suitability_lat_long_all_apecies.csv")
              #     suit<-merge(suit,states, by =c("x","y"),all=FALSE,all.x=TRUE)
              #     state_name<-unique(suit$STATE)
              # #get cell area
              #     aus_mask<-raster("f:\\Raw Data\\Australia masks\\Australia.asc")
              #     aus_extent<-extent(112.9167,153.5833,-43.58333,-9.333333)
              #     aus_mask<-crop(aus_mask,aus_extent)
              #     grid_area<-raster::area(aus_mask,na.rm=TRUE,package="raster")
              #     #extract the cell size for our locations
              #     locs<-as.data.frame(suit)[,c("x","y")]
              #     area<-as.data.frame(extract(grid_area,locs))
              #     colnames (area)[1] <-"area_km2"
              #     area<-cbind(locs,area)
              #     #merge table with area
              #     suit2<-merge(suit,area, by =c("x","y"),all=FALSE,all.x=TRUE) 
              # #area of states
              #     state_area<-suit2[,c("STATE","area_km2")]    
              #     state_area <- (aggregate(state_area[,2], by=list(state_area$STATE), FUN=sum))
              #     colnames(state_area)[2]<-"state_area"
              #   
              # # for each species caluclate % of state with suitability of greater than 50%
````````````````````````````
###############need to do some manual cleaning
#write.csv(suitability,paste(work.dir,"data//suitability_50per.csv",sep=""),row.names = FALSE)


  #read in suitability csv
    suitability<-read.csv(paste(work.dir,"data//suitability_50per.csv",sep=""))
  #fix up names
    aa<-strsplit(as.vector(suitability$run)," ")
    aa<-do.call("rbind",aa)
    suitability$species<-paste(aa[,1],aa[,2])
    suitability$time<-paste(aa[,3],"suit=",suitability$suitability)
    suitability<-suitability[,c(1:8,11,12)]
  #change to wide format
    suitabilityb<-reshape(suitability, timevar="time",idvar="species",direction="wide")
  #combine with previus data frame
    dat<-merge(dat3, suitabilityb, by="species")



write.csv(dat,"F:\\Current_Projects\\exotic_plants\\outputs\\avg_obs_suitability.csv",row.names = FALSE)


#############################################################################################












