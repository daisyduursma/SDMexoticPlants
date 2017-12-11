rm(list = ls())


work.dir<-"f:\\daisy\\Current_Projects\\exotic_plants\\"
out.dir<-paste(work.dir,"outputs\\",sep="")

#background data
#csv of center points of grid cells for each Australian state
states<-read.csv("f:\\daisy\\Current_Projects\\exotic_plants\\outputs\\Land_Catagories\\Australian_States_point_polygon_summary.csv")
#state polygon layer
poly_states<-readShapePoly("f:\\daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\NRM_Australia_states.shp")

state<-as.vector(unique(states$STATE))


#species observations
sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")[,c(1:3)]
x<-(sp_dat[,2])
y<-(sp_dat[,3])
locs <- SpatialPoints(data.frame(x,y))
locs<-over(locs,poly_states)
locs<-na.omit(cbind(sp_dat,locs))[,c(1:3)]

#species names
spec<-as.vector(read.csv("c:\\daisy\\Current_Projects\\exotic_plants\\data\\species_names.csv")[,1])

# for each state number of 8km grid cells with known observations
#get locations
# x<-(locs[,"Longitude"])
# y<-(locs[,"Latitude"])
# locs2 <- SpatialPoints(data.frame(x,y))
# #extract which states the locations are in
# obs_state<-sp::over(locs2,poly_states)
# obs_state<-na.omit(cbind(locs,obs_state))
# #get name of states
# state<-as.vector(unique(obs_state$STATE))
#make dataframe fom species names
dat<-as.data.frame(spec)
colnames(dat)[1]<-"Species"



#distance from suitable habitat within state to known observation
######################################
#get points for each species
#make raster of with points from distance



#function to determine distance between highly suitable habitat in each state and nearest observation
obsHABITATdist<- function (spec,r,ext,locs,dat, states,state_name,time){
  
  
  rst <- require(raster)
  if(!rst)stop("Install raster")
  s <- require(sp)
  if(!s)stop("Install sp")
  m <- require(maptools)
  if(!m)stop("Install maptools")
  #clip to extent
  r<-raster::crop(a,ext)
  #everything blow .5 becomes na
  fun <- function(x) { x[x<.5] <- NA; return(x) }
  r<- calc(r, fun)
  
  #make the raster as small as possible
  #r<-trim(r)
  #calculate distance for all points
  
  if(nrow(dat)==0){ag4<-matrix(rep(NA,8),nrow=1)} else {
    xy <- SpatialPoints(dat[,c("Longitude","Latitude")])
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
  if (length(na.omit(ag4[1,]))==0){
    ag4$aus<-NA } else{
      ag4$aus<-as.vector(min(ag4,na.rm = TRUE))  
    }
  colnames(ag4)[ncol(ag4)]<-paste0("AUS",time,"_km_observation_dist")
  
  return(ag4)
  
}

#clip by area of 50% suitability for each state (check on polygon functions, minimum of each polygon)
#get state names
state_name<-as.data.frame(unique(states$STATE))
ext<-extent(112.9167,153.5833,-43.58333,-9.333333)



#current
min_km_curent<-list()

for (jj in 1:length(spec)){
  res <- obsHABITATdist(
    spec,
    r=raster(paste(out.dir,spec[jj],"_final2\\species_current_avg.asc",sep="")),
    ext,
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
min_km_curent3<-min_km_curent2
min_km_curent3<-cbind(aa,min_km_curent3)


#rcp45_2035

min_km_rcp45_2035<-list()
for (jj in 1:length(spec)){
  try(res <- obsHABITATdist(
    spec,
    r=raster(paste(out.dir,spec[jj],"_final2\\average_rcp45_2035.asc",sep="")),
    ext,
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp45_2035"
  ))
  min_km_rcp45_2035[[jj]]<-res
  message(jj)
}

min_km_rcp45_2035<-do.call("rbind",min_km_rcp45_2035)

min_km<-cbind(min_km_curent3,min_km_rcp45_2035)


#rcp85_2035
min_km_rcp85_2035<-list()
for (jj in 1:length(spec)){
  try(res <- obsHABITATdist(
    spec,
    r=raster(paste(out.dir,spec[jj],"_final2\\average_rcp85_2035.asc",sep="")),
    ext,
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp85_2035"
  ))
  min_km_rcp85_2035[[jj]]<-res
  message(jj)
}

min_km_rcp85_2035<-do.call("rbind",min_km_rcp85_2035)
min_km<-cbind(min_km,min_km_rcp85_2035)


#rcp45_2065
min_km_rcp45_2065<-list()
for (jj in 1:length(spec)){
  try(res <- obsHABITATdist(
    spec,
    r=raster(paste(out.dir,spec[jj],"_final2\\average_rcp45_2065.asc",sep="")),
    ext,
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp45_2065"
  ))
  min_km_rcp45_2065[[jj]]<-res
  message(jj)
}

min_km_rcp45_2065<-do.call("rbind",min_km_rcp45_2065)

min_km<-cbind(min_km,min_km_rcp45_2065)


#rcp85_2065
min_km_rcp85_2065<-list()
for (jj in 1:length(spec)){
  try(res<- obsHABITATdist(
    spec,
    r=raster(paste(out.dir,spec[jj],"_final2\\average_rcp85_2065.asc",sep="")),
    ext,
    locs,
    dat=subset(locs,species==paste(spec[jj])),
    states,
    state_name,
    time="_rcp85_2065"
  ))
  min_km_rcp85_2065[[jj]]<-res
  message(jj)
}

min_km_rcp85_2065<-do.call("rbind",min_km_rcp85_2065)
min_km<-cbind(min_km,min_km_rcp85_2065)
