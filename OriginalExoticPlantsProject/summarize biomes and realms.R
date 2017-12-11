rm(list = ls())

library(raster)
library(maptools)
work.dir<-"c:\\daisy\\Current_Projects\\exotic_plants\\outputs\\all species\\"
poly_states<-states<-readShapePoly("c:\\daisy\\Current_Projects\\exotic_plants\\data\\GRIDS\\Bioregions.shp")
scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")
grids<-list.files(paste(work.dir),pattern="_10_sum_all_species.asc",full.names=TRUE)
#get lat and longs of raster
  r<-raster(paste(grids[1],sep=""))
  r2<-area(r)
  locs<-rasterToPoints(r,spatial=TRUE)
  locations<-as.data.frame(locs)[,c("x","y")]

#quantiles to divide data
  qu <- c(0,0.75,1)

  
#extract data for 5 time periods and shapefile and area
    dat2<-over(locs,poly_states) 
    dat2$grid_size<-extract(r2,locs)
for(i in 1:length(grids)){
      dat2$grid<-extract(raster(grids[i]),locs)
      grid_name<-strsplit(strsplit(grids[i],"AUS_thresholded_")[[1]][2],"_10_sum")[[1]][1]
      colnames(dat2)[ncol(dat2)]<-paste(grid_name)
      
}

#get Australian values

sub_dat<-dat2[,23:27]
a<-t(colMeans(sub_dat))
b<-t(apply(sub_dat,2, sd))
fin_dat<-as.data.frame(cbind(a,b))
colnames(fin_dat)<-c("current_mean","rcp45_2035_mean","rcp45_2065_mean","rcp85_2035_mean","rcp85_2065_mean","current_sd","rcp45_2035_sd","rcp45_2065_sd","rcp85_2035_sd","rcp85_2065_sd")
fin_dat$Region<-"Australia"


sub_dat<-dat2
area_change<-list()
for (jj in 2:length(scenario)){
  qu_dat<-sub_dat[,"current"]
  xcut <- quantile(qu_dat, qu,na.rm=TRUE)
  #keep top 75% data  
  #current conditions
  sub_dat$keep<-sub_dat[,"current"]>round(xcut[2])
  sub_dat2<-subset(sub_dat,keep==TRUE)
  cur_area<-sum(sub_dat2$grid_size)
  
  sub_dat$keep<-sub_dat[,scenario[jj]]>round(xcut[2])
  sub_dat2<-subset(sub_dat,keep==TRUE)
  scen_area<-sum(sub_dat2$grid_size)
  change<-(scen_area-cur_area)*100/cur_area
  area_change[[jj]]<-change
}

ss<-as.data.frame(do.call("cbind",area_change))
colnames(ss)<-c("rcp45_2035_change","rcp45_2065_change","rcp85_2035_change","rcp85_2065_change")
ss$current_area<-cur_area
fin_dat<-cbind(fin_dat,ss)


####BIOMES

#keep only lat and longs for locations where ther is biome information
dat2$remove<-is.na(dat2$BIOME)
dat3<-subset(dat2,remove==FALSE)
dat3<-dat3[,1:ncol(dat3)-1]

st<-as.vector(unique(dat3$BIOME))

all_state<-list()
for (j in 1:length(st)){
    sub_dat<-subset(dat3,BIOME==st[j])
    sub_dat2<-sub_dat[,23:27]
    a<-t(colMeans(sub_dat2))
    b<-t(apply(sub_dat2,2, sd))
    fin_dat2<-as.data.frame(cbind(a,b))
    colnames(fin_dat2)<-c("current_mean","rcp45_2035_mean","rcp45_2065_mean","rcp85_2035_mean","rcp85_2065_mean","current_sd","rcp45_2035_sd","rcp45_2065_sd","rcp85_2035_sd","rcp85_2065_sd")
    fin_dat2$Region<-paste(st[j])
    
    area_change<-list()
    for (jj in 2:length(scenario)){
      qu_dat<-sub_dat[,"current"]
      xcut <- quantile(qu_dat, qu,na.rm=TRUE)
      #keep top 75% data  
      #current conditions
      sub_dat$keep<-sub_dat[,"current"]>round(xcut[2])
      sub_dat2<-subset(sub_dat,keep==TRUE)
      cur_area<-sum(sub_dat2$grid_size)
      
      sub_dat$keep<-sub_dat[,scenario[jj]]>round(xcut[2])
      sub_dat2<-subset(sub_dat,keep==TRUE)
      scen_area<-sum(sub_dat2$grid_size)
      change<-(scen_area-cur_area)*100/cur_area
      area_change[[jj]]<-change
    }
    
    ss<-as.data.frame(do.call("cbind",area_change))
    colnames(ss)<-c("rcp45_2035_change","rcp45_2065_change","rcp85_2035_change","rcp85_2065_change")
    ss$current_area<-cur_area
    fin_dat2<-cbind(fin_dat2,ss)  
      all_state[[j]]<-fin_dat2
}
    
    fin_dat3<-do.call("rbind",all_state)

fin_dat<-rbind(fin_dat,fin_dat3)
    
    
    
############BIOREGION



#keep only lat and longs for locations where ther is biome information
dat2$remove<-is.na(dat2$ECO_NAME)
dat3<-subset(dat2,remove==FALSE)
dat3<-dat3[,1:ncol(dat3)-1]

st<-as.vector(unique(dat3$ECO_NAME))

all_state<-list()
for (j in 1:length(st)){
  sub_dat<-subset(dat3,ECO_NAME==st[j])
  sub_dat2<-sub_dat[,23:27]
  a<-t(colMeans(sub_dat2))
  b<-t(sapply(sub_dat2, sd))
  fin_dat2<-as.data.frame(cbind(a,b))
  colnames(fin_dat2)<-c("current_mean","rcp45_2035_mean","rcp45_2065_mean","rcp85_2035_mean","rcp85_2065_mean","current_sd","rcp45_2035_sd","rcp45_2065_sd","rcp85_2035_sd","rcp85_2065_sd")
  fin_dat2$Region<-paste(st[j])
  
  area_change<-list()
  for (jj in 2:length(scenario)){
    qu_dat<-sub_dat[,"current"]
    xcut <- quantile(qu_dat, qu,na.rm=TRUE)
    #keep top 75% data  
    #current conditions
    sub_dat$keep<-sub_dat[,"current"]>round(xcut[2])
    sub_dat2<-subset(sub_dat,keep==TRUE)
    cur_area<-sum(sub_dat2$grid_size)
    
    sub_dat$keep<-sub_dat[,scenario[jj]]>round(xcut[2])
    sub_dat2<-subset(sub_dat,keep==TRUE)
    scen_area<-sum(sub_dat2$grid_size)
    change<-(scen_area-cur_area)*100/cur_area
    area_change[[jj]]<-change
  }
  
  ss<-as.data.frame(do.call("cbind",area_change))
  colnames(ss)<-c("rcp45_2035_change","rcp45_2065_change","rcp85_2035_change","rcp85_2065_change")
  ss$current_area<-cur_area
  fin_dat2<-cbind(fin_dat2,ss)  
  all_state[[j]]<-fin_dat2
}

fin_dat3<-do.call("rbind",all_state)

fin_dat<-rbind(fin_dat,fin_dat3)


write.csv(fin_dat,paste0(work.dir,"biomes\\hotspots_all_regions.csv"))














    
    
  time_dat<-list()
  for (jj in 1:length(scenario)){
    qu_dat<-sub_dat[,"current"]
    xcut <- quantile(qu_dat, qu,na.rm=TRUE)
    #keep top 75% data  
    #current conditions
          sub_dat$keep<-sub_dat[,"current"]>round(xcut[2])
          sub_dat2<-subset(sub_dat,keep==TRUE)
          cur_area<-sum(sub_dat2$grid_size)
       sub_dat$keep<-sub_dat[,paste(scenario[jj])]>round(xcut[2])
       sub_dat2<-subset(sub_dat,keep==TRUE)
       area<-sum(sub_dat2$grid_size)
       avg_suit<-mean(sub_dat[,paste(scenario[jj])])
       max_suit<-max(sub_dat[,paste(scenario[jj])])
       min_suit<-min(sub_dat[,paste(scenario[jj])])
       break_75<-round(xcut[2])
       year<-scenario[jj]
       biome_name<-st[j]
    change_in_area<-(area-cur_area)*100/cur_area
      time_dat[[jj]]<-cbind(biome_name,year,area,avg_suit,max_suit,min_suit,break_75,change_in_area)  
  }
   all_state[[j]]<-do.call("rbind",time_dat)
}
  
biome_dat<-do.call("rbind",all_state)
write.csv(biome_dat,paste0(work.dir,"biomes\\hotspots_biomes.csv"))


#ecoregions

st<-as.vector(unique(dat2$ECO_NAME))

all_state<-list()
for (j in 1:length(st)){
    sub_dat<-subset(dat2,ECO_NAME==st[j])
  
  time_dat<-list()
  for (jj in 1:length(scenario)){
    qu_dat<-sub_dat[,"current"]
    xcut <- quantile(qu_dat, qu,na.rm=TRUE)
    #keep top 75% data 
          #current conditions
          sub_dat$keep<-sub_dat[,"current"]>round(xcut[2])
          sub_dat2<-subset(sub_dat,keep==TRUE)
          cur_area<-sum(sub_dat2$grid_size)
       sub_dat$keep<-sub_dat[,paste(scenario[jj])]>round(xcut[2])
       sub_dat2<-subset(sub_dat,keep==TRUE)
       area<-sum(sub_dat2$grid_size)
       avg_suit<-mean(sub_dat[,paste(scenario[jj])])
       max_suit<-max(sub_dat[,paste(scenario[jj])])
       min_suit<-min(sub_dat[,paste(scenario[jj])])
       break_75<-round(xcut[2])
       year<-scenario[jj]
       biome_name<-st[j]
      change_in_area<-(area-cur_area)*100/cur_area
      time_dat[[jj]]<-cbind(biome_name,year,area,avg_suit,max_suit,min_suit,break_75,change_in_area)  
  }
   all_state[[j]]<-do.call("rbind",time_dat)
}
  
biome_dat<-do.call("rbind",all_state)
write.csv(biome_dat,paste0(work.dir,"biomes\\hotspots_bioregions.csv"))






































   
  