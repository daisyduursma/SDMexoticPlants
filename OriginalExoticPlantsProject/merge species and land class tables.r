

#make sure workspace is clean
rm(list = ls())
library(doBy)
library(raster)

out.dir<-"f:\\Current_Projects\\exotic_plants\\outputs\\"

#get list of land tables
land_tab<-list.files( paste(out.dir,"Land_Catagories\\",sep=""),"_point_polygon_summary.csv",full.names=TRUE)
#get table of species ocurrance for each lat and long
sp_table<-read.csv(paste(out.dir,"all species\\ Presence_absence_lat_long_all_apecies.csv",sep=""))
#round lat and longs to 5 didgets
sp_table$x<-round(sp_table$x,5)
sp_table$y<-round(sp_table$y,5)
#read in ascii to get area of species
aus_mask<-raster("f:\\Current_Projects\\exotic_plants\\outputs\\Viburnum tinus_final2\\current_10.asc")
grid_area<-raster::area(aus_mask,na.rm=TRUE,package="raster")
locs<-rasterToPoints(grid_area,spatial=TRUE)
rm("aus_mask","grid_area")
#normal data.frame of locs
locs<-as.data.frame(locs)
locs$x<-round(locs$x,5)
locs$y<-round(locs$y,5)
colnames(locs)[1]<-"area_km2"


#################################
#CAPAD
#################################
  #presnece
  #CAPAD DAtA
    dat<-read.csv(paste(out.dir,"Land_Catagories\\CAPAD_point_polygon_summary.csv",sep=""))
    dat1<-dat[,c("x","y","PA_ID")]
    #unique rows for each PA_ID
    dat$dups<-duplicated(dat$PA_ID)
    CAPAD_Unique<-subset(dat,dups==FALSE)[,c("PA_ID","NAME","TYPE","STATE")]
    #round CAPAD lat and longs to 5 digets
    dat1$x<-round(dat1$x,5)
    dat1$y<-round(dat1$y,5)
    CAPAD<-merge(dat1, sp_table,by=c("x","y"))
    #find max for each species in each area 
    ag <- aggregate(CAPAD[,5:ncol(CAPAD)], by=list(CAPAD$PA_ID), FUN=max)
    colnames(ag)[1]<-"PA_ID"
    #recombine for other information
    area_max<-merge(CAPAD_Unique,ag,by="PA_ID")
    write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\CAPAD_species_presence.csv")
         
  # % occumpied
    #set up dataframes
    CAPAD2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
    CAPAD3<-merge(CAPAD2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
    #multiply by cell area
    cell_area<-CAPAD3$"area_km2"
    CAPAD4<-apply(CAPAD3[,6:ncol(CAPAD3)],2,function(y)y*cell_area)
    #aggregate to get for each PA_ID area sum
    ag2 <- aggregate(area_km2 ~ PA_ID, data=CAPAD3, FUN=sum)
    ag4 <- aggregate(CAPAD4, by=list(CAPAD3$PA_ID), FUN=sum)
    #combine to get percentage 
    CAPAD5<-apply(ag4[,2:ncol(ag4)],2,function(z)(round(z/ag2[,2]*100)))
    CAPAD6<-as.data.frame(cbind(as.vector(ag2$PA_ID),CAPAD5))
    #reasign column name              
    colnames(CAPAD6)[1]<-"PA_ID"
    #recombine for other information
    area_prop<-merge(CAPAD_Unique,CAPAD6,by="PA_ID")             
    write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\CAPAD_species_percent_occupied.csv")
                



##############################
#CAPAD restricted
##############################
    #presnece
    #CAPAD  restricted DAtA
    dat<-read.csv(paste(out.dir,"Land_Catagories\\CAPAD_restrictedt _point_polygon_summary.csv",sep=""))
    dat1<-dat[,c("x","y","PA_ID")]
    #unique rows for each PA_ID
    dat$dups<-duplicated(dat$PA_ID)
    CAPAD_Unique<-subset(dat,dups==FALSE)[,c("PA_ID","NAME","TYPE","STATE")]
    #round CAPAD lat and longs to 5 digets
    dat1$x<-round(dat1$x,5)
    dat1$y<-round(dat1$y,5)
    CAPAD<-merge(dat1, sp_table,by=c("x","y"))
    #find max for each species in each area 
    ag <- aggregate(CAPAD[,5:ncol(CAPAD)], by=list(CAPAD$PA_ID), FUN=max)
    colnames(ag)[1]<-"PA_ID"
    #recombine for other information
    area_max<-merge(CAPAD_Unique,ag,by="PA_ID")
    write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\CAPAD_restricted_species_presence.csv")
    
    # % occumpied
    #set up dataframes
    CAPAD2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
    CAPAD3<-merge(CAPAD2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
    #multiply by cell area
    cell_area<-CAPAD3$"area_km2"
    CAPAD4<-apply(CAPAD3[,6:ncol(CAPAD3)],2,function(y)y*cell_area)
    #aggregate to get for each PA_ID area sum
    ag2 <- aggregate(area_km2 ~ PA_ID, data=CAPAD3, FUN=sum)
    ag4 <- aggregate(CAPAD4, by=list(CAPAD3$PA_ID), FUN=sum)
    #combine to get percentage 
    CAPAD5<-apply(ag4[,2:ncol(ag4)],2,function(z)(round(z/ag2[,2]*100)))
    CAPAD6<-as.data.frame(cbind(as.vector(ag2$PA_ID),CAPAD5))
    #reasign column name              
    colnames(CAPAD6)[1]<-"PA_ID"
    #recombine for other information
    area_prop<-merge(CAPAD_Unique,CAPAD6,by="PA_ID")             
    write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\CAPAD_restricted_species_percent_occupied.csv")


##############################
#IBRA 7
##############################

dat<-read.csv(paste(out.dir,"Land_Catagories\\IBRA7_regions _point_polygon_summary.csv",sep=""))
dat1<-dat[,c("x","y","REG_NAME_7")]
#unique rows for each REG_NAME_7
dat$dups<-duplicated(dat$REG_NAME_7)
cols_Unique<-subset(dat,dups==FALSE)[,c("REG_NAME_7","REG_CODE_7","REG_CODE_6","REG_NAME_6")]
#round CAPAD lat and longs to 5 digets
dat1$x<-round(dat1$x,5)
dat1$y<-round(dat1$y,5)
locations<-merge(dat1, sp_table,by=c("x","y"))
#find max for each species in each area 
ag <- aggregate(locations[,5:ncol(locations)], by=list(locations$REG_NAME_7), FUN=max)
colnames(ag)[1]<-"REG_NAME_7"
#recombine for other information
area_max<-merge(cols_Unique,ag,by="REG_NAME_7")
write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\IBRA7_species_presence.csv")


# % occumpied
#set up dataframes
locations2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
locations2<-merge(locations2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
#multiply by cell area


area_name<-as.vector(cols_Unique$"REG_NAME_7")
locations2b<-list()
for (i in 1:length(area_name)){
  
  sub_locs_a<-subset(locations2,REG_NAME_7==area_name[i])
  cell_area<-sub_locs_a$"area_km2"
  sub_locs_b<-apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area)
  
  #aggregate to get for each REG_NAME_7 area sum
  ag2 <- aggregate(area_km2 ~ REG_NAME_7, data=sub_locs_a, FUN=sum)
   ag4<- as.data.frame(t(as.data.frame(apply(sub_locs_b,2,FUN=sum))))
  #ag4 <- aggregate(locations4[,6:ncol(locations2)], by=list(sub_locs_b$REG_NAME_7), FUN=sum)
  #combine to get percentage 
  locations5<-as.data.frame(t(as.data.frame(apply(ag4,2,function(z)(round(z/ag2[,2]*100))))))
  #locations6<-t(as.data.frame(cbind(as.vector(ag2$REG_NAME_7),locations5)))
  #reasign column name
  locations5$"REG_NAME_7"<-as.vector(ag2$"REG_NAME_7")
    
  locations2b[[i]]<-locations5
}

locations6<-do.call("rbind",locations2b)


#recombine for other information
area_prop<-merge(cols_Unique,locations6,by="REG_NAME_7")             
write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\IBRA7_species_percent_occupied.csv")


#################################
#LGA
################################
# land_tab[4]
# dat1<-read.csv(land_tab[4])
# LGA<-merge(dat1, sp_table,by=c("x","y"),all.x=TRUE)
# 
# LGA_CODE11

dat<-read.csv(paste(out.dir,"Land_Catagories\\LGA_point_polygon_summary.csv",sep=""))
dat1<-dat[,c("x","y","LGA_CODE11")]
#unique rows for each LGA_CODE11
dat$dups<-duplicated(dat$LGA_CODE11)
cols_Unique<-subset(dat,dups==FALSE)[,c("LGA_CODE11","STATE_CODE","LGA_NAME11")]
#round CAPAD lat and longs to 5 digets
dat1$x<-round(dat1$x,5)
dat1$y<-round(dat1$y,5)
locations<-merge(dat1, sp_table,by=c("x","y"))
#find max for each species in each area 
ag <- aggregate(locations[,5:ncol(locations)], by=list(locations$LGA_CODE11), FUN=max)
colnames(ag)[1]<-"LGA_CODE11"
#recombine for other information
area_max<-merge(cols_Unique,ag,by="LGA_CODE11")
write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\LGA_species_presence.csv")


# % occumpied
#set up dataframes
locations2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
locations2<-merge(locations2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
#multiply by cell area


area_name<-as.vector(cols_Unique$"LGA_CODE11")
locations2b<-list()
for (i in 1:length(area_name)){
  
  sub_locs_a<-subset(locations2,LGA_CODE11==area_name[i])
  cell_area<-sub_locs_a$"area_km2"
  #get cell area for where species occur
  
  ifelse(nrow(sub_locs_a)>1,sub_locs_b<-as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area)),sub_locs_b<-t(as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area))))
 
  #aggregate to get for each LGA_CODE11 area sum
  ag2 <- aggregate(area_km2 ~ LGA_CODE11, data=sub_locs_a, FUN=sum)
 
  ag4<- as.data.frame(t(as.data.frame(apply(sub_locs_b,2,FUN=sum))))
  #ag4 <- aggregate(locations4[,6:ncol(locations2)], by=list(sub_locs_b$LGA_CODE11), FUN=sum)
  #combine to get percentage 
  locations5<-as.data.frame(t(as.data.frame(apply(ag4,2,function(z)(round(z/ag2[,2]*100))))))
  #locations6<-t(as.data.frame(cbind(as.vector(ag2$LGA_CODE11),locations5)))
  #reasign column name
  locations5$"LGA_CODE11"<-as.vector(ag2$"LGA_CODE11")
  
  locations2b[[i]]<-locations5
}

locations6<-do.call("rbind",locations2b)


#recombine for other information
area_prop<-merge(cols_Unique,locations6,by="LGA_CODE11")             
write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\LGA_species_percent_occupied.csv")


#################################
#NRM
################################

dat<-read.csv(paste(out.dir,"Land_Catagories\\NRM_point_polygon_summary.csv",sep=""))
dat1<-dat[,c("x","y","NRM_REGION")]
#unique rows for each NRM_REGION
dat$dups<-duplicated(dat$NRM_REGION)
cols_Unique<-subset(dat,dups==FALSE)[,c("NRM_REGION","STATE","NRM_BODY","AREA_DESC")]
#round CAPAD lat and longs to 5 digets
dat1$x<-round(dat1$x,5)
dat1$y<-round(dat1$y,5)
locations<-merge(dat1, sp_table,by=c("x","y"))
#find max for each species in each area 
ag <- aggregate(locations[,5:ncol(locations)], by=list(locations$NRM_REGION), FUN=max)
colnames(ag)[1]<-"NRM_REGION"
#recombine for other information
area_max<-merge(cols_Unique,ag,by="NRM_REGION")
write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\NRM_species_presence.csv")



# % occumpied
#set up dataframes
locations2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
locations2<-merge(locations2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
#multiply by cell area


area_name<-as.vector(cols_Unique$"NRM_REGION")
locations2b<-list()
for (i in 1:length(area_name)){
  
  sub_locs_a<-subset(locations2,NRM_REGION==area_name[i])
  cell_area<-sub_locs_a$"area_km2"
  
  ifelse(nrow(sub_locs_a)>1,sub_locs_b<-as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area)),sub_locs_b<-t(as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area))))


  #aggregate to get for each NRM_REGION area sum
  ag2 <- aggregate(area_km2 ~ NRM_REGION, data=sub_locs_a, FUN=sum)
  ag4<- as.data.frame(t(as.data.frame(apply(sub_locs_b,2,FUN=sum))))
  #ag4 <- aggregate(locations4[,6:ncol(locations2)], by=list(sub_locs_b$NRM_REGION), FUN=sum)
  #combine to get percentage 
  locations5<-as.data.frame(t(as.data.frame(apply(ag4,2,function(z)(round(z/ag2[,2]*100))))))
  #locations6<-t(as.data.frame(cbind(as.vector(ag2$NRM_REGION),locations5)))
  #reasign column name
  locations5$"NRM_REGION"<-as.vector(ag2$"NRM_REGION")
  
  locations2b[[i]]<-locations5
}

locations6<-do.call("rbind",locations2b)

#recombine for other information
area_prop<-merge(cols_Unique,locations6,by="NRM_REGION")             

write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\NRM_species_percent_occupied.csv")

###############################################

#RAMSAR
# RAMSAR_NAM

dat<-read.csv(paste(out.dir,"Land_Catagories\\ramsar_point_polygon_summary.csv",sep=""))
dat1<-dat[,c("x","y","RAMSAR_NAM")]
#unique rows for each RAMSAR_NAM
dat$dups<-duplicated(dat$RAMSAR_NAM)
cols_Unique<-subset(dat,dups==FALSE)[,c("RAMSAR_NAM","WETLAND_NA","STATE")]
#round CAPAD lat and longs to 5 digets
dat1$x<-round(dat1$x,5)
dat1$y<-round(dat1$y,5)
locations<-merge(dat1, sp_table,by=c("x","y"))
#find max for each species in each area 
ag <- aggregate(locations[,5:ncol(locations)], by=list(locations$RAMSAR_NAM), FUN=max)
colnames(ag)[1]<-"RAMSAR_NAM"
#recombine for other information
area_max<-merge(cols_Unique,ag,by="RAMSAR_NAM")
write.csv(area_max,"F:\\Current_Projects\\exotic_plants\\data\\website\\RAMSAR_species_presence.csv")


# % occumpied
#set up dataframes
locations2<-merge(dat1, locs,by=c("x","y"),all = FALSE,all.x = TRUE)  
locations2<-merge(locations2, sp_table,by=c("x","y"),all = FALSE,all.x = TRUE)
#multiply by cell area


area_name<-as.vector(cols_Unique$"RAMSAR_NAM")
locations2b<-list()
for (i in 1:length(area_name)){
  
  sub_locs_a<-subset(locations2,RAMSAR_NAM==area_name[i])
  cell_area<-sub_locs_a$"area_km2"
  #get cell area for where species occur
  
  ifelse(nrow(sub_locs_a)>1,sub_locs_b<-as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area)),sub_locs_b<-t(as.matrix(apply(sub_locs_a[,6:ncol(sub_locs_a)],2,function(y)y*cell_area))))
  
  #aggregate to get for each RAMSAR_NAM area sum
  ag2 <- aggregate(area_km2 ~ RAMSAR_NAM, data=sub_locs_a, FUN=sum)
  
  ag4<- as.data.frame(t(as.data.frame(apply(sub_locs_b,2,FUN=sum))))
  #ag4 <- aggregate(locations4[,6:ncol(locations2)], by=list(sub_locs_b$RAMSAR_NAM), FUN=sum)
  #combine to get percentage 
  locations5<-as.data.frame(t(as.data.frame(apply(ag4,2,function(z)(round(z/ag2[,2]*100))))))
  #locations6<-t(as.data.frame(cbind(as.vector(ag2$RAMSAR_NAM),locations5)))
  #reasign column name
  locations5$"RAMSAR_NAM"<-as.vector(ag2$"RAMSAR_NAM")
  
  locations2b[[i]]<-locations5
}

locations6<-do.call("rbind",locations2b)


#recombine for other information
area_prop<-merge(cols_Unique,locations6,by="RAMSAR_NAM")             
write.csv(area_prop,"F:\\Current_Projects\\exotic_plants\\data\\website\\RAMSAR_species_percent_occupied.csv")















