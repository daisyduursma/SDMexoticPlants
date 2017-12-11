
# with this script you can work through each state and the nation and find out the proportion of each area that is highly suitable for a certian species. This was not put into a function because every area has its own distinct set names, areas, etc. There are three sections to this code and the first section "#Table with species observations for each gridcell" needs to be run before the other two

#make sure workspace is clean
rm(list = ls())

r <- require(raster)
if(!r)stop("Install raster")
r <- require(sp)
if(!r)stop("Install sp")
library(maptools)


out.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\"
write.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\aa_all_species\\PrcntSUITregion\\"
scen.dir<-paste0(out.dir,"\\aa_all_species\\scenario_Pres_abs\\")
grid.dir<-"D:\\Current_Projects\\exotic_plants\\data\\GRIDS\\data_WGS_1984\\"


r1<-raster("C:\\Daisy\\Current_Projects\\exotic_plants_2\\outputs\\Acetosa sagittata\\Australia_average_rcp45_2035.asc")


locs<-rasterToPoints(r1,spatial=TRUE)
#normal data.frame of locs
locations<-as.data.frame(locs)[,c("x","y")]
locs2<-SpatialPoints(cbind(locations$x,locations$y))

#names used in loops
scen<-c("Australia_current_thresholded_high_quality.asc","Australia_average_rcp45_2035_thresholded_high_quality.asc","Australia_average_rcp45_2065_thresholded_high_quality.asc","Australia_average_rcp85_2035_thresholded_high_quality.asc","Australia_average_rcp85_2065_thresholded_high_quality.asc")
scenario<-c("current","rcp45_2035","rcp45_2065","rcp85_2035","rcp85_2065")

 
#Table with species observations for each gridcell
 
for(i in 1:length(scen)){
    #get list of files
    sp_ascii<-list.files("D:\\Current_Projects\\exotic_plants_2\\outputs\\",
                         pattern=scen[i],recursive=TRUE,full.names=TRUE)
    locations<-as.data.frame(locs)[,c("x","y")]
  #for every ascii extract the data for every xy locations
  for (ii in 1:length(sp_ascii)){
    #get the raster
    rr<-raster(sp_ascii[ii])
    #extract the data point
    dat1<-as.data.frame(extract(rr,locs))
    #get the species name
    sp_name<-strsplit(sp_ascii[ii],"/")[[1]][2]
    #set the column name
    colnames(dat1)<-paste(sp_name)
    locations<-cbind(locations,dat1)
    message(ii)
  }
    
  #write output table
  write.csv(locations,paste0(out.dir,"aa_all_species\\scenario_Pres_abs_high_quality\\",scenario[i],"_Presence_absence_lat_long_all_apecies_high_quality_11_12_3013.csv"))
}
  
############### GET DATA SUMMARY DATA FOR EACH SPECIES##################

############### for States
{
#empty list
all_scen<-list()
for(i in 1:length(scen)){
  #read in the data for all xy locations and all species
  dat<-read.csv(paste(out.dir,"aa_all_species//scenario_Pres_abs_high_quality//",scenario[i],"_Presence_absence_lat_long_all_apecies_high_quality_11_12_3013.csv",sep=""))
  #get the xy cordinates
    locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find out which states point occurs in 
    aa<-readShapePoly(paste(grid.dir,"NRM_Australia_states.shp",sep=""))
    dat$states<-over(locs2,aa)[,"STATE"]
    b<-area(r1)
    dat$grid_size<-extract(b,locs2)
  # get name of staes
    st<-as.vector(unique(dat$states))[2:9]
  #new empty list for next lope
    all_state<-list()
  #for each state find out the percent of suitable habitat
    for (j in 1:length(st)){
      sub_dat<-subset(dat,states==st[j])
      st_area<-sum(sub_dat$grid_size)
      sp_dat<-sub_dat[,4:256]
      percent_suitable<-sp_dat*sub_dat$grid_size
      percent_suitable<-(colSums(percent_suitable)/st_area)*100
      species<-dimnames(sub_dat)[[2]][4:256]
      species<-gsub("[.]"," ",species)
      year<-rep(scenario[i],253)
      presence_suitable<-apply(sp_dat,2,FUN=max)
      state<-rep(st[j],253)
      all_state[[j]]<-cbind(year,species,state,percent_suitable,presence_suitable)
      dat_files<-list.files(write.dir,full.names=TRUE)
  }
  #convert list to dataframe
  all_scen[[i]]<-do.call("rbind",all_state)
  }
#convert final list to dataframe
  final_dat<-do.call("rbind",all_scen)
write.csv(final_dat,paste0(write.dir,"state_percent_high_quality_suitable_habitat.csv"),row.names=FALSE)
}


############### for Australia

all_scen<-list()
for(i in 1:length(scen)){
  dat<-read.csv(paste(out.dir,"aa_all_species//scenario_Pres_abs_high_quality//",scenario[i],"_Presence_absence_lat_long_all_apecies_high_quality_11_12_3013.csv",sep=""))
  locs2<-SpatialPoints(cbind(dat$x,dat$y))
  #find areas
    b<-area(r1)
    dat$grid_size<-extract(b,locs2)
    st_area<-sum(dat$grid_size)
    sp_dat<-dat[,4:256]
  #calculate percent suitable habitat
    percent_suitable<-sp_dat*dat$grid_size
    percent_suitable<-(colSums(percent_suitable)/st_area)*100
    species<-dimnames(dat)[[2]][4:256]
    species<-gsub("[.]"," ",species)
    year<-rep(scenario[i],253)
    presence_suitable<-apply(sp_dat,2,FUN=max)
    Nation<-rep("Australia",253)
    all_state<-cbind(year,species,Nation,percent_suitable,presence_suitable)
  #add to list
    all_scen[[i]]<-all_state
}
final_dat<-do.call("rbind",all_scen)
write.csv(final_dat,paste0(write.dir,"Australia_percent_high_quality_suitable_habitat.csv"),row.names=FALSE)


