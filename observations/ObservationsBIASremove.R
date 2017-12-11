
#make sure workspace is clean
rm(list = ls())
#load library 
library(dismo)
library(sp)
#get directories where data located

#set directory where data is
  work.dir<-"C:\\Daisy\\Current_Projects\\exotic_plants_2\\observationsCleaning\\"
# read in a base raster
  r <- raster("c:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
  r[]<-1
  r<-aggregate(r,fact=4)
#read in species data
  sp_dat<- read.table(paste(work.dir,"more_than_30_obs_final_obs_OnePerCell_17102013_with_country.csv", sep=""),header=TRUE, sep=",")
#get list of species
  species<-as.vector(unique(sp_dat$species))


########################################
########################################
########################################
#set up loop to remove bias and create final list of species
  sub_obs <- list()
  for (i in 1:length(species)){
   #get occurance points for one species
    remove_dat<-sp_dat$species==species[i]
    occurence<-sp_dat[remove_dat,]
    all_obs<-nrow(occurence)
  #get xy locations
    x<-occurence$lon
    y<-occurence$lat
    
  ########## EUROPE ####################  
  #find out if species have above 200 obs and if 75% of those are within Eur or NA
  #list of Eur Countries
      sub2=c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden","Finland", "Norway","Greece")
  #find out if points fall in countries of Europe or are NA
    eur_obs<-subset(occurence,cntr %in% paste(sub2) | is.na(cntr) )
  #get non Europe points
    non_eur_sub<-setdiff(occurence$X,eur_obs$X)
    non_eur_sub<-subset(occurence,X %in% paste(non_eur_sub))
    Eur<-nrow(eur_obs)
    non_Eur<-nrow(non_eur_sub)
    eur_per<-round(Eur/all_obs*100,digits=1)
    
  #########  USA   ####################
  #find out if species have above 200 obs and if 75% of those are within USA or NA
  #find out if points fall in countries of USA or are NA
    usa_obs<-subset(occurence,cntr =="United States" | is.na(cntr) )
  #get non USA points
    non_usa_sub<-setdiff(occurence$X,usa_obs$X)
    non_usa_sub<-subset(occurence,X %in% paste(non_usa_sub))
    USA<-nrow(usa_obs)
    non_USA<-nrow(non_usa_sub)
    USA_per<-round(USA/all_obs*100,digits=1)
  #combine to one line
    ob<-cbind(species[i],all_obs,Eur,USA, eur_per, USA_per)
  #put into list
    sub_obs[[i]]<-paste(ob)
  }

    
  #make dataframe, give column names and write csv
    obs_summary<-do.call("rbind",sub_obs)
    colnames(obs_summary)<-c("species","all_obs","Eur","USA", "eur_per", "USA_per")
    write.csv(obs_summary,paste0(work.dir,"Eur_USA_observation_summary.csv"),row.names=FALSE)   
    
###################################
###################################
###################################

    
 #select out species that need bias removed in Europe and USA
  obs_summary<-read.csv(paste0(work.dir,"Eur_USA_observation_summary.csv"))
  sub_dat<-subset(obs_summary,eur_per>=50)
  eur_sp<-as.vector(subset(sub_dat,all_obs>=200,select="species")$species)
  sub_dat<-subset(obs_summary,USA_per>=50)
  USA_sp<-as.vector(subset(sub_dat,all_obs>=200,select="species")$species)


################### EUR

#for species that need to be subset based on sub2 of Europe get all observations
  all_Eur_bias<-subset(sp_dat,species %in% paste(eur_sp),sep="")

#observations occuring in area of interset
  countries=c("Luxembourg","Belgium","Netherlands","Andorra","Isle of Man","United Kingdom","Germany","Ireland","France","Liechtenstein","Austria","Denmark","Spain","Sweden","Finland", "Norway","Greece")
#get only the Europe obs
  sub_Eur<-subset(all_Eur_bias,cntr %in% paste(countries,sep="") | is.na(cntr) )
#get non Eur obs
  non_eur<-setdiff(as.vector(all_Eur_bias$X),as.vector(sub_Eur$X))
  non_eur<-subset(all_Eur_bias,X %in% paste(non_eur,sep=""))[,c("species","lat","lon")]

cleaneurdat<-list()
for (i in 1:length(eur_sp)){
  
  sp_sub_Eur<-subset(sub_Eur,species==eur_sp[i])
  #find out cell value (resolution  : 0.3333334, 0.3333334)
  locs<-cbind(sp_sub_Eur$lon,sp_sub_Eur$lat)
  sp_sub_Eur$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  sp_sub_Eur$dup<-duplicated(sp_sub_Eur$cell)
  #recombine the europe and not europew observations
  subeur<-subset(sp_sub_Eur,dup=="FALSE")[,c("species","lat","lon")]
  cleaneurdat[[i]]<-subeur
 }
  cleaned_eur<-do.call("rbind",cleaneurdat)

#add back in the non_eur obs that did not need to be cleaned
 final_eur<-rbind(cleaned_eur,non_eur)

#######################USA


#for species that need to be subset based on sub2 of USA get all observations
  all_USA_bias<-subset(sp_dat,species %in% paste(USA_sp),sep="")
#observations occuring in area of interset
  countries=c("United States")
#get only the USA obs
  sub_USA<-subset(all_USA_bias,cntr %in% paste(countries,sep="") | is.na(cntr) )
#get non Eur obs
  non_USA<-setdiff(as.vector(all_USA_bias$X),as.vector(sub_USA$X))
  non_USA<-subset(all_USA_bias,X %in% paste(non_USA,sep=""))[,c("species","lat","lon")]

cleanUSAdat<-list()
for (i in 1:length(USA_sp)){
  
  sp_sub_USA<-subset(sub_USA,species==USA_sp[i])
  #find out cell value (resolution  : 0.3333334, 0.3333334)
  locs<-cbind(sp_sub_USA$lon,sp_sub_USA$lat)
  sp_sub_USA$cell<-cellFromXY(r,locs)
  #remove duplicate cell observations
  sp_sub_USA$dup<-duplicated(sp_sub_USA$cell)
  #recombine the europe and not europew observations
  subUSA<-subset(sp_sub_USA,dup=="FALSE")[,c("species","lat","lon")]
  cleanUSAdat[[i]]<-subUSA
 }
  cleaned_USA<-do.call("rbind",cleanUSAdat)
#add back in the non_eur obs that did not need to be cleaned
 final_USA<-rbind(cleaned_USA,non_USA)



#get the observations the species that do not have bias observations in USA or Europe and combine with cleaned data
  a<-subset(obs_summary,all_obs< 200)
  b<-subset(obs_summary,USA_per<50  )
  c<-subset(b,eur_per<50  )
  d<-droplevels(unique(rbind(a,c)$species))
  all_un_bias<-subset(sp_dat,species %in% paste(d),sep="")[,c("species","lat","lon")]

#put all the observations back together
  final_all<-rbind(final_USA,final_eur,all_un_bias)
#write out data
write.csv(final_all,paste0(work.dir,"Final_observation_bias_removed.csv"),row.names=FALSE)  



























