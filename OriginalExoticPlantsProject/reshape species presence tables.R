
#make sure workspace is clean
rm(list = ls())

a<-list.files("f:\\Current_Projects\\exotic_plants\\data\\website\\",pattern="factor_3",full.names=TRUE)

out.dir<-"f:\\Current_Projects\\exotic_plants\\data\\website\\"

# 
# #CAPAD Restricted
# 
# ################################
try(makeDATAlong(
  obs_dat<-read.csv(paste(out.dir,"CAPAD_restricted_species_presence_factor_3.csv",sep="")),
  per_dat<-read.csv(paste(out.dir,"CAPAD_restricted_species_percent_occupied_factor_3.csv",sep="")),
  nonspecvars <- c("PA_ID","NAME","TYPE","STATE"),
  dat_name<-"CAPAD_restricted"
))



###############################

#CAPAD

################################

#dat<-read.csv(a[i])[,2:ncol(dat)]
try(makeDATAlong(
      obs_dat<-read.csv(paste(out.dir,"CAPAD_species_presence_factor_3.csv",sep="")),
      per_dat<-read.csv(paste(out.dir,"CAPAD_species_percent_occupied_factor_3.csv",sep="")),
      nonspecvars <- c("PA_ID","NAME","TYPE","STATE"),
      dat_name<-"CAPAD"
))
      
#################################
#IBRA7

################################

try(makeDATAlong(
  obs_dat<-read.csv(paste(out.dir,"IBRA7_species_presence.csv",sep="")),
  per_dat<-read.csv(paste(out.dir,"IBRA7_species_percent_occupied.csv",sep="")),
  nonspecvars <- c("REG_NAME_7","REG_CODE_7"),
  dat_name<-"IBRA7"
))

##################################
#NRM
##################################

try(makeDATAlong(
  obs_dat<-read.csv(paste(out.dir,"NRM_species_presence.csv",sep="")),
  per_dat<-read.csv(paste(out.dir,"NRM_species_percent_occupied.csv",sep="")),
  nonspecvars <- c("NRM_REGION","STATE","NRM_BODY"),
  dat_name<-"NRM"
))






##################################
#LGA
##################################

try(makeDATAlong(
  obs_dat<-read.csv(paste(out.dir,"LGA_species_presence.csv",sep="")),
  per_dat<-read.csv(paste(out.dir,"LGA_species_percent_occupied.csv",sep="")),
  nonspecvars <- c("LGA_CODE11","STATE_CODE","LGA_NAME11"),
  dat_name<-"LGA"
))

##################################
#RAMSAR
##################################

try(makeDATAlong(
  obs_dat<-read.csv(paste(out.dir,"RAMSAR_species_presence_factor_3.csv",sep="")),
  per_dat<-read.csv(paste(out.dir,"RAMSAR_species_percent_occupied_factor_3.csv",sep="")),
  nonspecvars <- c("RAMSAR_NAM","WETLAND_NA","STATE"),
  dat_name<-"RAMSAR"
))




makeDATAlong<- function ( out.dir=out.dir,obs_dat=obs_dat,per_dat=per_dat,nonspecvars=nonspecvars,dat_name=dat_name){
  
#species presence  
  dats <- split(obs_dat, 1:nrow(obs_dat))
  dat2 <- lapply(dats,function(x){

  datspec <- x[,-match(nonspecvars,names(x))]
  specnames <- names(datspec)
  presence <- unname(unlist(datspec))
  
  x2 <- x[rep(1,length(specnames)),match(nonspecvars,names(x))]
  x2$species_name <- specnames
  x2$species_presence <- presence
  return(x2)
  })

    dat3<-do.call(rbind, dat2)

#species percent occupied

  dats <- split(per_dat, 1:nrow(per_dat))
  dat2 <- lapply(dats,function(x){
  
  datspec <- x[,-match(nonspecvars,names(x))]
  specnames <- names(datspec)
  occupied <- unname(unlist(datspec))
  
  x2 <- x[rep(1,length(specnames)),match(nonspecvars,names(x))]
  x2$species_name <- specnames
  x2$percent_occupied <- occupied
  return(x2)
  
})

dat3_b<-do.call(rbind, dat2)

#merge tables together

dat4<-merge(dat3,dat3_b, by=c(paste(nonspecvars),"species_name"))

sp<-as.vector(dat4$species_name)
sp2<-strsplit(sp,"\\.")


dat4$species_name<-sapply(sp2,function(x) paste(x[1],x[2],sep=" "))
dat4$year<-sapply(sp2,function(x) x[3])
 dat4$region_type<-rep(dat_name,nrow(dat4))
  
  
  
write.csv(dat4,paste(out.dir,dat_name,"_long_format_presence_occupancy.csv",sep=""))


}

























# 
# 
# 
# 
# 
# #long_dat<-stack(dat,select=dimnames(dat)[[2]][c(2,6:ncol(dat))],NRM_REGION~dimnames(dat)[[2]][c(2,6:ncol(dat))] )
# 
# long_dat<-reshape(dat, idvar = c("NRM_REGION","STATE", "NRM_BODY","AREA_DESC"),varying=c(dimnames(dat)[[2]][c(6:10)]),direction="long")
# 
#   
#   
#   )wide <- reshape(df3, idvar = c("school","class"), Varying=direction = "wide")