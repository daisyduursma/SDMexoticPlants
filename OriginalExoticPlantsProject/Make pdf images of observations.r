#packages
library(maptools)
data(wrld_simpl)

#plot
nf <- layout(matrix(c(1,2),2,1,byrow=TRUE), c(1), c(1), TRUE)
layout.show(nf)

par(mar=c(0,0,0,1))

plot(wrld_simpl,axes=FALSE,col="light gray")
par(mar=c(0,0,0,0))
plot(wrld_simpl,axes=FALSE,col="light gray")


rm(list = ls())

#load packages
	library(maptools)
	
	data(wrld_simpl)

#directorys
	work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")

#read in species data
dat<-read.csv(paste(dat.dir,"AVH_GBIF_all_unique_with_lat_long_one_per_cell.csv",sep=""))


spp<-as.vector(unique(dat$species))
	
	
for (i in 1:length(spp)){

	#get species data
		sp_dat<-subset(dat, species==spp[i])
	#set up plot
		nf <- layout(matrix(c(1,1,2,2),2,2,byrow=TRUE), c(1), c(1), TRUE)
		layout.show(nf)
		par(mar=c(0,0,0,1))
	#global plot
		plot(wrld_simpl,axes=FALSE,col="light gray")
	#add species name
		mtext(paste(spp[i]),side=3,font=2,line=-1.3)
	#add observations 
		points(sp_dat$Longitude,sp_dat$Latitude,col="red",pch=20,cex=.75)
	#Australian plot
		plot(wrld_simpl[which(wrld_simpl$NAME=="Australia"),],axes=FALSE,col="light gray")
	
	###############this is the part that clips###########
	#get points that fall with in wrld_simpl mask for Australia
		aa<-SpatialPoints(cbind(sp_dat$Longitude,sp_dat$Latitude))
		sp_dat$AUS=(!is.na(overlay(aa,wrld_simpl[which(wrld_simpl$NAME=="Australia"),])))
		AUS_dat<-subset(sp_dat,AUS=="TRUE")
	#add Australin observations
		points(AUS_dat$Longitude,AUS_dat$Latitude,col="red",pch=20,cex=.75)
	#number of observations
		mtext(paste("World obs. = ",nrow(sp_dat),"   Australia obs. = ",nrow(AUS_dat)),side = 1,line=-3)
	#save image
		dev.copy2pdf(file=paste(work.dir,"images\\",spp[i]," global_Australia_obs.pdf",sep=""))
	
		}

	# dev.copy2pdf(file=paste("C:\\Daisy\\Current Projects\\Grass Paper\\images\\",species[i],"_obs_koeppen_zones.pdf",sep=""))
	#points(bg,col="black",cex=0.75)