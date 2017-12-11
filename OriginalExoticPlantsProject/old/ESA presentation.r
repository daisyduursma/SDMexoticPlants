#packages
library(maptools)
data(wrld_simpl)

#plot
# nf <- layout(matrix(c(1,2),2,1,byrow=TRUE), c(1), c(1), TRUE)
# layout.show(nf)
# 
# par(mar=c(0,0,0,1))
# 
# plot(wrld_simpl,axes=FALSE,col="light gray")
# par(mar=c(0,0,0,0))
# plot(wrld_simpl,axes=FALSE,col="light gray")


rm(list = ls())

#load packages
	library(maptools)
	
	data(wrld_simpl)


a<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_species.csv")

b<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked.csv")

c<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked_one_per_cell.csv")

d<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv")

#directorys
	work.dir<-"C:\\Daisy\\Current Projects\\exotic plants\\"
	dat.dir<-paste(work.dir,"data\\",sep="")


spp<-as.vector(unique(d$species))
	
	
i<-119

	#get species data
		
	#set up plot
		nf <- layout(matrix(c(1,1,2,2,3,3),3,2,byrow=TRUE), c(1), c(1), TRUE)
		layout.show(nf)
		par(mar=c(0,0,0,1))


#global plot


sp_dat<-subset(b, species==spp[i])
plot(wrld_simpl,axes=FALSE,col="light gray")
points(sp_dat$Longitude,sp_dat$Latitude,col="red",pch=20,cex=.75)
mtext(paste(nrow(sp_dat)),side=3,font=2,line=-1.3)

sp_dat<-subset(c, species==spp[i])
plot(wrld_simpl,axes=FALSE,col="light gray")
points(sp_dat$Longitude,sp_dat$Latitude,col="red",pch=20,cex=.75)
mtext(paste(nrow(sp_dat)),side=3,font=2,line=-1.3)

sp_dat<-subset(d, species==spp[i])
plot(wrld_simpl,axes=FALSE,col="light gray")
points(sp_dat$Longitude,sp_dat$Latitude,col="red",pch=20,cex=.75)
mtext(paste(nrow(sp_dat)),side=3,font=2,line=-1.3)



plot of koeppen greiger
###################################
library(raster)
kg_5min <- raster("C:\\Daisy\\Raw Data\\Koeppen\\KG_masked")
# #expanded it to the extent of world clim data and mask the land area to world clim data.


plot(kg_5min,frame.plot=FALSE,main=NULL,axes = FALSE,legend=FALSE,col=rainbow)







library(raster)

r1<-raster("E:\\Exotics_Maxent_output\\Juncus_canadensis_final\\species_RCP85_giss-modeler_2035_avg.asc")

library(SDMTools)


#check out the help file for color Ramp palette, the interplote part lets you make really great color ramps to use. Sometimes you want more values to have a gray value for example

ascols = colorRampPalette(c("gray","yellow","orange","red"),interpolate="linear") 
max

par(mfrow = c(2,2),mar = c(1,1,2,1))
plot(r1,col=ascols(100))

#add some text
text (x=-100,y=100, labels = "blah blah" ,cex=1)

#Turn of title and and frame and scale bar
plot(r1,col=ascols(100),frame.plot=FALSE,main=NULL,axes = FALSE,legend=FALSE)


#use legend gradient to make you own legend... this code does not work

legend.gradient(pnts,cols=ascols(100),limits=c("0","1"),title = "% Scenarios",cex=1)

a<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_species.csv")

b<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked.csv")

c<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\old\\AVH_GBIF_all_unique_with_lat_long_8km_cell_value_masked_one_per_cell.csv")

d<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\data\\observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv")



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