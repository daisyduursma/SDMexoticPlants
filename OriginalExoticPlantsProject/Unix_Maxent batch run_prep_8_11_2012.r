work.dir<-"/data2/home/dduursma/exotic_plants/"
out.dir<-paste(work.dir,"outputs/final/",sep="")
dir.create(paste(out.dir))
temp.dir = paste(work.dir,"/tmp.sh.jobs/",sep="")
dir.create(paste(temp.dir))
setwd(temp.dir)


	sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
	
#make list of species
	species <-as.vector(unique(sp_dat$species))[10:12]


for(spec in species){

	zz = file(paste(spec,'.sh',sep=''),'w')
			cat('##################################\n',file=zz)
			cat('#!/bin/sh\n',file=zz)
			cat('cd $PBS_O_WORKDIR\n',file=zz)
			cat("R CMD BATCH '--args spec=",'"',spec,'" out.dir="',out.dir,'" work.dir="',work.dir,'"',"' /data2/home/dduursma/exotic_plants/r-scripts/Unix_Maxent_batch_run_3_8_11_2012.r ",
				spec,'.Rout --no-save & \n',sep='',file=zz)
			cat('##################################\n',file=zz)
		close(zz)

  system(paste('qsub -l nodes=n0001+n0002+n0003:ppn=8 ',spec,'.sh',sep=''))

		
	}



for(spec in species){
	zz = file(paste('bb.sh',sep=''),'w')
			cat('##################################\n',file=zz)
			cat('#!/bin/sh\n',file=zz)
			cat('cd $PBS_O_WORKDIR\n',file=zz)
			cat("R CMD BATCH '--args spec=",'"',spec,'" out.dir="',out.dir,'" work.dir="',work.dir,'"',"' /data2/home/dduursma/exotic_plants/r-scripts/Unix_Maxent_batch_run_3.r ",
				'bb.Rout --no-save\n',sep='',file=zz)
			cat('##################################\n',file=zz)
		close(zz)

  system(paste('qsub -l nodes=n0001+n0002+n0003:ppn=8  bb.sh',sep=''))

		
	}

	
	
	
	#make species names with out spaces
	zz<-as.vector(sp_dat$species)
	sp_dat$species<-gsub(" ","_", as.character(zz))
	write.csv(sp_dat,paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep
	
	 all_background<-read.table(paste(work.dir,"data/","RK_background_10000_05_11_12_8km_grid.csv", sep=""),header=TRUE, sep=",")
	zz<-as.vector(all_background$species)
	all_background$species<-gsub(" ","_", as.character(zz))
	write.csv(all_background,paste(work.dir,"data/","RK_background_10000_05_11_12_8km_grid.csv", sep=""))

