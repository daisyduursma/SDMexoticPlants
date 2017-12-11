
work.dir<-"/data2/home/dduursma/exotic_plants/"
out.dir<-paste(work.dir,"/outputs/",sep="")

a<-list.files(out.dir,pattern="_final",full.names=TRUE)

pres <- list()
for(i in 1:length(a)){
  
  b<-length(list.files(paste(a[i])))
  
  pres[[i]]<-paste(b)
  
}

cc<-as.data.frame(do.call("rbind",pres))
cc$file<-a
dd<-strsplit(cc$file,"outputs//")[[2]
                                  
                                  
                                  
                                  subset(cc,V1 != 116)
                                  
                                  
                                  sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
                                  
                                  #make list of species
                                  species <-as.vector(unique(sp_dat$species))
                                  
                                  
                                  sp_dat<- read.table(paste(work.dir,"data/", "observations_05_11_12_8km_grid_all_worldclimvar_sub_eur_usa.csv", sep=""),header=TRUE, sep=",")
                                  > 
                                    > #make list of species
                                    > spec <-as.vector(unique(sp_dat$species))
                                  > 
                                    > 
                                    > dd<-list.files(out.dir, pattern="_final")
                                  > ee<-strsplit(dd,"_final")
                                  > ff<-unlist(ee)
                                  > ff<- ff[ff!=""]
                                  
                                  done01 <- ifelse(spec %in% ff, 1, 0)
                                  
                                  > gg<-cbind(spec,done01)
                                  > species<-as.vector(subset(gg,done01==0,select="spec"))
                                  > 