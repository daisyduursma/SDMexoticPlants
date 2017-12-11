

#make sure workspace is clean
  rm(list = ls())

#libs
  library(raster)
  
  
  
  
  
  #get list of species 
  
  dat<-read.csv("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_20_03_2013.csv")
  regions<-as.vector(c("ACT", "NSW" ,"NT",  "QLD" ,"SA",  "TAS" ,"VIC", "WA","AUS" ))
  outdir="f:\\Current_Projects\\exotic_plants\\outputs\\"
  
  for (i in 1:length(regions)){
  
  suit<-dat[,c("spec",paste("X10per_suitible.",regions[i],"_rcp85_2035",sep=""))]
  colnames(suit)[2]<-"area"
  obs<-subset(suit,area>0)
  
  species<-as.data.frame(obs$spec)
  colnames(species)[1]<-"species"
  species$outdir<-outdir
  species$folder<-"_final2"
  species$file<-paste(species$outdir,species$species,species$folder,"\\thresholded_RCP85_2035_10.asc",sep="")
  #make list of ascii files for timestep
  rcp_asc<-as.vector(species$file)
  
  #read in first ascii    
  a<-raster(rcp_asc[1])
  #start loop for rest of asciis
  for(j in 2:length(rcp_asc)){
    a<-raster(rcp_asc[j])+a
  }
  #write raster
  writeRaster(a,paste(outdir,"all species\\",regions[i],"_RCP85_2035_sum_all_species.asc",sep=""),
              NAflag=-9999,overwrite=TRUE)
  
  message(i)
  }
  
  
  for (i in 1:length(regions)){
    r<-raster(paste(outdir,"all species\\",regions[i],"_sum_all_species.asc",sep=""))
    plot(r)
    
  }
#   
#   
#   #create function to   
#   
# #   makeAsciiSUM <- function(i, outdir=outdir){
# #     
# #     r <- require(raster)
# #     if(!r)stop("Install raster")
#     
#     #make list of ascii files for timestep
#     rcp_asc<- list.files(outdir, pattern=paste("thresholded_",rcp_year[i],"_10.asc",sep=""), 
#                          full.name=TRUE, recursive=TRUE)
#     #read in first ascii    
#     a<-raster(rcp_asc[1])
#     #start loop for rest of asciis
#     for(j in 2:length(rcp_asc)){
#       a<-raster(rcp_asc[j])+a
#     }
#     #write raster
#     writeRaster(a,paste(outdir,"all species\\",rcp_year[i],"_sum_all_species.asc",sep=""),
#                 NAflag=-9999,overwrite=TRUE)
#     
#     plot(a)
#   }
#   
#   
#   
#   
# #get lists of asciis  
#   rcp_year<-  c( "rcp45_2035",
#   "rcp45_2065",
#   "rcp85_2035",
#   "rcp85_2065",
#   "current")
#     
#   
#   for(i in 1:length(rcp_year)){
#     message("summing rcp and year ",rcp_year[i])
#     try(makeAsciiSUM(i, 
#                    outdir="h:\\Current_Projects\\exotic_plants\\outputs\\"
#                    ))
#     
#   }
#      
#     