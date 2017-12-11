

rm(list = ls())



getSpeciesClimate <- function(species, worldclim_dataloc = "c:/data/worldclim",
                              writefile=TRUE, outdir="", gbifOnly=FALSE){


  library(raster)
  library(dismo)
  library(XML)
  library(rgdal)

  sp <- strsplit(species, " ")[[1]]
  gbifall <- gbif(sp[1], sp[2], removeZeros=TRUE, download=TRUE, geo=TRUE)
  
  if(is.null(gbifall))return()
    
  # remove duplicates
  gbifdat <- gbifall[!duplicated(paste(gbifall$lat,gbifall$lon,sep="-")),]
  
  # keep only original species ('species' field in gbif), and lat/lon
  gbifdat <- gbifdat[,c("species","lat","lon","locality")]
  names(gbifdat)[1] <- "speciesFull"
  gbifdat$species <- species  # species name as input
 
  ###use only if you want to extract worldclim data
#   if(!gbifOnly){
#     coors <- SpatialPoints(gbifdat[,c("lon","lat")])
#     
#     extractVar <- function(varname, ind=1:12){
#       p <- list()
#     
#       dir <- paste0(worldclim_dataloc,"/",varname,"/")
#       
#       vars <- paste0(varname,"_", ind)
#     
#       for (i in 1:length(vars)){
#         a<-raster(paste0(dir,vars[i]))
#         dataVal <- extract(a,coors)
#         p[[i]] <- dataVal
#       }
#       outvars <- do.call(cbind,p)
#       names(outvars) <- vars
#       return(outvars)
#     }
#   
#     # precipitation
#     precips <- extractVar("prec")
#     preciptot <- apply(precips,1,sum)
#     gbifdat$MAP <- preciptot
#     
#     # tmean
#     tmeans <- extractVar("tmean")
#     tmeanmean <- apply(tmeans,1,mean)/10
#     gbifdat$MAT <- tmeanmean
#   }
#   
 specout <- gsub(" ","_",species)
  if(writefile)write.csv(gbifdat, paste0(outdir,specout,"_wc_gbif.csv"), row.names=FALSE)

#return(invisible(gbifdat))

}



