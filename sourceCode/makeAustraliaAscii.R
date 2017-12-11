
makeAUSascii<- function(sum_rcp,aus_extent,aus_mask,new.outdir="",thresh){

    aus<-mask(crop(sum_rcp,aus_extent),aus_mask)
    writeRaster(aus3,paste0(new.outdir,"Australia_",f_name,".asc",sep=""),NAflag=-9999,overwrite=TRUE)
    fun <- function(x) { x[x<thresh] <- 0; return(x) }
    aus2 <- calc(aus, fun)
writeRaster(aus2,paste0(new.outdir,"Australia_",f_name,"_10.asc"),NAflag=-9999,overwrite=TRUE)
    
    fun <- function(x) { x[x>=thresh] <- 1; return(x) }
    aus3 <- calc(aus2, fun)
    writeRaster(aus3,paste0(new.outdir,"Australia_",f_name,"_thresholded_10.asc",sep=""),NAflag=-9999,overwrite=TRUE)

}