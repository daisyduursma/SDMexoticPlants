

rm(list = ls())

work.dir<-"E://Exotics_Maxent_output//"

out.dir<-paste(work.dir,"SUMMARY_ASCII//",sep="")

a<-list.files(work.dir,pattern="agreement_72_per_rcp45_2035",recursive=TRUE,full.names= TRUE)
s1<-stack(a)
sum_rcp45_2035<- calc(s1, sum)
writeRaster(sum_rcp45_2035,paste(out.dir,"26_sp_rcp45_2035_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


b<-list.files(work.dir,pattern="agreement_72_per_rcp45_2065",recursive=TRUE,full.names= TRUE)
s1<-stack(b)
sum_rcp45_2065<- calc(s1, sum)
writeRaster(sum_rcp45_2065,paste(out.dir,"26_sp_rcp45_2065_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)

c<-list.files(work.dir,pattern="agreement_72_per_rcp85_2035",recursive=TRUE,full.names= TRUE)
s1<-stack(c)
sum_rcp85_2035<- calc(s1, sum)
writeRaster(sum_rcp85_2035,paste(out.dir,"26_sp_rcp85_2035_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


d<-list.files(work.dir,pattern="agreement_72_per_rcp85_2065",recursive=TRUE,full.names= TRUE)
s1<-stack(d)
sum_rcp85_2065<- calc(s1, sum)
writeRaster(sum_rcp85_2065,paste(out.dir,"26_sp_rcp85_2065_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


e<-list.files(work.dir,pattern="current_threshold",recursive=TRUE,full.names= TRUE)
s1<-stack(e)
sum_cur<- calc(s1, sum)
writeRaster(sum_cur,paste(out.dir,"57_sp_cur_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)





################################################



work.dir<-"/data2/home/dduursma/exotic_plants/outputs/"
out.dir<-paste(work.dir,"SUMMARY_ASCII/",sep="")

a<-list.files(work.dir,pattern="agreement_72_per_rcp45_2035",recursive=TRUE,full.names= TRUE)
s1<-stack(a)
sum_rcp45_2035<- calc(s1, sum)
writeRaster(sum_rcp45_2035,paste(out.dir,"57_sp_rcp45_2035_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


b<-list.files(work.dir,pattern="agreement_72_per_rcp45_2065",recursive=TRUE,full.names= TRUE)
s1<-stack(b)
sum_rcp45_2065<- calc(s1, sum)
writeRaster(sum_rcp45_2065,paste(out.dir,"57_sp_rcp45_2065_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)

c<-list.files(work.dir,pattern="agreement_72_per_rcp85_2035",recursive=TRUE,full.names= TRUE)
s1<-stack(c)
sum_rcp85_2035<- calc(s1, sum)
writeRaster(sum_rcp85_2035,paste(out.dir,"57_sp_rcp85_2035_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


d<-list.files(work.dir,pattern="agreement_72_per_rcp85_2065",recursive=TRUE,full.names= TRUE)
s1<-stack(d)
sum_rcp85_2065<- calc(s1, sum)
writeRaster(sum_rcp85_2065,paste(out.dir,"57_sp_rcp85_2065_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)


e<-list.files(work.dir,pattern="current_threshold",recursive=TRUE,full.names= TRUE)
s1<-stack(e)
sum_cur<- calc(s1, sum)
writeRaster(sum_cur,paste(out.dir,"57_sp_cur_75_agree.asc",sep=""),NAflag=-9999,overwrite=TRUE)







