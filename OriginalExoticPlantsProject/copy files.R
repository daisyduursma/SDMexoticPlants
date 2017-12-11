

rm(list = ls())

dat.dir<-("C:\\Daisy\\Current_Projects\\exotic_plants\\outputs\\")

setwd(dat.dir)

a<-list.files(path=dat.dir,pattern="_final2",full.names=TRUE)
b<-list.files(path=a,pattern="jpg",full.names=TRUE,recursive=TRUE)

for (i in 1:length(b)){
  
  file.copy(b[i], "C:\\Daisy\\Current_Projects\\exotic_plants\\Website\\maps\\", overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  
}