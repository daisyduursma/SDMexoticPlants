
rm(list = ls())

dat<-read.csv("c:\\daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version3.csv")

spec<-dat$scientific_name

sp <- strsplit(as.character(spec)," ")



sub_sp<-list()
for (i in 1:length(sp)){

a<-length(sp[[i]])

ifelse( a != 2, sub_sp[[i]]<-paste(spec[i]),sub_sp[[i]]<-NA)

}

sub_sp2<-do.call("rbind",sub_sp)
sub_sp2<-na.omit(sub_sp2)[,1]
write.csv(sub_sp2,"f:\\daisy\\Current_Projects\\exotic_plants_2\\trait database\\long_names.csv",row.names=FALSE)

####################################33
#find out if I got everything from ala and gbif
rm(list = ls())


a<-list.files("c:\\daisy\\Current_Projects\\exotic_plants_2\\old\\observationsRAW\\", pattern="_ala")
b<-list.files("c:\\daisy\\Current_Projects\\exotic_plants_2\\old\\observationsRaw\\", pattern="_gbif")

a2<-as.vector(do.call("rbind",strsplit(a,"_ala.csv")))
b2<-as.vector(do.call("rbind",strsplit(b,"_gbif.csv")))

c<-c(a2,b2)

d<-table(c)

e<-d[d!=2]
e


######################################3
# what species are still needed

rm(list = ls())


a<-list.files("F:\\daisy\\Current_Projects\\exotic_plants_2\\observations\\", pattern="_ala")
b<-list.files("F:\\daisy\\Current_Projects\\exotic_plants_2\\observations\\", pattern="_gbif")

a2<-as.vector(do.call("rbind",strsplit(a,"_ala.csv")))
b2<-as.vector(do.call("rbind",strsplit(b,"_gbif.csv")))

c<-unique(c(a2,b2))


dat<-read.csv("f:\\daisy\\Current_Projects\\exotic_plants_2\\trait database\\OEH trait database Version3.csv")

spec<-as.vector(dat[,"name"])

c<-c(c,spec)

d<-table(c)

e<-d[d<2]



