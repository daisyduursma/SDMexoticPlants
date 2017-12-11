
rm(list = ls())

library(raster)
library(car) 
library(stringr)
library(Hmisc)
library(lubridate)
library(gtools)

indir<-'~/Google Drive/PhD/Data/Observaitons/Raw/egg'

NRS<-read.csv(paste0(indir,'/BLA_NRS/NRSExtract.csv'))
NRS$YEAR<- ifelse(NRS$YEAR>2015,NA,NRS$YEAR)
NRS$MONTH<- ifelse(NRS$MONTH>12,NA,NRS$MONTH)
NRS$DAY<- ifelse(NRS$DAY>31,NA,NRS$DAY)
NRS<-with(NRS,NRS[!is.na(DAY) & !is.na(MONTH)  & !is.na(YEAR) &
                    !is.na(date) & !is.na(Lat) & !is.na(Lon),])
NRS$ID<-1:nrow(NRS)

#code for E=eggs, H = eggs hatching,B = nest being built,U = Unknown
E<-with(NRS,NRS[NRS_VISt_Event=='M' 
                | NRS_VISt_Event == 'm' 
                | lkpEvent_Event =='changeover/both off'
                | lkpEvent_Event =='male at nest'
                | NRS_VISt_Event=='F' 
                | NRS_VISt_Event == 'f' 
                | lkpEvent_Event =='female at nest'
                | lkpEvent_Event =='cold eggs'
                | NRS_VISt_Event=='u' 
                | NRS_VISt_Event == 'U'
                | NRS_VISt_Event=='x' 
                | NRS_VISt_Event == 'X'
                | lkpEvent_Event =='sex unknown at nest'
                | lkpEvent_Event =='warm eggs',])
H<-with(NRS,NRS[NRS_VISt_Event=='H' 
                | lkpEvent_Event == 'eggs hatching',])
B<-with(NRS,NRS[NRS_VISt_Event=='b' 
                |NRS_VISt_Event=='B' 
                |lkpEvent_Event == 'nest being built',])
U<-NRS[NRS$ID %nin% (c(B$ID,H$ID,E$ID)),]
E$EventCode<-'E'
H$EventCode<-'H'
B$EventCode<-'B'
U$EventCode<-'U'
NRS<-rbind(E,H,B,U) 

#if eggs are in nest give EventCode E as long as not already H
df<-subset(NRS, EGGS != '')
df<-with(df,df[EGGS !=0
               & EGGS != '0?'
               & EGGS != 'B'
               & EGGS != 'O'
               & EventCode != 'H',])
fix<-NRS[NRS$ID %in% (df$ID),]
fix$EventCode<-'E'
nofix<-NRS[NRS$ID %nin% (df$ID),]
NRS2<-rbind(fix, nofix)

#if Young are in nest give EventCode Y as long as not already H
df<-subset(NRS2, Y_IN != '')
df<-with(df,df[Y_IN !=0
               & Y_IN != '0?'
               & Y_IN != '?'
               & Y_IN != 'a'
               & Y_IN != 'O'
               & Y_IN != 'U'
               & Y_IN != 'W'
               & EventCode != 'H',])
fix<-NRS2[NRS2$ID %in% (df$ID),]
fix$EventCode<-'Y'
nofix<-NRS2[NRS2$ID %nin% (df$ID),]
NRS3<-rbind(fix, nofix)

#if young outside nest add Y to EventCode
df<-subset(NRS3, Y_OUT != '')
df<-with(df,df[Y_OUT != 0
               & Y_OUT != '0?'
               & Y_OUT != '?'
               & Y_OUT != 'O'
               & Y_OUT != 'U'
               & Y_OUT != 'W'
               & EventCode != 'H'
               & EventCode != 'Y',])
fix<-NRS3[NRS3$ID %in% (df$ID),]
fix$EventCode<-'Y'
nofix<-NRS3[NRS3$ID %nin% (df$ID),]
NRS4<-rbind(fix, nofix)

###########clean NRS data
############################################################################################
#finish cleaning egdat
NRS4$Scientific_name<- capitalize(trim(gsub('  ', ' ', NRS4$Scientific_name,
                                              fixed=TRUE)))#check for extra spaces and capatlize
NRS4<-subset(NRS4, !is.na(Lat) & !is.na(Lon) & !is.na(Scientific_name))#make sure lat and longs are given

#make sure all latitudes are negative
latfix<-subset(NRS4, Lat > 0)
latfix$lat<-latfix$Lat*-1
latgood<-subset(NRS4, Lat <= 0)
NRS4<-rbind(latfix, latgood)

#remove duplicates
NRS4<-NRS4[!duplicated(NRS4),]

#make sure years are as expected
syn<-subset(read.csv('/Users/daisy/Google Drive/PhD/Data/Observaitons/Raw/namesResolved.csv'), use ==1)
bad<-subset(read.csv('/Users/daisy/Google Drive/PhD/Data/Observaitons/Raw/namesResolved.csv'), use ==0)
NRS4<-NRS4[NRS4$Scientific_name %nin% bad$scn1,]
NRS4$startUnknown<-as.numeric(as.Date(dmy_hms(NRS4$date)))
NRS4$endUnknown<-as.numeric(as.Date(dmy_hms(NRS4$date)))
fix<-NRS4[NRS4$Scientific_name %in% syn$scn1,]
nofix<-NRS4[NRS4$Scientific_name %nin% syn$scn1,]#[c(1:8)]
fix<-merge(fix,syn,by.x = 'Scientific_name',by.y='scn1')
fix$Scientific_name <-fix$garnett_name
fix<-fix[,c( 'Scientific_name','Lat','Lon','startUnknown','endUnknown','SPEC_REF')]
nofix<-nofix[,c( 'Scientific_name','Lat','Lon','startUnknown','endUnknown','SPEC_REF')]
NRS4<-rbind(fix,nofix)
NRS4<-NRS4[!duplicated(NRS4),]

#make sure data is within continental Australia
aus<-raster('~/Google Drive/PhD/Data/Spatial/Climate/biovars/bio_1.asc',
            crs = '+proj=longlat +datum=WGS84')
xy<-cbind(NRS4$Lon,NRS4$Lat)
NRS4$outsideAustralia<-is.na(extract(aus,xy))
NRS4<-subset(NRS4,outsideAustralia==FALSE)[,c( 'Scientific_name','Lat','Lon','startUnknown','endUnknown','SPEC_REF')]

#keep only observations interested in
inc<-read.csv('/Users/daisy/Google Drive/PhD/BreedingTiming/tables/speciesOfInterest_20042015.csv')
obs<-NRS4[NRS4$Scientific_name %in% inc$Species,]


species<-as.character(unique(obs$Scientific_name))
#calculate the potential point of lay dates
fld<-list()
for(i in 1:length(species)){
  
  spDat<-subset(obs,Scientific_name==species[i])
  incub<-subset(inc,Species==species[i])[1,'IncubationMean']
  fledge<-subset(inc,Species==species[i])[1,'FledgingMean']
  clutch<-subset(inc,Species==species[i],select = 'ClutchSizeMean',drop=TRUE)
  lay<-subset(inc,Species==species[i])[1,'RateOfLay']*
    subset(inc,Species==species[i],select = 'ClutchSizeMean',drop=TRUE)
  
  
  
  #first eggdate
  #spDat$PLegg<-floor(apply(cbind(spDat$startEgg-lay,spDat$endEgg-incub-lay), 1, mean, trim = 0))
  #Hatch
  #spDat$PLhatch<-spDat$startHatch-incub-lay
  #first young
  #spDat$PLstartYoung<-spDat$startYoung-incub-lay
  #unknown
  spDat$PLUnkn_fullFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-fledge-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_halfFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/2)-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_forthFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/4)-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_thirdFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/3)-incub-lay), 1, mean, trim = 0))
  
  
  fld[[i]]<-spDat
  message(i)
}

finObs<-do.call("rbind",fld)












###############calculate first lay date

inc<-read.csv('/Users/daisy/Google Drive/PhD/BreedingTiming/tables/SpeciesOfInterest.csv')

species<-as.character(inc$Species)

obs<-NRS_eggobs[NRS_eggobs$Scientific.Name %in% species,]

fld<-list()
for(i in 1:length(species)){
  
  
  spDat<-subset(obs,Scientific.Name==species[i])
  incub<-subset(inc,Species==species[i])[1,'IncubationMean']
  fledge<-subset(inc,Species==species[i])[1,'FledgingMean']
  clutch<-subset(inc,Species==species[i],select = 'ClutchSizeMean',drop=TRUE)
  lay<-subset(inc,Species==species[i])[1,'RateOfLay']*
    subset(inc,Species==species[i],select = 'ClutchSizeMean',drop=TRUE)
  
  
  
  #first eggdate
  spDat$PLegg<-floor(apply(cbind(spDat$startEgg-lay,spDat$endEgg-incub-lay), 1, mean, trim = 0))
  #Hatch
  spDat$PLhatch<-spDat$startHatch-incub-lay
  #first young
  spDat$PLstartYoung<-spDat$startYoung-incub-lay
  #unknown
  spDat$PLUnkn_fullFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-fledge-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_halfFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/2)-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_forthFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/4)-incub-lay), 1, mean, trim = 0))
  spDat$PLUnkn_thirdFlg<-floor(apply(cbind(spDat$startUnknown-lay,spDat$startUnknown-incub-lay,spDat$startUnknown-(fledge/3)-incub-lay), 1, mean, trim = 0))
  
  
  fld[[i]]<-spDat
  message(i)
}

finObs<-do.call("rbind",fld)

for ii in 1:length(finObs){
  
  if (finObs$PLhatch[ii] != NA) {
    PL<=PLhatch[ii]
  } else {
    if (PLstartegg[ii] != NA & PLstartYoung[ii] != NA)
      PL<-floor(min(mean(PLstartegg[ii],PLendegg[ii]),PLstartYoung[ii])
  } else {
    if (PLstartegg[ii] != NA)
      PL<-floor(mean(PLstartegg[ii],PLendegg[ii]))
  } else {
    PL<-
      if (PLstartYoung[ii] != NA)
        PL<=PLstartYoung[ii]
  } else {
    ,mean(PLstartUnkn[ii],PLminUnkn[ii],PLendUnkn[ii]),na.rm=TRUE))

  }

}

#Hatch
hatch<-subset(spDat,!is.na(startHatch))
if (nrow(hatch)>0){
  hatch$FirstLay<-hatch$startHatch-incub-eggs
}
#Only 1 egg



}



