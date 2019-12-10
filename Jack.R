## Note: For elevation and rainfall standard deviation (sd) calculations where more than one
## watershed polygon is used to describe one StationID the sd of the watershed is reported as
## the average of all individual watershed standard deviations. For the most accurate results
## you must dissolve all polygons by StationID's to calculate the true sd of the population.

library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)

# Set GIS working directory
wd <- "G:/evjones/GIS/ProbMonGIS/GISdata"

# Where do you want to save the outputs? 
#saveHere <- 'C:/GIS/ProbMonGIS/Jack/Results/'


# Bring in watersheds
wshdPolys <- readOGR('G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/Jack','JackWatershedsFinal_albers')
wshdSites <- readOGR('G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/Jack','JackSitesFinal_albers')
wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdList <- as.character(wshdPolys$StationID)
siteList <- as.character(wshdSites$StationID)

# Merge with original station list to combine bioregion and Year sampled information
Jack100 <- readxl::read_excel('G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/Jack/ProbMetrics_2001-2014_Jack.xlsx',sheet='Jack100')%>%
  select(StationID,Year,StationID_Trend,EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(Year_=Year)
# Double check everything matches up and no duplication
#wshd <- select(wshdPolys@data,StationID)%>%mutate(x=1)
#sites <- select(wshdSites@data,StationID,Latitude,Longitude)%>%mutate(x=1)
#wshd[duplicated(wshd$StationID),]
#sites[duplicated(sites$StationID),]

#x <- full_join(Jack100,wshd, by='StationID')
#y <- full_join(Jack100,sites, by='StationID')

# Bring in appropriate landcover layer 
landcover2001 <- raster(paste(wd,"/NLCD2001.TIF",sep=""))
landcover2006 <- raster(paste(wd,"/NLCD2006.TIF",sep=""))
landcover2011 <- raster(paste(wd,"/nlcd2011VA.TIF",sep=""))

# Bring in landcover functions for appropriate yearly analysis
source('landcoverFunctions.R')

# Critical Link (file with StationID's linked to year sampled for correct NLCD)
# This file must have fields: StationID and Year_ where StationID=DEQStationID that
#   matches input watershed StationID's and Year_ is year sampled
#criticalLink <- readxl::read_excel("C:/HardDriveBackup/ProbMon/2015/Regional_Results_Sites2015.xlsx",
#                                   sheet='EmmaGIS') %>%
#  mutate(Year=2015,Year_=Year,StationID=DEQSITEID)

criticalLink <- Jack100

#### LANDUSE CALCULATIONS
# Set up dataframe to store landcover data
template <- data.frame(VALUE_11=0,VALUE_21=0,VALUE_22=0, VALUE_23=0,VALUE_24=0
                       ,VALUE_31=0,VALUE_41=0,VALUE_42=0,VALUE_43=0,VALUE_52=0,VALUE_71=0
                       ,VALUE_81=0,VALUE_82=0,VALUE_90=0,VALUE_95=0)

# Run the functions
df <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%
  select(StationID,YearSampled,NLCD,everything())

for(i in 1:length(wshdList)){
  l <- landuseCalc(i)
  df <- rbind(df,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  df <- df[complete.cases(df[,1]),]#remove any placeholder rows
}

landusewide <- landuseDataManagement(df)

write.csv(landusewide,paste(saveHere,'landusewide.csv',sep=''))

#### RIPARIAN CALCULATIONS 
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')
# Set up dataframes to store riparian landcover data
df1 <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%select(StationID,YearSampled,NLCD,everything())
df30 <- df1
df120 <- df1

# Run riparian calculations
finalRiparian <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA
                            ,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA
                            ,RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA
                            ,RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA
                            ,RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA
                            ,RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA) 
for(i in 1:length(wshdList)){
  # Subset nhd streams by each polygon in wshdPolys
  testnhd <- nhd[wshdPolys[i,],]
  # Assign StationID to line segments pertaining to each polygon StationID
  if(length(testnhd)==0){
    blank <- data.frame(StationID=wshdList[i],YearSampled=landusewide$YearSampled[i],NLCD=landusewide$NLCD[i],
                        RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA,
                        RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA,
                        RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA,
                        RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA,
                        RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA)
    finalRiparian <- rbind(finalRiparian, blank)
  }else{
    testnhd@data$StationID <- wshdPolys@data$StationID[i]
    finalRiparian <- rbind(finalRiparian, riparianDataManagment2(i,testnhd))
  }
  finalRiparian <- finalRiparian[complete.cases(finalRiparian$StationID),]
}

Result <- merge(landusewide,finalRiparian, by=c('StationID','YearSampled','NLCD'))

write.csv(finalRiparian,paste(saveHere,'finalRiparian.csv',sep=''))
write.csv(Result,paste(saveHere,'Result1.csv',sep=''))
rm(landcover2001);rm(landcover2006);rm(landcover2011)#remove raster to increase memory availability



###### VA VPDES Calculations
# Need to rerun Jason's Discoverer query to update layer, Kristy's permit layer is questionable
vaVPDES <- readOGR(wd, layer='vpdesalbers')
# Dataframe to store permit count data
vaVPDEStemplate <- data.frame(ID=NA,FACILITY_N=NA,PERMIT_NO=NA,CLASSIFICA=NA,PERMIT_WRI=NA,MAJOR__MIN=NA
                              ,MUNICIPAL_=NA,REGION=NA,WATER_BODY=NA,RECEIVING_=NA,OUTFALL_NU=NA
                              ,LATITUDE__=NA,LATITUDE__.1=NA,LATITUDE__.2=NA,LAT=NA,LONGITUDE_=NA
                              ,LONGITUDE_.1=NA,LONGITUDE_.2=NA,LONG=NA,NEGLONG=NA,coords.x1=NA,coords.x2=NA,StationID=NA)


# Data frame to store permit data post data management step
vaVPDES1 <- data.frame(StationID=NA,MunMajor=NA,MunMinor=NA,IndMajor=NA,IndMinor=NA)

for(i in 1:length(wshdPolys)){
  p <- permitCount(i)
  dm_p <- VPDESdataManagement(p)
  vaVPDES1 <- rbind(vaVPDES1,dm_p) 
  vaVPDES1 <- vaVPDES1[complete.cases(vaVPDES1$StationID),] #remove any placeholder rows
}

# Add to final results
Result <- merge(Result,vaVPDES1, by='StationID')
write.csv(Result,paste(saveHere,'/Result2.csv', sep=''))
write.csv(vaVPDES1,paste(saveHere,'/vaVPDES1.csv', sep=''))
rm(vaVPDES)#remove shapefile to increase memory availability




#### Dam Calculations 
dams <- readOGR(wd,'dam_albers')
# Dataframe to store dam data
damsdf <- data.frame(StationID=NA,damcount=NA)


damsummary <- data.frame(StationID=NA,damcount=NA)

for(i in 1:length(wshdPolys)){
  damCountresults <- damCount(i)
  damsummary <- rbind(damsummary,damCountresults)
  damsummary <- damsummary[complete.cases(damsummary$StationID),]
}

# Add to final results
Result <- merge(Result,damsummary, by='StationID')
write.csv(Result,paste(saveHere,'/Result3.csv', sep=''))
write.csv(damsummary,paste(saveHere,'/damsummary.csv', sep=''))
rm(dams)#remove shapefile to increase memory availability




######## Stream Length & Density Calculations 
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')

streams <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,STRMLEN=NA,STRMDENS=NA)

for(i in 1:length(wshdPolys)){
  print(i)
  streams1 <- streamCalcs(i)
  streams <- rbind(streams,streams1)
  streams <- streams[complete.cases(streams$StationID),]
}

# Add to final results
streams$StationID <- as.factor(streams$StationID)
Result <- join(Result,streams, by=c('StationID','YearSampled','NLCD'))
write.csv(streams,paste(saveHere,'/streams.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results4.csv', sep=''))
rm(nhd)



############################### % Impervious Calculations ##############################################

imperv2001 <- raster(paste(wd,"/NLCD2001imp.TIF",sep=""))
imperv2006 <- raster(paste(wd,"/NLCD2006imp.TIF",sep=""))
imperv2011  <- raster(paste(wd,"/NLCD2011imp.TIF",sep=""))


# Set up dataframe to store impervious data
dfi <- data.frame(matrix(NA, ncol = 104))
names(dfi) <- c('StationID','YearSampled','NLCD',paste("PCT",c(0:100),sep=""))
templatei <- dfi[,4:104]

# Run the impervious
for(i in 1:length(wshdPolys)){
  l <- imperviousCalc(i)
  dfi <- rbind(dfi,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  dfi <- dfi[complete.cases(dfi$StationID),] #remove any placeholder rows
}

imperviousresults <- imperviousDataManagement(dfi)  
Result <- merge(Result,imperviousresults, by=c('StationID','YearSampled','NLCD'))
write.csv(imperviousresults,paste(saveHere,'impervious.csv',sep=''))  
write.csv(Result,paste(saveHere,'Result5.csv',sep=''))  




######## Elevation Calculations 
DEM <- raster(paste(wd,'/vaelevation.TIF',sep=''))
# Set up dataframe to store elevation data
elev <- data.frame(StationID=NA,ELEVMIN=NA,ELEVMAX=NA,ELEVMEAN=NA,ELEVSD=NA,ELEVRANGE=NA)

elevationCalcs <- function(x){
  print(paste(x,wshdPolys@data$StationID[x],sep=' '))
  e <-  extract(DEM, wshdPolys[x,],small=T, na.rm=F) 
  ELEVMIN <- as.numeric(sapply(e, FUN=min, na.rm=T))
  ELEVMAX <- as.numeric(sapply(e, FUN=max, na.rm=T))
  ELEVMEAN <- as.numeric(sapply(e, FUN=mean, na.rm=T))
  ELEVSD <- as.numeric(sapply(e, FUN=sd, na.rm=T))
  s <- data.frame(StationID=wshdPolys@data$StationID[x])%>%
    mutate(ELEVMIN=ELEVMIN,ELEVMAX=ELEVMAX,ELEVMEAN=ELEVMEAN,ELEVSD=ELEVSD,ELEVRANGE=ELEVMAX-ELEVMIN)
  return(s)
}

for(i in 1:length(wshdList)){ 
  e <- elevationCalcs(i)
  elev <- rbind(elev,e)
  elev <- elev[complete.cases(elev$StationID),]
}

# Add to final results
Result <- merge(Result,elev, by=c('StationID'))
write.csv(elev,paste(saveHere,'/elev.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results6.csv', sep=''))
rm(DEM)


####### Slope Calculations 
# Too large to bring in whole layer, clipped to bounding box in GIS first, exported as .TIFF from GIS
slope <-  raster(paste(wd,'/slope2010.TIF',sep=''))

# Set up dataframe to store slope data
slp <- data.frame(StationID=NA,SLPMIN=NA,SLPMAX=NA,SLPMEAN=NA,SLPSD=NA,SLPRANGE=NA)

for(i in 1:length(wshdList)){ 
  s <- slopeCalcs(i)
  slp <- rbind(slp,s)
  slp <- slp[complete.cases(slp$StationID),]
}

# Add to final results
Result <- merge(Result,slp, by='StationID')
write.csv(slp,paste(saveHere,'/slp.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results7.csv', sep=''))
rm(slope)#remove raster to increase memory availability


####### Rainfall Calculations 
# Bring in rainfall raster
rainfall <- raster(paste(wd,'/rainfall21.TIF',sep=''))
# Set up dataframe to store rainfall data
shed <- data.frame(StationID=NA,wshdRain_mmyr=NA,wshedRain_inyr=NA)
site <- data.frame(StationID=NA,siteRain_mmyr=NA,siteRain_inyr=NA)


# must loop through separately bc the shapefile object orders don't match
for(i in 1:length(wshdList)){ 
  shed_ <- rainfall_shed(i)
  shed <- rbind(shed,shed_)
  shed <- shed[complete.cases(shed$StationID),]}
for(i in 1:length(wshdSites)){
  site_ <- rainfall_site(i)
  site <- rbind(site,site_)
  site <- site[complete.cases(site$StationID),]}

rain <- merge(shed,site,by='StationID')

# Add to final results
Result <- merge(Result,rain, by='StationID')
write.csv(rain,paste(saveHere,'/rain.csv', sep=''))
write.csv(Result,paste(saveHere,'/Result8.csv', sep=''))
rm(rainfall)#remove raster to increase memory availability


########### Population Density Calculations 
# Bring in clipped block census data
pop2000 <- readOGR(wd, layer='pop2000final')
pop2010 <- readOGR(wd, layer='pop2010final')

pop <- data.frame(StationID=NA,wshdPOP2000=NA,wshdPOP2010=NA)

for(i in 1:length(wshdPolys)){
  pop_ <- popCalculation(i)
  pop <- rbind(pop,pop_)
  pop <- pop[complete.cases(pop$StationID),]
}


pop <- merge(pop,landusewide[,1:4],by='StationID')%>%
  mutate(POPDENS2000=wshdPOP2000/(totalArea_sqMile*2.58999)
         ,POPDENS2010=wshdPOP2010/(totalArea_sqMile*2.58999)
         ,POPCHG2000_2010=((POPDENS2010-POPDENS2000)/POPDENS2000)*100)%>%
  select(-c(YearSampled,NLCD,totalArea_sqMile))


# Add to final results
Result <- merge(Result,pop,by='StationID')
write.csv(Result,paste(saveHere,'Result9.csv'))
write.csv(pop,paste(saveHere,'/pop.csv', sep=''))
rm(pop2000)#remove shapefile to increase memory availability
rm(pop2010)#remove shapefile to increase memory availability


############################## Road Density Calculations ###################################################
# TigerRoad file, HUGE! can preclip in GIS to speed this up, only for more advanced R users
roads <- readOGR('C:/GIS/ProbMonGIS/Jack','Jack_roads')
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')


roaddf <- data.frame(StationID=NA, RDLEN=NA,RDLEN120=NA, wshd_sqkm=NA, area120_sqkm=NA, STXRD_CNT=NA)

for(i in 1:length(wshdPolys)){ 
  print(i)
  roaddf[i,] <- roadCalculation(i)
}

roaddf[,2:6] <- apply(roaddf[,2:6],2,function(x) as.numeric(x))
roaddf <- join_all(list(roaddf,streams[,c(1,4)]), by='StationID') %>%
  mutate(roadlength_km=RDLEN/1000,RDDENS=roadlength_km/(wshd_sqkm),
         roadlength120_km=RDLEN120/1000,RDDENS=roadlength120_km/(area120_sqkm),
         pctRoadLengthInRiparian=(roadlength120_km/roadlength_km)*100,
         streamlength_km=STRMLEN/1000, STXRD=STXRD_CNT/streamlength_km)%>%
  select(-c(wshd_sqkm,area120_sqkm,streamlength_km,roadlength_km,roadlength120_km,STRMLEN))

# Add to final results
Result <- merge(Result,roaddf, by='StationID')
write.csv(Result,paste(saveHere,'Result10.csv'))
write.csv(roaddf,paste(saveHere,'roaddf.csv', sep=''))
rm(roads)#remove shapefile to increase memory availability












































# Where do you want to save the outputs? 
saveHere <- 'C:/GIS/ProbMonGIS/Jack/JackFinal3/'


# Bring in watersheds
wshdPolys <- readOGR('C:/GIS/ProbMonGIS/Jack/JackFinal3','Jack3wshd_albers')
wshdSites <- readOGR('C:/GIS/ProbMonGIS/Jack/JackFinal3','Jack3sites_albers')
wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdList <- as.character(wshdPolys$StationID)
siteList <- as.character(wshdSites$StationID)

# Merge with original station list to combine bioregion and Year sampled information
Jack100 <- readxl::read_excel('C:/GIS/ProbMonGIS/Jack/ProbMetrics_2001-2014_Jack.xlsx',sheet='Jack100')%>%
  select(StationID,Year,StationID_Trend,EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(Year_=Year)
# Double check everything matches up and no duplication
#wshd <- select(wshdPolys@data,StationID)%>%mutate(x=1)
#sites <- select(wshdSites@data,StationID,Latitude,Longitude)%>%mutate(x=1)
#wshd[duplicated(wshd$StationID),]
#sites[duplicated(sites$StationID),]

#x <- full_join(Jack100,wshd, by='StationID')
#y <- full_join(Jack100,sites, by='StationID')

# Bring in appropriate landcover layer 
landcover2001 <- raster(paste(wd,"/NLCD2001.TIF",sep=""))
landcover2006 <- raster(paste(wd,"/NLCD2006.TIF",sep=""))
landcover2011 <- raster(paste(wd,"/nlcd2011VA.TIF",sep=""))

# Bring in landcover functions for appropriate yearly analysis
source('landcoverFunctions.R')

#### LANDUSE CALCULATIONS
# Set up dataframe to store landcover data
template <- data.frame(VALUE_11=0,VALUE_21=0,VALUE_22=0, VALUE_23=0,VALUE_24=0
                       ,VALUE_31=0,VALUE_41=0,VALUE_42=0,VALUE_43=0,VALUE_52=0,VALUE_71=0
                       ,VALUE_81=0,VALUE_82=0,VALUE_90=0,VALUE_95=0)

# Run the functions
df <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%
  select(StationID,YearSampled,NLCD,everything())

for(i in 1:length(wshdList)){
  l <- landuseCalc(i)
  df <- rbind(df,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  df <- df[complete.cases(df[,1]),]#remove any placeholder rows
}

landusewide <- landuseDataManagement(df)

write.csv(landusewide,paste(saveHere,'landusewide.csv',sep=''))

#### RIPARIAN CALCULATIONS 
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')
# Set up dataframes to store riparian landcover data
df1 <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%select(StationID,YearSampled,NLCD,everything())
df30 <- df1
df120 <- df1

# Run riparian calculations
finalRiparian <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA
                            ,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA
                            ,RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA
                            ,RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA
                            ,RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA
                            ,RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA) 
for(i in 1:length(wshdList)){
  # Subset nhd streams by each polygon in wshdPolys
  testnhd <- nhd[wshdPolys[i,],]
  # Assign StationID to line segments pertaining to each polygon StationID
  if(length(testnhd)==0){
    blank <- data.frame(StationID=wshdList[i],YearSampled=landusewide$YearSampled[i],NLCD=landusewide$NLCD[i],
                        RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA,
                        RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA,
                        RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA,
                        RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA,
                        RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA)
    finalRiparian <- rbind(finalRiparian, blank)
  }else{
    testnhd@data$StationID <- wshdPolys@data$StationID[i]
    finalRiparian <- rbind(finalRiparian, riparianDataManagment2(i,testnhd))
  }
  finalRiparian <- finalRiparian[complete.cases(finalRiparian$StationID),]
}

Result <- merge(landusewide,finalRiparian, by=c('StationID','YearSampled','NLCD'))

write.csv(finalRiparian,paste(saveHere,'finalRiparian.csv',sep=''))
write.csv(Result,paste(saveHere,'Result1.csv',sep=''))
rm(landcover2001);rm(landcover2006);rm(landcover2011)#remove raster to increase memory availability



###### VA VPDES Calculations
# Need to rerun Jason's Discoverer query to update layer, Kristy's permit layer is questionable
vaVPDES <- readOGR(wd, layer='vpdesalbers')
# Dataframe to store permit count data
vaVPDEStemplate <- data.frame(ID=NA,FACILITY_N=NA,PERMIT_NO=NA,CLASSIFICA=NA,PERMIT_WRI=NA,MAJOR__MIN=NA
                              ,MUNICIPAL_=NA,REGION=NA,WATER_BODY=NA,RECEIVING_=NA,OUTFALL_NU=NA
                              ,LATITUDE__=NA,LATITUDE__.1=NA,LATITUDE__.2=NA,LAT=NA,LONGITUDE_=NA
                              ,LONGITUDE_.1=NA,LONGITUDE_.2=NA,LONG=NA,NEGLONG=NA,coords.x1=NA,coords.x2=NA,StationID=NA)


# Data frame to store permit data post data management step
vaVPDES1 <- data.frame(StationID=NA,MunMajor=NA,MunMinor=NA,IndMajor=NA,IndMinor=NA)

for(i in 1:length(wshdPolys)){
  p <- permitCount(i)
  dm_p <- VPDESdataManagement(p)
  vaVPDES1 <- rbind(vaVPDES1,dm_p) 
  vaVPDES1 <- vaVPDES1[complete.cases(vaVPDES1$StationID),] #remove any placeholder rows
}

# Add to final results
Result <- merge(Result,vaVPDES1, by='StationID')
write.csv(Result,paste(saveHere,'/Result2.csv', sep=''))
write.csv(vaVPDES1,paste(saveHere,'/vaVPDES1.csv', sep=''))
rm(vaVPDES)#remove shapefile to increase memory availability




#### Dam Calculations 
dams <- readOGR(wd,'dam_albers')
# Dataframe to store dam data
damsdf <- data.frame(StationID=NA,damcount=NA)


damsummary <- data.frame(StationID=NA,damcount=NA)

for(i in 1:length(wshdPolys)){
  damCountresults <- damCount(i)
  damsummary <- rbind(damsummary,damCountresults)
  damsummary <- damsummary[complete.cases(damsummary$StationID),]
}

# Add to final results
Result <- merge(Result,damsummary, by='StationID')
write.csv(Result,paste(saveHere,'/Result3.csv', sep=''))
write.csv(damsummary,paste(saveHere,'/damsummary.csv', sep=''))
rm(dams)#remove shapefile to increase memory availability




######## Stream Length & Density Calculations 
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')

streams <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,STRMLEN=NA,STRMDENS=NA)

for(i in 1:length(wshdPolys)){
  print(i)
  streams1 <- streamCalcs(i)
  streams <- rbind(streams,streams1)
  streams <- streams[complete.cases(streams$StationID),]
}

# Add to final results
streams$StationID <- as.factor(streams$StationID)
Result <- join(Result,streams, by=c('StationID','YearSampled','NLCD'))
write.csv(streams,paste(saveHere,'/streams.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results4.csv', sep=''))
rm(nhd)



############################### % Impervious Calculations ##############################################

imperv2001 <- raster(paste(wd,"/NLCD2001imp.TIF",sep=""))
imperv2006 <- raster(paste(wd,"/NLCD2006imp.TIF",sep=""))
imperv2011  <- raster(paste(wd,"/NLCD2011imp.TIF",sep=""))


# Set up dataframe to store impervious data
dfi <- data.frame(matrix(NA, ncol = 104))
names(dfi) <- c('StationID','YearSampled','NLCD',paste("PCT",c(0:100),sep=""))
templatei <- dfi[,4:104]

# Run the impervious
for(i in 1:length(wshdPolys)){
  l <- imperviousCalc(i)
  dfi <- rbind(dfi,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  dfi <- dfi[complete.cases(dfi$StationID),] #remove any placeholder rows
}

imperviousresults <- imperviousDataManagement(dfi)  
Result <- merge(Result,imperviousresults, by=c('StationID','YearSampled','NLCD'))
write.csv(imperviousresults,paste(saveHere,'impervious.csv',sep=''))  
write.csv(Result,paste(saveHere,'Result5.csv',sep=''))  




######## Elevation Calculations 
DEM <- raster(paste(wd,'/vaelevation.TIF',sep=''))
# Set up dataframe to store elevation data
elev <- data.frame(StationID=NA,ELEVMIN=NA,ELEVMAX=NA,ELEVMEAN=NA,ELEVSD=NA,ELEVRANGE=NA)

elevationCalcs <- function(x){
  print(paste(x,wshdPolys@data$StationID[x],sep=' '))
  e <-  extract(DEM, wshdPolys[x,],small=T, na.rm=F) 
  ELEVMIN <- as.numeric(sapply(e, FUN=min, na.rm=T))
  ELEVMAX <- as.numeric(sapply(e, FUN=max, na.rm=T))
  ELEVMEAN <- as.numeric(sapply(e, FUN=mean, na.rm=T))
  ELEVSD <- as.numeric(sapply(e, FUN=sd, na.rm=T))
  s <- data.frame(StationID=wshdPolys@data$StationID[x])%>%
    mutate(ELEVMIN=ELEVMIN,ELEVMAX=ELEVMAX,ELEVMEAN=ELEVMEAN,ELEVSD=ELEVSD,ELEVRANGE=ELEVMAX-ELEVMIN)
  return(s)
}

for(i in 1:length(wshdList)){ 
  e <- elevationCalcs(i)
  elev <- rbind(elev,e)
  elev <- elev[complete.cases(elev$StationID),]
}

# Add to final results
Result <- merge(Result,elev, by=c('StationID'))
write.csv(elev,paste(saveHere,'/elev.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results6.csv', sep=''))
rm(DEM)


####### Slope Calculations 
# Too large to bring in whole layer, clipped to bounding box in GIS first, exported as .TIFF from GIS
slope <-  raster(paste(wd,'/slope2010.TIF',sep=''))

# Set up dataframe to store slope data
slp <- data.frame(StationID=NA,SLPMIN=NA,SLPMAX=NA,SLPMEAN=NA,SLPSD=NA,SLPRANGE=NA)

for(i in 1:length(wshdList)){ 
  s <- slopeCalcs(i)
  slp <- rbind(slp,s)
  slp <- slp[complete.cases(slp$StationID),]
}

# Add to final results
Result <- merge(Result,slp, by='StationID')
write.csv(slp,paste(saveHere,'/slp.csv', sep=''))
write.csv(Result,paste(saveHere,'/Results7.csv', sep=''))
rm(slope)#remove raster to increase memory availability


####### Rainfall Calculations 
# Bring in rainfall raster
rainfall <- raster(paste(wd,'/rainfall21.TIF',sep=''))
# Set up dataframe to store rainfall data
shed <- data.frame(StationID=NA,wshdRain_mmyr=NA,wshedRain_inyr=NA)
site <- data.frame(StationID=NA,siteRain_mmyr=NA,siteRain_inyr=NA)


# must loop through separately bc the shapefile object orders don't match
for(i in 1:length(wshdList)){ 
  shed_ <- rainfall_shed(i)
  shed <- rbind(shed,shed_)
  shed <- shed[complete.cases(shed$StationID),]}
for(i in 1:length(wshdSites)){
  site_ <- rainfall_site(i)
  site <- rbind(site,site_)
  site <- site[complete.cases(site$StationID),]}

rain <- merge(shed,site,by='StationID')

# Add to final results
Result <- merge(Result,rain, by='StationID')
write.csv(rain,paste(saveHere,'/rain.csv', sep=''))
write.csv(Result,paste(saveHere,'/Result8.csv', sep=''))
rm(rainfall)#remove raster to increase memory availability


########### Population Density Calculations 
# Bring in clipped block census data
pop2000 <- readOGR(wd, layer='pop2000final')
pop2010 <- readOGR(wd, layer='pop2010final')

pop <- data.frame(StationID=NA,wshdPOP2000=NA,wshdPOP2010=NA)

for(i in 1:length(wshdPolys)){
  pop_ <- popCalculation(i)
  pop <- rbind(pop,pop_)
  pop <- pop[complete.cases(pop$StationID),]
}


pop <- merge(pop,landusewide[,1:4],by='StationID')%>%
  mutate(POPDENS2000=wshdPOP2000/(totalArea_sqMile*2.58999)
         ,POPDENS2010=wshdPOP2010/(totalArea_sqMile*2.58999)
         ,POPCHG2000_2010=((POPDENS2010-POPDENS2000)/POPDENS2000)*100)%>%
  select(-c(YearSampled,NLCD,totalArea_sqMile))


# Add to final results
Result <- merge(Result,pop,by='StationID')
write.csv(Result,paste(saveHere,'Result9.csv'))
write.csv(pop,paste(saveHere,'/pop.csv', sep=''))
rm(pop2000)#remove shapefile to increase memory availability
rm(pop2010)#remove shapefile to increase memory availability


############################## Road Density Calculations ###################################################
# TigerRoad file, HUGE! can preclip in GIS to speed this up, only for more advanced R users
roads <- readOGR('C:/GIS/ProbMonGIS/Jack/JackFinal3','Jack3superroads')
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')


roaddf <- data.frame(StationID=NA, RDLEN=NA,RDLEN120=NA, wshd_sqkm=NA, area120_sqkm=NA, STXRD_CNT=NA)

for(i in 1:length(wshdPolys)){ 
  print(i)
  # Subset nhd streams by each polygon in wshdPolys
  testroads <- suppressWarnings(raster::intersect(roads,wshdPolys[i,])) # cut roads to watershed of interest
  wshd_sqkm <- gArea(wshdPolys[i,])*1e-6 # save area of watershed
  if(length(testroads)>0){
    RDLEN <- gLength(testroads) # calculate road length throughout watershed (in meters)
    testnhd <- suppressWarnings(raster::intersect(nhd,wshdPolys[i,])) # cut NHD to watershed of interest
    bufferNHD <- gBuffer(testnhd,width=120) # buffer all streams in watershed to riparian buffer distance
    area120_sqkm <- gArea(bufferNHD)*1e-6 # save area of 120m buffer
    roadInRiparianBuffer <- suppressWarnings(raster::intersect(roads,bufferNHD)) # cut roads to riparian buffer
    if(length(roadInRiparianBuffer)==0){
      RDLEN120 <- 0 # calculate road length in riparian buffer (in meters)
    }else{
      RDLEN120 <- gLength(roadInRiparianBuffer) # calculate road length in riparian buffer (in meters)
    }
    streamXroad <- gIntersection(testroads,testnhd) # find all stream/road crossings
    if(length(streamXroad)>0){
      STXRD_CNT <- nrow(as.matrix(streamXroad@coords))  # calculate number of stream/road crossings
    }else{
      STXRD_CNT <- 0 # set stream/road crossings to 0 if needed
    }
  }else{
    RDLEN <- 0
    RDLEN120 <- 0
    area120_sqmi <- 0
    STXRD_CNT <- 0
  }
  
  r <- cbind(StationID=wshdList[i],RDLEN,RDLEN120, wshd_sqkm, area120_sqkm,STXRD_CNT)
  roaddf[i,] <- r
}

roaddf[,2:6] <- apply(roaddf[,2:6],2,function(x) as.numeric(x))
roaddf <- join_all(list(roaddf,streams[,c(1,4)]), by='StationID') %>%
  mutate(roadlength_km=RDLEN/1000,RDDENS=roadlength_km/(wshd_sqkm),
         roadlength120_km=RDLEN120/1000,RDDENS=roadlength120_km/(area120_sqkm),
         pctRoadLengthInRiparian=(roadlength120_km/roadlength_km)*100,
         streamlength_km=STRMLEN/1000, STXRD=STXRD_CNT/streamlength_km)%>%
  select(-c(wshd_sqkm,area120_sqkm,streamlength_km,roadlength_km,roadlength120_km,STRMLEN))

# Add to final results
Result <- merge(Result,roaddf, by='StationID')
write.csv(Result,paste(saveHere,'Result10.csv'))
write.csv(roaddf,paste(saveHere,'roaddf.csv', sep=''))
rm(roads)#remove shapefile to increase memory availability













































## function but not used here bc need newest road metrics

# need to rewrite road function with new metric





# TigerRoad file, HUGE! can preclip in GIS to speed this up, only for more advanced R users
roads <- readOGR(wd,'VAsuperroads_albers')
roaddf <- data.frame(StationID=NA,RDLEN=NA,STXRD_CNT=NA)
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')

roadCalculation <- function(x){
  testroads <- raster::intersect(roads,wshdPolys[x,]) 
  RDLEN <- gLength(testroads)
  testnhd <- raster::intersect(nhd,wshdPolys[x,])
  streamXroad <- gIntersection(testroads,testnhd)
  if(length(streamXroad)>0){
    STXRD_CNT <- nrow(as.matrix(streamXroad@coords))
  }else{STXRD_CNT <- 0}
  return(data.frame(StationID=wshdPolys@data$StationID[x],cbind(RDLEN,STXRD_CNT)))
}

for(i in 1:3){#length(wshdPolys)){
  print(i,wshdPolys@data$StationID[x])
  r <- roadCalculation(i)
  roaddf <- rbind(roaddf,r)
  roaddf <- roaddf[complete.cases(roaddf$StationID),]
}

write.csv(roaddf,paste(saveHere,'/roads3.csv', sep=''))

roaddf$RDLEN <- as.numeric(roaddf$RDLEN);roaddf$STXRD_CNT <- as.numeric(roaddf$STXRD_CNT)
roads2 <- join_all(list(roaddf,landusewide[,1:2],streams3[,c(1,4)]), by='StationID')
roads3 <- mutate(roads2, roadlength_km=RDLEN/1000,RDDENS=roadlength_km/(totalArea_sqMile*2.58999)
                 ,STXRD=STXRD_CNT/streamlength_km)

# Add to final results
Result <- merge(Result,roads3[,c(1:3,7:8)], by='StationID')
write.csv(Result,paste(saveHere,'/Result10.csv', sep=''))
write.csv(roads3,paste(saveHere,'/roads3.csv', sep=''))
rm(roads)#remove shapefile to increase memory availability









































########################### VPDES Permit Calculations #########################################################
# Need to rerun Jason's Discoverer query to update layer, Kristy's permit layer is questionable
vaVPDES <- readOGR('C:/GIS/ProbMonGIS/GISdata', layer='vpdesalbers')
# Dataframe to store permit data
vaVPDES1 <- data.frame(matrix(NA, ncol = 24, nrow = 0))
columnnames <- c('ID','FACILITY_N','PERMIT_NO','CLASSIFICA','PERMIT_WRI','MAJOR__MIN','MUNICIPAL_'
                 ,'REGION','WATER_BODY','RECEIVING_','OUTFALL_NU','LATITUDE__','LATITUDE__.1'
                 ,'LATITUDE__.2','LAT','LONGITUDE_','LONGITUDE_.1','LONGITUDE_.2','LONG','NEGLONG'
                 ,'coords.x1','coords.x2','optional','StationID')
names(vaVPDES1) <- columnnames
# Loop over permit information within each watershed
for(i in 1:length(wshdPolys)){
  print(i)
  s <- vaVPDES[wshdPolys[i,],]
  if(!nrow(s@data)==0){
    vaVPDESselect <- data.frame(s)
    vaVPDESselect <- mutate(vaVPDESselect, StationID=wshdList[i])}
  if(nrow(s@data)==0){
    vaVPDESselect <- data.frame(matrix(0,ncol=24,nrow=1))
    names(vaVPDESselect) <- columnnames
    vaVPDESselect$StationID <- wshdList[i]}
  vaVPDES1 <- rbind(vaVPDES1,vaVPDESselect)
}
# Mark permits according to classification
vaVPDES2 <- mutate(vaVPDES1,MunMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Municipal'),1,0)
                   ,MunMinor=ifelse(c(MAJOR__MIN=='Minor'& MUNICIPAL_=='Municipal'),1,0)
                   ,IndMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Industrial'),1,0)
                   ,IndMinor=ifelse(c(MAJOR__MIN=='Minor'&MUNICIPAL_=='Industrial'),1,0))
vaVPDES2[is.na(vaVPDES2)] <- 0 # make 0 to enable subsequent sum function
# Sum all permit types based on StationID
vaVPDES3 <- ddply(vaVPDES2,'StationID',summarise,MunMajor=sum(MunMajor)
                  ,MunMinor=sum(MunMinor),IndMajor=sum(IndMajor),IndMinor=sum(IndMinor))  
# Add to final results
Result <- merge(Result,vaVPDES3, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result2.csv')
rm(vaVPDES)#remove shapefile to increase memory availability


################################ Dam Calculations ########################################################
dams <- readOGR('C:/GIS/ProbMonGIS/GISdata','dam_albers')
# Dataframe to store dam data
damsummary <- data.frame(matrix(NA, ncol = 63, nrow = 0))
columnnames1 <- c('NID_ID','STATE','DAM_NAME','OTHER_NAME','HAZARD','EAP','STATE_NAME'
                  ,'CONG_DIST','COUNTY','NEAR_CITY','DIST_CITY','RIVER','PRM_PURPOS'
                  ,'NID_DAMTYP','YEAR_COMPL','NID_HEIGHT','NID_STOR','DAM_LENGTH','MAX_DISCH'
                  ,'OWNER','OWN_TYPE','STATE_AGCY','FED_AGCY','NONFED_DAM','SECT_TOWN','PURPOSE'
                  ,'DAM_TYPE','DAM_HEIGHT','HYDR_HGT','STRUCT_HGT','NORM_STOR','MAX_STOR'
                  ,'SURF_AREA','DRAIN_AREA','SPILL_TYPE','SPILL_WDTH','NUM_LOCKS','LOCK_LEN'
                  ,'LOCK_WIDTH','VOLUME','INSP_DATE','PHASEI_INS','FD_CONSTRC','FD_DESIGN'
                  ,'FD_FUNDING','FD_INSPECT','FD_OPERATE','FD_OTHER','FD_OWNER','FD_REGULAT'
                  ,'SUPP_FED','SUPP_DATE','SOURC_AGCY','SOURC_DATE','SOURCE_ID','LONGITUD_X'
                  ,'LATITUDE_Y','FIPS_STATE','FIPS_CNTY','BCU','coords.x1','coords.x2'
                  ,'StationID')
names(damsummary) <- columnnames1
# Loop over permit information within each watershed
for(i in 1:length(wshdPolys)){
  print(i)
  s <- dams[wshdPolys[i,],]
  if(!nrow(s@data)==0){
    damselect <- data.frame(s)
    damselect <- mutate(damselect, StationID=wshdList[i])
    print('number of s rows')
    print(nrow(s))}
  if(nrow(s@data)==0){
    damselect <- data.frame(matrix(0,ncol=63,nrow=1))
    names(damselect) <- columnnames1
    damselect$StationID <- wshdList[i]}
  damsummary <- rbind(damsummary,damselect)
}
damsummary[is.na(damsummary)] <- 0 # make 0 to enable subsequent sum function
# Mark permits according to classification
damsummary1 <- mutate(damsummary,Dam=ifelse(c(NID_HEIGHT>0),1,0))
damsummary2 <- ddply(damsummary1, 'StationID',summarise,damcount=sum(Dam))
# Add to final results
Result <- merge(Result,damsummary2, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result3.csv')
rm(dams)#remove shapefile to increase memory availability


############################## Stream Length & Density Calculations ####################################
streams <- data.frame(matrix(NA, ncol = 2, nrow = 0))
names(streams) <- c('StationID','STRMLEN')
s <- NULL

for(i in 1:length(wshdPolys)){ 
  print(i)
  # Clip nhd streams to each polygon in wshdPolys
  testnhd <- raster::intersect(nhd,wshdPolys[i,])  
  STRMLEN <- gLength(testnhd)
  s <- cbind(wshdList[i],STRMLEN)
  streams[i,] <- s
}
streams$STRMLEN <- as.numeric(streams$STRMLEN)
#streams1 <- aggregate(STRMLEN~StationID, data=streams, sum)
streams2 <- merge(streams,landusewide[,1:2], by='StationID')
streams3 <- mutate(streams2, streamlength_km=STRMLEN/1000
                   ,STRMDENS=streamlength_km/(totalArea_sqMile*2.58999))
# Add to final results
Result <- merge(Result,streams3[,c(1:2,5)], by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result4.csv')


############################### Impervious Surface Calculations ####################################

impervious <- raster("C:/GIS/ProbMonGIS/GISdata/nlcd2011imp.TIF")

# Set up dataframe to store impervious data
dfi <- data.frame(matrix(NA, ncol = 102))
names(dfi) <- c('StationID',paste("PCT",c(0:100),sep=""))
template <- dfi[,2:102]

for(i in 1:length(wshdList)){ 
  print(i)
  e = extract(impervious, wshdPolys[i,],small=T, na.rm=F)
  et = lapply(e,table)
  t <- melt(et)
  t.cast <- cast(t, L1 ~ Var.1, sum)
  names(t.cast)[1] <- "StationID"
  print(t.cast[1,1] <- wshdList[i])
  colnames(t.cast)[2:length(names(t.cast))] <- paste("PCT",colnames(t.cast)
                                                     [2:length(names(t.cast))], sep = "")
  if(length(et[[1]])==101){
    results <- t.cast
  }
  if(length(et[[1]])<101){
    zeros = template[is.na(match(names(template), names(t.cast)))]
    zeros[1,] = 0
    results <- cbind(t.cast, zeros)
    results <- results[,colnames(dfi)]
  }
  dfi[i,] <- results
}

dfi1 <- melt(dfi,'StationID')  
imperv <- mutate(dfi1, sqMile=900*value*0.0002471053814672*0.0015625
                 ,PCT=as.numeric(substr(variable, 4,length(variable))),sqMileImp=sqMile*(PCT/100))
imperv1 <- aggregate(sqMileImp ~ StationID,imperv,FUN='sum')
imperv2 <- merge(imperv1,landusewide[,1:2],by='StationID')
imperv3 <- mutate(imperv2,wshdImpPCT=(sqMileImp/totalArea_sqMile)*100)

# Add to final results
Result <- merge(Result,imperv3[,c(1:2,4)], by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result5.csv')
rm(impervious)#remove raster to increase memory availability


####################################### DEM Calculations ###################################################
DEM <- raster("C:/GIS/ProbMonGIS/GISdata/vaelevation.TIF")
# Set up dataframe to store elevation data
elev <- data.frame(matrix(NA, ncol = 5))
names(elev) <- c('StationID','ELEVMIN','ELEVMAX','ELEVMEAN','ELEVSD')

# Loop through polygons in shapefile to get elevation information
for(i in 1:length(wshdList)){ 
  print(i)
  e <-  extract(DEM, wshdPolys[i,],small=T, na.rm=F) 
  ELEVMIN <- sapply(e, FUN=min, na.rm=T)
  ELEVMAX <- sapply(e, FUN=max, na.rm=T)
  ELEVMEAN <- sapply(e, FUN=mean, na.rm=T)
  ELEVSD <- sapply(e, FUN=sd, na.rm=T)
  s <- cbind(wshdList[i],ELEVMIN,ELEVMAX,ELEVMEAN,ELEVSD)
  elev[i,] <- s
}

elev[,2:5] <- sapply(elev[,2:5],as.numeric)
elev1 <- aggregate(ELEVMIN~StationID, data=elev,min)
elev2 <- aggregate(ELEVMAX~StationID, data=elev,max)
elev3 <- aggregate(cbind(ELEVMEAN, ELEVSD)~StationID, data=elev, mean)
elev4 <- join_all(list(elev1,elev2,elev3),by='StationID')
elev5 <- mutate(elev4, ELEVRANGE=ELEVMAX-ELEVMIN)

# Add to final results
Result <- merge(Result,elev5, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result6.csv')
rm(DEM)#remove raster to increase memory availability


###################################### Slope Calculations ###########################################
# Too large to bring in whole layer, clipped to bounding box in GIS first, exported as .TIFF from GIS
slope <-  raster('C:/GIS/ProbMonGIS/GISdata/slope2010.TIF')

# Set up dataframe to store slope data
slp <- data.frame(matrix(NA, ncol = 5))
names(slp) <- c('StationID','SLPMIN','SLPMAX','SLPMEAN','SLPSD')

# Loop through polygons in shapefile to get slope information
for(i in 1:length(wshdList)){ 
  print(i)
  e <-  extract(slope, wshdPolys[i,],small=T, na.rm=F) 
  SLPMIN <- sapply(e, FUN=min, na.rm=T)
  SLPMAX <- sapply(e, FUN=max, na.rm=T)
  SLPMEAN <- sapply(e, FUN=mean, na.rm=T)
  SLPSD <- sapply(e, FUN=sd, na.rm=T)
  t <- cbind(wshdList[i],SLPMIN,SLPMAX,SLPMEAN,SLPSD)
  slp[i,] <- t
}

slp[,2:5] <- sapply(slp[,2:5],as.numeric)
slp1 <- aggregate(SLPMIN~StationID, data=slp,min)
slp2 <- aggregate(SLPMAX~StationID, data=slp,max)
slp3 <- aggregate(cbind(SLPMEAN, SLPSD)~StationID, data=slp, mean)
slp4 <- join_all(list(slp1,slp2,slp3),by='StationID')
slp5 <- mutate(slp4, SLPRANGE=SLPMAX-SLPMIN)

# Add to final results
Result <- merge(Result,slp5, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result7.csv')
rm(slope)#remove raster to increase memory availability


########################################## Rainfall Calculations ####################################################
# Bring in rainfall raster
rainfall <- raster('C:/GIS/ProbMonGIS/GISdata/rainfall21.TIF')
# Set up dataframe to store rainfall data
rainp <- data.frame(matrix(NA, ncol = 2))
rains <- data.frame(matrix(NA, ncol = 2))
# Loop through polygons in shapefile to get rainfall information
for(i in 1:length(wshdList)){ 
  print(i)
  e <- extract(rainfall, wshdPolys[i,],small=T, na.rm=F) 
  rain1 <- (sapply(e,sum)/100)
  rainp[i,] <- c(wshdList[i],rain1)
}
# Loop through sites in shapefile to get rainfall information
for(i in 1:length(wshdSites)){ 
  print(i)
  rain2 <- (extract(rainfall, wshdSites[i,])/100)
  rains[i,] <- c(siteList[i],rain2)
}
rainp[,2] <- sapply(rainp[,2], as.numeric);rains[,2] <- sapply(rains[,2],as.numeric)
rainp <- aggregate(X2~X1, data=rainp,sum)
rain <- merge(rainp,rains, by='X1')
names(rain) <- c('StationID', 'wshedRain_mmyr', 'siteRain_mmyr')
rain <- mutate(rain, wshedRain_inyr=wshedRain_mmyr*0.0393701
               ,siteRain_inyr=siteRain_mmyr*0.0393701)
# Add to final results
Result <- merge(Result,rain, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result8.csv')
rm(rainfall)#remove raster to increase memory availability


############################### Population Density Calculations ########################################
# Bring in clipped block census data
pop2000 <- readOGR(dsn='C:/GIS/ProbMonGIS/GISdata', layer='pop2000final')
pop2010 <- readOGR(dsn='C:/GIS/ProbMonGIS/GISdata', layer='pop2010final')

pop <- data.frame(StationID=NA,wshdPOP2000=NA,wshdPOP2010=NA)


for(i in 1:length(wshdPolys)){
  print(i)
  blockpop2000 <- suppressWarnings(raster::intersect(wshdPolys[i,],pop2000))
  if(nrow(blockpop2000)==0){ # special case if watershed polygon falls entirely within population block
    blockpop2000 <- pop2000[wshdPolys[i,],]
    blockpop2000$shapeAREA <- sapply(slot(wshdPolys[i,], "polygons"),slot, "area")
  }else{
    blockpop2000$shapeAREA <- sapply(slot(blockpop2000, "polygons"),slot, "area")}
  blockpop2000@data <- mutate(blockpop2000@data,blockPOP=(shapeAREA/AREA)*POP2000)
  wshdPOP2000 <- summarize(blockpop2000@data, wshdPOP2000=sum(blockPOP))
  # Repeat for 2010
  blockpop2010 <- raster::intersect(wshdPolys[i,],pop2010)
  if(nrow(blockpop2010)==0){ # special case if watershed polygon falls entirely within population block
    blockpop2010 <- pop2010[wshdPolys[i,],]
    blockpop2010$shapeAREA <- sapply(slot(wshdPolys[i,], "polygons"),slot, "area")
  }else{
    blockpop2010$shapeAREA <- sapply(slot(blockpop2010, "polygons"),slot, "area")}
  blockpop2010@data <- mutate(blockpop2010@data,blockPOP=(shapeAREA/AREA)*POP10)
  wshdPOP2010 <- summarize(blockpop2010@data, wshdPOP2010=sum(blockPOP))  
  #pop[i,1] <- wshdPolys1@data$StationID[i] cannot get StationID to print properly in df
  pop[i,2:3] <- cbind(wshdPOP2000,wshdPOP2010)
}
# add in StationID outside of first loop
for(i in 1:length(wshdPolys)){
  print(wshdList[i])
  pop[i,1] <- wshdList[i]
}

pop2 <- join_all(list(pop,landusewide[,1:2]),by='StationID')
pop3 <- mutate(pop2, POPDENS2000=wshdPOP2000/(totalArea_sqMile*2.58999)
               ,POPDENS2010=wshdPOP2010/(totalArea_sqMile*2.58999)
               ,POPCHG2000_2010=((POPDENS2010-POPDENS2000)/POPDENS2000)*100)
# Add to final results
Result <- merge(Result,pop3[,c(1:3,5:7)], by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Results/Result9.csv')
rm(pop2000)#remove shapefile to increase memory availability
rm(pop2010)#remove shapefile to increase memory availability


############################## Road Density Calculations ###################################################
# TigerRoad file too big, bring in pre-clipped road file, same projection as polygon
roads <- readOGR('C:/GIS/ProbMonGIS/Jack','JackFinal_roads')
roaddf <- data.frame(StationID=NA, RDLEN=NA,RDLEN120=NA, wshd_sqkm=NA, area120_sqkm=NA, STXRD_CNT=NA)
# Bring in NHD polyline file
nhd <- readOGR(wd,'nhd_83albers')

for(i in 1:length(wshdPolys)){ 
  print(i)
  # Subset nhd streams by each polygon in wshdPolys
  testroads <- suppressWarnings(raster::intersect(roads,wshdPolys[i,])) # cut roads to watershed of interest
  wshd_sqkm <- gArea(wshdPolys[i,])*1e-6 # save area of watershed
  if(length(testroads)>0){
    RDLEN <- gLength(testroads) # calculate road length throughout watershed (in meters)
    testnhd <- suppressWarnings(raster::intersect(nhd,wshdPolys[i,])) # cut NHD to watershed of interest
    bufferNHD <- gBuffer(testnhd,width=120) # buffer all streams in watershed to riparian buffer distance
    area120_sqkm <- gArea(bufferNHD)*1e-6 # save area of 120m buffer
    roadInRiparianBuffer <- suppressWarnings(raster::intersect(roads,bufferNHD)) # cut roads to riparian buffer
    if(length(roadInRiparianBuffer)==0){
      RDLEN120 <- 0 # calculate road length in riparian buffer (in meters)
    }else{
      RDLEN120 <- gLength(roadInRiparianBuffer) # calculate road length in riparian buffer (in meters)
    }
    streamXroad <- gIntersection(testroads,testnhd) # find all stream/road crossings
    if(length(streamXroad)>0){
      STXRD_CNT <- nrow(as.matrix(streamXroad@coords))  # calculate number of stream/road crossings
    }else{
      STXRD_CNT <- 0 # set stream/road crossings to 0 if needed
    }
  }else{
    RDLEN <- 0
    RDLEN120 <- 0
    area120_sqmi <- 0
    STXRD_CNT <- 0
  }
  
  r <- cbind(StationID=wshdList[i],RDLEN,RDLEN120, wshd_sqkm, area120_sqkm,STXRD_CNT)
  roaddf[i,] <- r
}

roaddf[,2:6] <- apply(roaddf[,2:6],2,function(x) as.numeric(x))
roaddf <- join_all(list(roaddf,streams[,c(1,4)]), by='StationID') %>%
  mutate(roadlength_km=RDLEN/1000,RDDENS=roadlength_km/(wshd_sqkm),
         roadlength120_km=RDLEN120/1000,RDDENS=roadlength120_km/(area120_sqkm),
         pctRoadLengthInRiparian=(roadlength120_km/roadlength_km)*100,
         streamlength_km=STRMLEN/1000, STXRD=STXRD_CNT/streamlength_km)%>%
  select(-c(wshd_sqkm,area120_sqkm,streamlength_km,roadlength_km,roadlength120_km,STRMLEN))

# Add to final results
Result <- merge(Result,roaddf, by='StationID')
write.csv(Result,'C:/GIS/ProbMonGIS/Jack/Result10.csv')
rm(roads)#remove shapefile to increase memory availability







