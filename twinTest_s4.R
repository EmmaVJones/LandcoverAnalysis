## Note: For elevation and rainfall standard deviation (sd) calculations where more than one
## watershed polygon is used to describe one StationID the sd of the watershed is reported as
## the average of all individual watershed standard deviations. For the most accurate results
## you must dissolve all polygons by StationID's to calculate the true sd of the population.

library(tidyverse)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(reshape)
library(reshape2)
library(sf)
#library(plyr)
#library(dplyr)


# Establish a GIS working directory
# This is where you will source all static GIS data for the project (not your input watersheds)
wd <- "G:/evjones/GIS/ProbMonGIS/GISdata"

# Where do you want to save the outputs? 
saveHere <- 'Results'


# Bring in watersheds
#wshdPolys <- readOGR('GISdata/EmmaMessAround','TwinWatersheds')
#wshdSites <- readOGR('GISdata/EmmaMessAround','TwinSites')
#wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
#wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table
#wshdList <- as.character(wshdPolys$StationID)
#siteList <- as.character(wshdSites$StationID)

wshdPolys <- st_read('GISdata/AllWatersheds_through2016.shp') %>%
  filter(StationID %in% c("4AXOD000.38", "4AXOE001.26", "4AXOK000.29", "4AXOL000.94")) %>%
  dplyr::select(StationID) 
wshdSites <- st_read('GISdata/AllStations_through2016.shp') %>%
  filter(StationID %in% c("4AXOD000.38", "4AXOE001.26", "4AXOK000.29", "4AXOL000.94")) %>%
  dplyr::select(StationID) %>%
  as_Spatial()
wshdList <- as.character(wshdPolys$StationID)
siteList <- as.character(wshdSites$StationID)

# Critical Link (file with StationID's linked to year sampled for correct NLCD)
# This file must have fields: StationID and Year_ where StationID=DEQStationID that
#   matches input watershed StationID's and Year_ is year sampled
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')%>%
  dplyr::select(StationID,Year,StationID_Trend,EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(Year_=Year)

years <- filter(criticalLink, StationID %in% wshdPolys$StationID) %>%
  mutate(year = ifelse(!is.na(Year_),Year_,2016), # get rid of NA's, replace with 2016 for now bc most recent NLCD release
         NLCDyear = case_when( between(year, 2000, 2003) ~ 2001, 
                               between(year, 2004, 2008) ~ 2006,
                               between(year, 2009, 2013) ~ 2011,
                               between(year, 2014, 2020) ~ 2016)) # the upper bound of this will need to be changed when new NCLD released


wshdPolys <- left_join(wshdPolys, years, by = 'StationID') %>%
  as_Spatial()


# Bring in appropriate landcover layer 
landcover2001 <- raster(paste(wd,"/NLCD2001.TIF",sep=""))
landcover2006 <- raster(paste(wd,"/NLCD2006.TIF",sep=""))
landcover2011 <- raster(paste(wd,"/nlcd2011VA.TIF",sep=""))
landcover2016 <- raster(paste(wd,"/nlcd2016.TIF",sep="")) 


# Bring in landcover functions for appropriate yearly analysis
source('landcoverFunctions.R')




#### LANDUSE CALCULATIONS
# Set up dataframe to store landcover data
template <- tibble(StationID = 'template', VALUE_11=0,VALUE_21=0,VALUE_22=0, VALUE_23=0,VALUE_24=0
                       ,VALUE_31=0,VALUE_41=0,VALUE_42=0,VALUE_43=0,VALUE_52=0,VALUE_71=0
                       ,VALUE_81=0,VALUE_82=0,VALUE_90=0,VALUE_95=0)

# Run the functions
df <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%
  dplyr::select(StationID,YearSampled,NLCD,everything())

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
#nhd <- st_read(paste0(wd,'/nhd_83albers.shp'))

# Set up dataframes to store riparian landcover data
df1 <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA) %>% 
  dplyr::select(StationID,YearSampled,NLCD,everything())
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
  #testnhd <- nhd[wshdPolys[i,],]
  testnhd <- nhd[wshdPolys[i,],]
  
  # Assign StationID to line segments pertaining to each polygon StationID
  if(length(testnhd)==0){
    blank <- data.frame(StationID=wshdList[i],YearSampled=NA,NLCD=NA,RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA
                        ,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA
                        ,RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA
                        ,RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA
                        ,RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA
                        ,RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA) 
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

