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
wshdPolys <- readOGR('GISdata/EmmaMessAround','TwinWatersheds')
wshdSites <- readOGR('GISdata/EmmaMessAround','TwinSites')
wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table
wshdList <- as.character(wshdPolys$StationID)
siteList <- as.character(wshdSites$StationID)



wshdPolys <- st_read('GISdata/EmmaMessAround/TwinWatersheds.shp')
wshdSites <- st_read('GISdata/EmmaMessAround/TwinSites.shp')
#wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
#wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table
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

wshdPolys <- left_join(wshdPolys, years, by = 'StationID')

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
  #l <- landuseCalc(i)
  l <- landcoverCounts(get(paste0('landcover',wshdPolys$NLCDyear[i])), wshdPolys[i,])
  df <- rbind(df,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  df <- df[complete.cases(df[,1]),]#remove any placeholder rows
}

landusewide <- landuseDataManagement(df)

write.csv(landusewide,paste(saveHere,'landusewide.csv',sep=''))
