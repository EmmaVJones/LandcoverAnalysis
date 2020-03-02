# this script uses the automated delineation tools from StreamStats to delineate probMon sites from 
# 2013-2016 to complete 2020 IR data compilation

# Libraries
library(tidyverse)
library(sf)

source('StreamStatsAutoDelineation.R')

# Read in previously published data we want to fix
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')%>%
  dplyr::select(StationID,Year,StationID_Trend,LongitudeDD, LatitudeDD, EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(YearSampled=Year) %>%
  filter(YearSampled >= 2013)


# Where do you want to save the outputs? 
saveHere <- 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/'


# go get watersheds and organize by year

# wanted to do it automatically, but too much lost with server contact so ended up doing manually

year <- 2014 # 2015 #2016 # 2017

#for( year in unique(criticalLink$YearSampled)){
  
  startTime <- Sys.time()
  eachYear <- filter(criticalLink, YearSampled == year) %>%
    filter(!str_detect(StationID_Trend, '_')) # dont keep redoing trend sites
  
  # delineate with StreamStats
  dat <- streamStats_Delineation(state= 'VA', 
                                 longitude = eachYear$LongitudeDD, 
                                 latitude = eachYear$LatitudeDD, 
                                 UID = eachYear$StationID)
  print(Sys.time()-startTime)
  
  # organize into appropriate files, lossy move here- automatically removes sites with no data
  watersheds <- dat$polygon %>%
    reduce(rbind) %>%
    st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  
  points <- dat$point %>%
    reduce(rbind) %>%
    st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # fix anything that is missing
  if(nrow(points) != nrow(watersheds)){
    missing <- points$UID[!(points$UID %in% watersheds$UID)]
    missingDat <- filter(eachYear, StationID %in% missing)
    
    dat <- streamStats_Delineation(state= 'VA', 
                                   longitude = missingDat$LongitudeDD, 
                                   latitude = missingDat$LatitudeDD, 
                                   UID = missingDat$StationID)
    
    watersheds_missing <- dat$polygon %>%
      reduce(rbind) %>%
      st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    
    
    #points_missing <- dat$point %>%
    #  reduce(rbind) %>%
    #  st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
   
    
    watersheds <- rbind(watersheds, watersheds_missing) %>%
      arrange(UID)
  }
  
  
  # save         
  st_write(watersheds, paste0(saveHere, year, '_StreamStats/', year,'_StreamStats_watersheds.shp'))
  st_write(points, paste0(saveHere,  year, '_StreamStats/', year,'_StreamStats_points.shp'))
  
}



mapview(watersheds)+ mapview(points)


# now for trends, one time only
trends <- filter(criticalLink, str_detect(StationID_Trend, '_')) %>%
  distinct(StationID, .keep_all = TRUE)

dat <- streamStats_Delineation(state= 'VA', 
                               longitude = trends$LongitudeDD[11:21], 
                               latitude = trends$LatitudeDD[11:21], 
                               UID = trends$StationID[11:21])

# organize into appropriate files
watersheds2 <- dat$polygon %>%
  reduce(rbind)

points2 <- dat$point %>%
  reduce(rbind)


watersheds <- rbind(watersheds1, watersheds2)
points <- rbind(points1, points2)

st_write(watersheds, paste0(saveHere, 'trend_StreamStats/trend_StreamStats_watersheds.shp'))
st_write(points, paste0(saveHere, 'trend_StreamStats/trend_StreamStats_points.shp'))
