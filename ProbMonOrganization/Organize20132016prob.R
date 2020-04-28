# this script uses the automated delineation tools from StreamStats to delineate probMon sites from 
# 2013-2016 to complete 2020 IR data compilation

# Libraries
library(tidyverse)
library(sf)

source('StreamStatsAutoDelineation.R')
source('organizeShapefiles.R')

# Read in previously published data we want to fix
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')%>%
  dplyr::select(StationID,Year,StationID_Trend,LongitudeDD, LatitudeDD, EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(YearSampled=Year) %>%
  filter(YearSampled >= 2013)


# Where do you want to save the outputs? 
saveHere <- 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/'


# go get watersheds and organize by year

# wanted to do it automatically, but too much lost with server contact so ended up doing manually
year <- 2018 #2017 

# just for 2017 and 2018
eachYear <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2017/EmmaGIS2017.xlsx',
                               sheet = 'GIScrossWalk2017') %>%
  filter(!is.na(DEQSITEID) & !(DEQSITEID == 'N/A')) %>% # drop sites that weren't sampled for various reasons in `Sample Code` column
  filter(!str_detect(`Sample Code`,'PD|OT|NT|boatable')) %>% # drop more sites that weren't sampled permission denied, other, non target
  filter(!str_detect(SITEID, '_'))%>%
  mutate(StationID = DEQSITEID, Order = MDCATY) %>%
  arrange(DEQSITEID)

eachYear <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2018/EmmaGIS20172018.xlsx',
                               sheet = 'GIScrossWalk2018') %>%
  filter(!is.na(DEQSITEID) & !(DEQSITEID == 'N/A')) %>% # drop sites that weren't sampled for various reasons in `Sample Code` column
  filter(!str_detect(`Sample Code`,'PD|OT|NT|boatable')) %>% # drop more sites that weren't sampled permission denied, other, non target
  filter(!str_detect(SITEID, '_'))%>%
  mutate(StationID = DEQSITEID, Order = MDCATY) %>%
  arrange(DEQSITEID)

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
    st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  
  # watersheds <- st_read(paste0(saveHere, year, '_StreamStats/', year,'_StreamStats_watersheds.shp'))
  
  points <- dat$point %>%
    reduce(rbind) %>%
    st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  
  # points <- st_read(paste0(saveHere, year, '_StreamStats/', year,'_StreamStats_points.shp'))
  
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
      st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    
    
    #points_missing <- dat$point %>%
    #  reduce(rbind) %>%
    #  st_transform(crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
   
    
    watersheds <- rbind(watersheds, watersheds_missing) %>%
      arrange(UID)
  }
  
  
  ## QA- make sure auto delineated watershed (based on 1:24k NHD) grabs appropriate 1:100k stream
  vafrm <- st_read('D:/evjones/GIS/ProbMonGIS/vafrm_99_05Albers.shp') %>%
    st_transform(st_crs(watersheds))
  watershedsQA <- left_join(watersheds %>% 
                              mutate(StationID = UID), 
                            eachYear, by = 'StationID') %>%
    group_by(StationID) # critical to keep segments associated with each polygon
  vafrm_intersection <- st_intersection(vafrm, st_buffer(watershedsQA,0))
  rm(vafrm) # clear up some memory
  
  # what orders are in the NHD clipped to watershed?
  vafrm_intersectionQA <- st_drop_geometry(vafrm_intersection) %>%
    
    #pre 2017 dplyr::select(STRAHLER, UID:StreamSizeCat) %>%
    group_by(StationID) %>%
    summarise(OrdersIncluded = paste(sort(unique(STRAHLER)), collapse = '|'))
  
  # join and double check all is good
  watershedsQA <- st_drop_geometry(watershedsQA) %>%
    group_by(StationID) %>%
    mutate(OrdersExpected = paste(seq(1:Order), collapse = '|')) %>%
    left_join(vafrm_intersectionQA, by = 'StationID') %>%
    mutate(NeedManual = case_when(OrdersExpected != OrdersIncluded ~ 'QA',
                                  is.na(OrdersIncluded) ~ 'QA',
                                  TRUE ~ 'Fine')) %>%
    filter(NeedManual == 'QA')

  
  # These stations need manual review
  View(watershedsQA)
  #after manual review, not all sites need help. 
  watershedsQA <- filter(watershedsQA, StationID %in% c('1BBCK008.39','5AWAQ012.15')) # these are the only ones with actual issues
  
  # If you decide to override, here is how you replace with manual sites
  if(length(watershedsQA$StationID) >0){
    # get rid of wonky sites
    watersheds <- filter(watersheds, !(UID %in% watershedsQA$StationID))
    
    # read in new watersheds after manual QA
    shapes <- gsub('.prj','',list.files( paste0(saveHere, year, '_StreamStats/'), pattern="*.prj", full.names=F))
    #remove existing file 	
    shapes <- shapes[!( shapes %in% paste0(year, '_StreamStats_',c('watersheds','points')))]
    filenames <- paste0(saveHere, year, '_StreamStats/',shapes, '.shp')
    
    # read in shapefiles and organize
    newSheds <- filenames %>%
      map(st_read) %>%
      map2(shapes,~mutate(.x,UID=.y)) %>% # make a StationID column
      map(~dplyr::select(.,UID)) %>%
      reduce(rbind)  %>%
      st_transform(crs= "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    
    watersheds <- rbind(watersheds, newSheds) %>%
      arrange(UID)
    
    rm(watershedsQA); rm(newSheds);rm(vafrm_intersectionQA);rm(vafrm_intersection)
    
  }
  
  # save         
  st_write(watersheds, paste0(saveHere, year, '_StreamStats/', year,'_StreamStats_watersheds.shp'))
  st_write(points, paste0(saveHere,  year, '_StreamStats/', year,'_StreamStats_points.shp'))
  
#}

  
  
  
mapview(watersheds)+ mapview(points)











# now for trends, one time only
trends <- filter(criticalLink, str_detect(StationID_Trend, '_')) %>%
  distinct(StationID, .keep_all = TRUE)

dat <- streamStats_Delineation(state= 'VA', 
                               longitude = trends$LongitudeDD, 
                               latitude = trends$LatitudeDD, 
                               UID = trends$StationID)

# organize into appropriate files
watersheds <- dat$polygon %>%
  reduce(rbind)

points <- dat$point %>%
  reduce(rbind)

## QA- make sure auto delineated watershed (based on 1:24k NHD) grabs appropriate 1:100k stream
vafrm <- st_read('D:/evjones/GIS/ProbMonGIS/vafrm_99_05Albers.shp') %>%
  st_transform(st_crs(watersheds))
watershedsQA <- left_join(watersheds %>% 
                            mutate(StationID = UID), 
                          trends, by = 'StationID') %>%
  group_by(StationID) # critical to keep segments associated with each polygon
vafrm_intersection <- st_intersection(vafrm, st_buffer(watershedsQA,0))
rm(vafrm) # clear up some memory

# what orders are in the NHD clipped to watershed?
vafrm_intersectionQA <- st_drop_geometry(vafrm_intersection) %>%
  dplyr::select(STRAHLER, UID:StreamSizeCat) %>%
  group_by(StationID) %>%
  summarise(OrdersIncluded = paste(sort(unique(STRAHLER)), collapse = '|'))

# join and double check all is good
watershedsQA <- st_drop_geometry(watershedsQA) %>%
  group_by(StationID) %>%
  mutate(OrdersExpected = paste(seq(1:Order), collapse = '|')) %>%
  left_join(vafrm_intersectionQA, by = 'StationID') %>%
  mutate(NeedManual = case_when(OrdersExpected != OrdersIncluded ~ 'QA',
                                is.na(OrdersIncluded) ~ 'QA',
                                TRUE ~ 'Fine')) %>%
  filter(NeedManual == 'QA')


# These stations need manual review
View(watershedsQA)
#after manual review, not all sites need help. 
watershedsQA <- filter(watershedsQA, StationID %in% c('2-DCK003.94','4ABAU011.17')) # these are the only ones with actual issues

# If you decide to override, here is how you replace with manual sites
if(length(watershedsQA$StationID) >0){
  # get rid of wonky sites
  watersheds <- filter(watersheds, !(UID %in% watershedsQA$StationID))
  
  # read in new watersheds after manual QA
  shapes <- gsub('.prj','',list.files( paste0(saveHere, 'trend_StreamStats/'), pattern="*.prj", full.names=F))
  #remove existing file
  shapes <- shapes[!( shapes %in% paste0('trend_StreamStats_',c('watersheds','points')))]
  filenames <- paste0(saveHere, 'trend_StreamStats/',shapes, '.shp')
  
  # read in shapefiles and organize
  newSheds <- filenames %>%
    map(st_read) %>%
    map2(shapes,~mutate(.x,UID=.y)) %>% # make a StationID column
    map(~dplyr::select(.,UID)) %>%
    reduce(rbind)  %>%
    st_transform(crs= "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
  
  watersheds <- rbind(watersheds, newSheds) %>%
    arrange(UID)
  
  rm(watershedsQA); rm(newSheds);rm(vafrm_intersectionQA);rm(vafrm_intersection)
  
}


st_write(watersheds, paste0(saveHere, 'trend_StreamStats/trend_StreamStats_watersheds.shp'))
st_write(points, paste0(saveHere, 'trend_StreamStats/trend_StreamStats_points.shp'))
