# this script identifies which watersheds need landcover information to update the 2018 
#  IR dataset for 2020IR release

# Libraries
library(tidyverse)
library(sf)

source('organizeShapefiles.R')


#### 2017 Organization

# Bring in 2017 sites sampled
x2017 <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2017/EmmaGIS2017.xlsx',
                            sheet = 'GIScrossWalk2017') %>%
  filter(!is.na(DEQSITEID) & !(DEQSITEID == 'N/A')) %>% # drop sites that weren't sampled for various reasons in `Sample Code` column
  filter(!str_detect(`Sample Code`,'PD|OT|NT')) %>% # drop more sites that weren't sampled permission denied, other, non target
  arrange(DEQSITEID)
  



# Here are watersheds I already have delineated:
shapes <- gsub('.prj','',list.files( 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017', pattern="*.prj", full.names=F))

# are all the sites I need in the directory?
all(shapes %in% x2017$DEQSITEID )
# so what isn't in there?
missingFrom2017Watersheds <- x2017$DEQSITEID[!(x2017$DEQSITEID %in% shapes)] 
# But some of these could be already delineated in previous dataset (e.g. probtrend)
allWatersheds <- st_read('GISdata/AllWatersheds_through2016.shp') %>%
  filter(StationID %in% missingFrom2017Watersheds)

# What is still missing and not in allWatersheds?
stillMissing <- missingFrom2017Watersheds[!(missingFrom2017Watersheds %in% allWatersheds$StationID)]

# So I need to go to streamstats and delineate these myself. I will add shapefiles to the 
# D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017 directory
stillMissing_sf <- filter(x2017, DEQSITEID %in% stillMissing)


# rerun above until there are no sites left in stillMissing_sf
rm(stillMissing_sf)


combineSpatialData(inDirectoryName = 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017', # Where are the files in question stored? (Relative or absolute path name)
                   outDirectoryName = 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses', # Where should the combined file be saved? (Relative or absolute path name)
                   outShapefileName = '2017_final_EVJ', # What do you want to call the shapefile?
                   shapefileType = 'watershed' # Are we dealing with watershed or site data? 
                                      )

# If any issues arise in the function above about mismatches in crs use code below to fix
#fix2 <- st_read('D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017/6ABEJ001.14.shp')
#fix <- st_read('D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017/2DECP002.24.shp') %>%
#  st_transform(st_crs(fix2))
#st_crs(fix) == st_crs(fix2)
#st_write(fix, 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017/2DECP002.242.shp')
