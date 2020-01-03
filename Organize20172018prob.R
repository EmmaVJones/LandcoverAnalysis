# this script identifies which watersheds need landcover information to update the 2018 
#  IR dataset for 2020IR release

# Libraries
library(tidyverse)
library(sf)

source('organizeShapefiles.R')


# Bring in 2018 IR dataset
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')

# Bring in 2017 sites sampled
x2017 <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2017/Regional_Results_Spring2017_Final.xlsx',
                            sheet = 'GIScrosswalk (2)')

shapes <- gsub('.prj','',list.files( 'G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017', pattern="*.prj", full.names=F))

# are all the sites I need in the directory?
shapes %in% x2017$DEQSITEID 
# what isn't in there?
x2017$DEQSITEID[!(x2017$DEQSITEID %in% shapes)] # that's fine

combineSpatialData(inDirectoryName = 'G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2017', # Where are the files in question stored? (Relative or absolute path name)
                   outDirectoryName = 'G:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses', # Where should the combined file be saved? (Relative or absolute path name)
                   outShapefileName = '2017_final_EVJ', # What do you want to call the shapefile?
                   shapefileType = 'watershed' # Are we dealing with watershed or site data? 
                                      )



Simple feature collection with 1 feature and 10 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 1262408 ymin: 1548063 xmax: 1347866 ymax: 1636343
epsg (SRID):    NA
proj4string:    +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
Error in rbind.data.frame(...) : 
  numbers of columns of arguments do not match


#Bring in 2020 IR LRBS dataset
LRBS <- readxl::read_excel('C:/HardDriveBackup/PHAB_R/TMDLsummary_2019-12-17.xlsx')


# Identify differences, okay to just use StationID bc follow up final datasets will rerun
#  appropriate NLCD info for trend sites if the watershed exists
wshdDiff <- filter(LRBS, !(StationID %in% unique(criticalLink$StationID)))

# that's a lot of missing sites, but this dataset also includes targeted stress/ref/TMDL, etc.

