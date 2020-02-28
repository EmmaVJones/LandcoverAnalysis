# inspect results


Result <- read_csv(paste(saveHere,'Result10.csv'))


# What watersheds are missing?
all(unique(Result$StationID) %in% unique(wshdPolys$StationID))

unique(wshdPolys$StationID)[!(unique(wshdPolys$StationID) %in% unique(Result$StationID) )]
# so no missing Result$StationID but still 

wshdPolys$StationID[duplicated(wshdPolys$StationID)]



# identify missing data by  joining back to original dataset
test <- left_join(criticalLink, Result, by = c('StationID','YearSampled'))

nrow(filter(test, is.na(NLCD)))

# Find stations that are missing watersheds to rerun
delineateMe <- filter(test, is.na(NLCD))

#delineateMeSites <- filter(wshdSites, StationID %in% delineateMe$StationID) 
# only got 2 that way
criticalLink1 <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')

delineateMeSites <- filter(dplyr::select(criticalLink1, StationID, LatitudeDD, LongitudeDD),
                           StationID %in% delineateMe$StationID)%>%
  st_as_sf(coords = c("LongitudeDD", "LatitudeDD"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon columns from the dataset (may want to use them later)
           crs = 4326)
st_write(delineateMeSites, 'GISdata/EmmaMessAround/missing2001-2016sites.shp')


# So once those are delineated by hand (in ArcGIS) time to reorganize
source('organizeShapefiles.R')


combineSpatialData(inDirectoryName = 'F:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2001_2016missing', # Where are the files in question stored? (Relative or absolute path name)
                   outDirectoryName = 'GISdata/EmmaMessAround', # Where should the combined file be saved? (Relative or absolute path name)
                   outShapefileName = 'missing2001-2016watersheds', # What do you want to call the shapefile?
                   shapefileType = 'watershed' # Are we dealing with watershed or site data? 
                   )
