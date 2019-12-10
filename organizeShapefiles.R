
# this script makes a new method for combining sites and watersheds from different shape files into a single shape file.
# the script will replace the arduous task of adding a field to each object containing the site name the file references
# for each site and polygon file. 



library(tidyverse)
library(sf)



# Function to read in files, combine to single file, appropriately organize StationID name,
# and save it out as a shapefile

combineSpatialData <- function(inDirectoryName, outDirectoryName, outShapefileName, shapefileType){
  
  # Find StationID names from file names and make filename object for reading in shapefiles
  if(shapefileType == 'watershed'){
    stationIDs <- gsub('.prj', '' , list.files( inDirectoryName, pattern="*.prj", full.names=F))
    filenames <- paste0(inDirectoryName,'/',stationIDs,'.shp') # add .shp for reading in with sf
    }
  if(shapefileType == 'site'){
    stationIDs <- gsub('_pnt', '', gsub('.prj', '' , list.files( inDirectoryName, pattern="*.prj", full.names=F))) 
    filenames <- paste0(inDirectoryName,'/',stationIDs,'_pnt.shp') # add .shp for reading in with sf
  }
  
  # Read in shapefiles and add field that appropriately links StationID name
  shapefiles <- filenames %>%
    map(st_read) %>%
    reduce(rbind) %>%
    transmute(StationID = stationIDs) %>%
    st_transform(crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
  
  #Save out shapefile
  st_write(shapefiles, paste0(outDirectoryName, '/', outShapefileName, '.shp'))
  
}


# Run function
combineSpatialData(inDirectoryName = 'GISdata/EmmaMessAround/Watersheds', # Where are the files in question stored? (Relative or absolute path name)
                   outDirectoryName = 'GISdata/EmmaMessAround', # Where should the combined file be saved? (Relative or absolute path name)
                   outShapefileName = 'TwinWatersheds', # What do you want to call the shapefile?
                   shapefileType = 'watershed' # Are we dealing with watershed or site data? 
                   )


combineSpatialData(inDirectoryName = 'GISdata/EmmaMessAround/Sites', # Where are the files in question stored? (Relative or absolute path name)
                   outDirectoryName = 'GISdata/EmmaMessAround', # Where should the combined file be saved? (Relative or absolute path name)
                   outShapefileName = 'TwinSites', # What do you want to call the shapefile?
                   shapefileType = 'site' # Are we dealing with watershed or site data? 
)
