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




# clear environment








#### 2018 Organization

# Bring in 2018 sites sampled
x2018 <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2018/EmmaGIS20172018.xlsx',
                            sheet = 'GIScrossWalk2018') %>%
  filter(!is.na(DEQSITEID) & !(DEQSITEID == 'N/A')) %>% # drop sites that weren't sampled for various reasons in `Sample Code` column
  filter(!str_detect(`Sample Code`,'PD|OT|NT')) %>% # drop more sites that weren't sampled permission denied, other, non target
  arrange(DEQSITEID)




# Here are watersheds I already have delineated:
#shapes <- gsub('.prj','',list.files( 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2018', pattern="*.prj", full.names=F))
x_2018_sf <- st_read('D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2018fix/2018sites_less1.shp')

# so there is no DEQ or NARS SiteID in the dataset. Excellent.


# new task: build autodelineation tool to access streamstats API from R to delineate these sites

# from https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
library(jsonlite)
library(sf)
library(sp)
library(geojsonio)


#Define function - watershed
watershed = function(state, lon, lat){
  p1 = 'https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode='
  p2 = '&xlocation='
  p3 = '&ylocation='
  p4 = '&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true'
  query <-  paste0(p1, state, p2, toString(lon), p3, toString(lat), p4)
  mydata <- fromJSON(query, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  poly_geojsonsting <- toJSON(mydata$featurecollection[[2]]$feature, auto_unbox = TRUE)
  poly <- geojson_sp(poly_geojsonsting)
  poly
}


poly <- geojson_sf(poly_geojsonsting)

mapview(poly) +
  mapview(ws)

# Delineate the watershed
ws <- watershed(state = 'VA', lon = x2018$LONG_DD[1], lat = x2018$LAT_DD[1])


# Emma Custom Function

streamStats_Delineation_single <- function(state, # StreamStats state info e.g. 'VA'
                                   longitude, # longitude value, numeric
                                   latitude, # latitude value, numeric
                                   UID # Unique station identifier to append to dataset
                                   ){ # function based off code by Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
  outData <- list(poly = list(), point = list())

  query <-  paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=',
                   state, '&xlocation=', toString(longitude), 
                   '&ylocation=', toString(latitude), 
                   '&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true') 
  # data pulled
  mydata <- fromJSON(query, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  
  # organize watershed
  poly_geojsonsting <- toJSON(mydata$featurecollection[[2]]$feature, auto_unbox = TRUE)
  outData$poly <- geojson_sf(poly_geojsonsting) %>%
    mutate(UID = UID) %>%
    dplyr::select(UID)
  
  # organize point
  point_geojsonsting <- toJSON(mydata$featurecollection[[1]]$feature, auto_unbox = TRUE)
  outData$point <- geojson_sf(point_geojsonsting) %>%
    mutate(UID = UID)%>%
    dplyr::select(UID)
 return(outData)
}


#dat <- streamStats_Delineation_single(state= 'VA', 
#                               longitude = x2018$LONG_DD[1], 
#                               latitude = x2018$LAT_DD[1], 
#                               UID = x2018$SITEID[1])
                


streamStats_Delineation <- function(# accepts multiple
  state, # StreamStats state info e.g. 'VA'
  longitude, # longitude value, numeric
  latitude, # latitude value, numeric
  UID # Unique station identifier to append to dataset
){ # function based off code by Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
                                    
  outDataAll <- list(polygon = list(), point = list())
  
  for(i in 1:length(longitude)){
    print(paste('Delineating Site:',i, 'of', length(longitude)))
    
    dat <- streamStats_Delineation_single(state= 'VA', 
                                          longitude = longitude[i], 
                                          latitude = latitude[i], 
                                          UID = UID[i])
    #catch in case streamstats bombs out
    if(is.null(dat)){dat <- list(polygon = data.frame(UID = NA), point = data.frame(UID = NA))}
    outDataAll$polygon[[i]] <- dat[['poly']] 
    outDataAll$point[[i]] <- dat[['point']] 
  }
  return(outDataAll)
}


x2018_sf <- streamStats_Delineation(state= 'VA', 
                               longitude = x2018$LONG_DD, 
                               latitude = x2018$LAT_DD, 
                               UID = x2018$DEQSITEID)




watersheds <- x2018_sf$polygon %>%
  reduce(rbind)

points <- x2018_sf$point %>%
  reduce(rbind)

# make sure everything got delineated
identical(nrow(points), nrow(watersheds), nrow(x2018))

# find missing
missingUID <- points$UID[!(points$UID %in% watersheds$UID)]

#redo what is missing
extra <- filter(x2018, DEQSITEID %in% missingUID)
x2018_sf_extra <- streamStats_Delineation(state= 'VA', 
                                    longitude = extra$LONG_DD, 
                                    latitude = extra$LAT_DD, 
                                    UID = extra$DEQSITEID)
watersheds_extra <- x2018_sf_extra$polygon %>%
  reduce(rbind)

# dont need this bc points already exist
#points_extra <- x2018_sf$point %>%
#  reduce(rbind)

# add back to orginal
watersheds <- rbind(watersheds, watersheds_extra) %>%
  arrange(UID)

rm(watersheds_extra); rm(x2018_sf_extra)

library(mapview)
mapview(watersheds) +
  mapview(points)


# get in correct projection for analyses
allWatersheds <- st_read('GISdata/AllWatersheds_through2016.shp')
watersheds <- st_transform(watersheds, st_crs(allWatersheds))

# save results
st_write(watersheds, 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2018_StreamStats/2018_StreamStats_watersheds.shp')
st_write(points, 'D:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2018_StreamStats/2018_StreamStats_Points.shp')
