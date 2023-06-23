## Note: For elevation and rainfall standard deviation (sd) calculations where more than one
## watershed polygon is used to describe one StationID the sd of the watershed is reported as
## the average of all individual watershed standard deviations. For the most accurate results
## you must dissolve all polygons by StationID's to calculate the true sd of the population.

library(tidyverse)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
#library(reshape)
library(reshape2)
library(sf)
library(readxl)


# Establish a GIS working directory
# This is where you will source all static GIS data for the project (not your input watersheds)
wd <- "E:/evjones/HardDriveBackup/GIS/ProbMonGIS/GISdata"

# Where do you want to save the outputs? 
saveHere <- 'Results/ProbWadeableNLCDFixesLandcoverRerun/'

#yearSampled <- 2019

# Bring in watersheds

# first bring in pop2000 bc relatively tiny and has appropriate crs to match further layers
pop2000 <- st_read(paste0(wd,'/pop2000final.shp'))


#Bring in 2001-2020 landcover data
original <- read.csv('C:/HardDriveBackup/R/GitHub/FreshwaterProbMonIntegratedReports/2024ProbChapter/originalData/Wadeable_ProbMon_2001-2020_fixedArea.csv')

# only want 2019-2020 sites
stations <- filter(original, Year %in% c(2019, 2020))



# polygons
wshdPolys <- st_read('C:/HardDriveBackup/GIS/ProbMonGIS/DelineatedWatersheds/Watersheds.gdb', 'AllWatersheds_through2022_burnEverythingElse') %>%
  filter(StationID %in% stations$StationID) %>% 
  mutate(StationID = sub("\r\n" ,"",StationID)) %>% # get rid of any stray spaces after StationID in attribute table
  dplyr::select(StationID) %>%
  st_transform(st_crs(pop2000))

missing <- filter(stations, !StationID %in% wshdPolys$StationID)

if(nrow(missing) > 0 ){
  # Bring in all watersheds to grab those for missing trend sites
  trendWatersheds <- st_read('E:/evjones/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/trend_StreamStats/trend_StreamStats_watersheds.shp') %>%
    dplyr::rename(StationID=UID) %>%
    filter(StationID %in% missing$StationID) %>%
    dplyr::select(StationID)%>%
    st_transform(st_crs(pop2000))
  
  
  # add these sites to wshdPolys for new spatial data
  wshdPolys <- rbind(wshdPolys, trendWatersheds)
  # anything missing???
  wshdPolys$StationID[!(wshdPolys$StationID %in% missing$StationID )]
  # cool 
  rm(trendWatersheds)
}


## Make sites object, best to do this from the actual data bc trend StationID's
wshdSites <- stations %>% 
  st_as_sf(coords = c("LongitudeDD", "LatitudeDD"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng,


#View(stations %>% group_by(StationID) %>% mutate(n = n()))
# this is okay bc the trend site got off cycle and was sampled twice in the two year window


# if(!('StationID_Trend' %in% names(x2018))){
#   wshdSites <- x2018 %>%
#     mutate(StationID = sub("\r\n" ,"",DEQSITEID), # get rid of any stray spaces after StationID in attribute table
#            StationID_Trend = case_when(str_detect(SITEID, '_') ~ SITEID,
#                                        TRUE ~ StationID)) %>% # keep trend names for repeated sites, but keep original name if not trend-ized
#     dplyr::rename(LatitudeDD = LAT_DD, LongitudeDD = LONG_DD) %>%
#     dplyr::select(StationID, StationID_Trend, LatitudeDD, LongitudeDD) 
# } else{
#   wshdSites <- dplyr::select(x2017, StationID, StationID_Trend, LatitudeDD, LongitudeDD) 
#   
# }

# # deal with trend sites later
# wshdSites <- filter(stations, StationID %in% wshdPolys$StationID) %>% 
#   dplyr::select(StationID = Station_Id, StationID_Trend = Station_Id, LatitudeDD = Latitude, LongitudeDD = Longitude)
# 
# if(nrow(wshdSites) != nrow(wshdPolys)){
#   # Get missing trend lat/longs
#   allSites <- st_read('GISdata/AllStations_through2016.shp') %>% 
#     filter(StationID %in% wshdSites$StationID) %>%
#     st_drop_geometry() %>%
#     dplyr::select(StationID, Latitude, Longitude)
#   
#   wshdSites <- left_join(wshdSites, allSites, by = 'StationID') %>%
#     mutate(LAT_DD = case_when(is.na(LAT_DD) ~ Latitude, TRUE ~ LAT_DD),
#            LONG_DD = case_when(is.na(LONG_DD) ~ Longitude, TRUE ~ LONG_DD)) %>%
#     dplyr::select(-c(Latitude, Longitude)) %>%
#     st_as_sf(coords = c("LONG_DD", "LAT_DD"),  # make spatial layer using these columns
#              remove = T, # don't remove these lat/lon cols from df
#              crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
#   rm(allSites)
# } else {
#   wshdSites <- wshdSites %>%
#     st_as_sf(coords = c("LongitudeDD", "LatitudeDD"),  # make spatial layer using these columns
#              remove = T, # don't remove these lat/lon cols from df
#              crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng,
# }



# Critical Link (file with StationID's linked to year sampled for correct NLCD)
# This file must have fields: StationID and Year_ where StationID=DEQStationID that
#   matches input watershed StationID's and Year_ is year sampled
#criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')%>%
#  dplyr::select(StationID,Year,StationID_Trend,EcoRegion,BioRegion,Order,StreamSizeCat)%>%
#  dplyr::rename(YearSampled=Year)

#years <- filter(criticalLink, StationID %in% wshdPolys$StationID) %>%
#  mutate(year = ifelse(!is.na(YearSampled),YearSampled,2016), # get rid of NA's, replace with 2016 for now bc most recent NLCD release
#         NLCD = case_when( between(year, 2000, 2003) ~ 2001, 
#                           between(year, 2004, 2008) ~ 2006,
#                           between(year, 2009, 2013) ~ 2011,
#                           between(year, 2014, 2020) ~ 2016)) # the upper bound of this will need to be changed when new NCLD released



wshdPolys <- left_join(wshdPolys, dplyr::select(stations, StationID, Year), by = 'StationID') %>%
  mutate(YearSampled = Year,
         NLCD = case_when( between(YearSampled, 2000, 2003) ~ 2001,
                           between(YearSampled, 2004, 2008) ~ 2006,
                           between(YearSampled, 2009, 2013) ~ 2011,
                           #between(YearSampled, 2014, 2020) ~ 2016)) %>% # This was how IR2022 was run, but we need to rerun 2019, 2020 now that we have new NCLD
                           between(YearSampled, 2014, 2018) ~ 2016,
                           between(YearSampled, 2019, 2022) ~ 2019)) %>% 
  
  # make sure there is a point for each watershed or rainfall will bomb out
  filter(StationID %in% unique(wshdSites$StationID)) %>%
  # make sure every watershed has a NLCD year
  filter(!is.na(NLCD))








# boatable removed already by only bringing in TS samples above
# ##### for a deadline, remove boatable sites from processing to speed data processing ######
# boatable <- filter(x2018, str_detect( `Sample Code`,'boatable')) %>%
#   pull(DEQSITEID)
# 
# wshdPolys <- filter(wshdPolys, !StationID %in% boatable)
# wshdSites <- filter(wshdSites, !StationID %in% boatable)



# use this variable for sifting through unique watersheds for things that don't vary by sample year, 
# e.g. permit counts, dam counts, stream length, elevation, slope, rainfall
uniqueWshdList <- unique(as.character(wshdPolys$StationID)) 

# use this variable for sifting through, unique NLCD years to efficiently calculate landcover, riparian, and imperviousness metrics
uniqueWshdListNLCD <- wshdPolys %>%
  st_drop_geometry() %>%
  group_by(StationID, NLCD) %>%
  distinct(StationID) %>%ungroup()
# use this variable when you need to calculate yearly data releases, e.g. roads
uniqueWshdListYear <- wshdPolys %>%
  st_drop_geometry() %>%
  group_by(StationID, YearSampled) %>%
  distinct(StationID) %>% ungroup()

# and this version has all the important data
uniqueWshdListNLCDYear <- wshdPolys %>%
  st_drop_geometry() %>%
  group_by(StationID, YearSampled, NLCD) %>%
  distinct(StationID) %>%ungroup()


# Bring in appropriate landcover layer 
landcover2001 <- raster(paste(wd,"/NLCD2001.TIF",sep=""))
landcover2006 <- raster(paste(wd,"/NLCD2006.TIF",sep=""))
landcover2011 <- raster(paste(wd,"/nlcd2011VA.TIF",sep=""))
landcover2016 <- raster(paste(wd,"/nlcd2016.TIF",sep="")) 
landcover2019 <- raster(paste(wd, '/nlcd2019.tif', sep=""))

# Bring in landcover functions for appropriate yearly analysis
source('landcoverFunctions_rebuild.R')




#### LANDUSE CALCULATIONS
# Set up dataframe to store landcover data
template <- tibble(StationID = 'template', VALUE_11=0,VALUE_21=0,VALUE_22=0, VALUE_23=0,VALUE_24=0
                   ,VALUE_31=0,VALUE_41=0,VALUE_42=0,VALUE_43=0,VALUE_52=0,VALUE_71=0
                   ,VALUE_81=0,VALUE_82=0,VALUE_90=0,VALUE_95=0)


# Run the functions
df <- mutate(template,StationID=NA,NLCD=NA, sqMi=NA)%>%
  dplyr::select(StationID,NLCD,everything(), sqMi)

for( i in 1:nrow(uniqueWshdListNLCD)){
  # Status update
  print(paste0('Processing site ',i, ' of ', nrow(uniqueWshdListNLCD)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdListNLCD$StationID[i] &
                              NLCD %in% uniqueWshdListNLCD$NLCD[i])
  
  l <- landcoverCounts(template, base::get(paste0('landcover',unique(wshdPolyOptions$NLCD))),
                       wshdPolyOptions[1,]) # only need to use one polygon to do analysis
  df <- rbind(df,l) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  df <- df[complete.cases(df[,1]),]#remove any placeholder rows
}


# now reorganize counts and join (potentially smaller dataframe) to full watershed list
landusewide <- landuseDataManagement(df, uniqueWshdListNLCDYear)

write.csv(landusewide,paste(saveHere,'landusewide.csv',sep=''), row.names= F)
rm(l); rm(df);rm(wshdPolyOptions)






#### RIPARIAN CALCULATIONS 
# Bring in NHD polyline file
nhd <- st_read(paste0(wd,'/nhd_83albers.shp')) %>% 
  st_transform(st_crs(wshdPolys))


# Run riparian calculations
finalRiparian <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA
                            ,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA
                            ,RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA
                            ,RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA
                            ,RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA
                            ,RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA) 



for(i in 1:nrow(uniqueWshdListNLCD)){
  # Status update
  print(paste0('Processing site ',i, ' of ', nrow(uniqueWshdListNLCD)))
  
  
  # get watershed polygon based on StationID and NLCD year combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdListNLCD$StationID[i] &
                              NLCD %in% uniqueWshdListNLCD$NLCD[i])
  
  # Subset nhd streams by each polygon
  testnhd <- suppressWarnings(st_intersection(nhd, st_buffer(wshdPolyOptions[1,], 0))) %>% # only need to use one polygon to do analysis
    mutate(StationID = unique(wshdPolyOptions$StationID), NLCDyear = unique(wshdPolyOptions$NLCD))
  # st_buffer(polygon, 0) fixes any unintended topology errors in the above step
  
  # Assign StationID to line segments pertaining to each polygon StationID
  if(nrow(testnhd)==0){
    # do all the joining in case watershed sampled multiple years
    blank <- finalRiparian %>%
      mutate(StationID = uniqueWshdListNLCDYear$StationID[i],
             NLCD = uniqueWshdListNLCD$NLCD[i]) %>%
      left_join(uniqueWshdListNLCDYear, by = c('StationID','NLCD')) %>%
      dplyr::select(-c(YearSampled.x)) %>%
      dplyr::select(StationID, YearSampled.y, NLCD, everything()) %>%
      dplyr::rename('YearSampled' = 'YearSampled.y')
    finalRiparian <- rbind(finalRiparian, blank)
  }else{
    finalRiparian <- rbind(finalRiparian, ripCalc(testnhd, base::get(paste0('landcover',unique(wshdPolyOptions$NLCD))),
                                                  uniqueWshdListNLCDYear))
  }
  finalRiparian <- finalRiparian[complete.cases(finalRiparian$StationID),]
}

Result <- left_join(landusewide,finalRiparian, by=c('StationID','YearSampled','NLCD'))

write.csv(finalRiparian,paste(saveHere,'finalRiparian.csv',sep=''), row.names= F)
write.csv(Result,paste(saveHere,'Result1.csv',sep=''), row.names= F)
rm(testnhd); rm(finalRiparian)
rm(landcover2001);rm(landcover2006);rm(landcover2011); rm(landcover2016)#remove raster to increase memory availability
rm(template);rm(nhd);rm(wshdPolyOptions); rm(landusewide)



############################### % Impervious Calculations ##############################################

imperv2001 <- raster(paste0(wd,"/NLCD2001imp.TIF"))
imperv2006 <- raster(paste0(wd,"/NLCD2006imp.TIF"))
imperv2011  <- raster(paste0(wd,"/NLCD2011imp.TIF"))
imperv2016  <- raster(paste0(wd,"/NLCD2016imp.TIF")) 
imperv2019  <- raster(paste0(wd,"/nlcd2019imp.TIF")) 


# Set up dataframe to store impervious data
dfi <- data.frame(matrix(NA, ncol = 104))
names(dfi) <- c('StationID','NLCD',paste("PCT",c(0:100),sep=""), 'sqMi')
templatei <- dfi[,3:104]

# Run the impervious
for(i in 1:nrow(uniqueWshdListNLCD)){
  # Status update
  print(paste0('Processing site ',i, ' of ', nrow(uniqueWshdListNLCD)))
  
  # get watershed polygon based on StationID and NLCD year combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdListNLCD$StationID[i] &
                              NLCD %in% uniqueWshdListNLCD$NLCD[i])
  
  impervious <- impervousCounts(templatei, base::get(paste0('imperv',unique(wshdPolyOptions$NLCD))), wshdPolyOptions[1,])
  
  dfi <- rbind(dfi,impervious) # must use rbind() instead of df[i,] <- l because l could be multiple rows
  dfi <- dfi[complete.cases(dfi$StationID),] #remove any placeholder rows
}

imperviousresults <- imperviousDataManagement(dfi, uniqueWshdListNLCDYear)  
Result <- left_join(Result,imperviousresults, by=c('StationID','YearSampled','NLCD'))
write.csv(imperviousresults,paste(saveHere,'impervious.csv',sep=''), row.names= F)  
write.csv(Result,paste(saveHere,'Result2.csv',sep=''), row.names= F)  
rm(imperv2001); rm(imperv2006); rm(imperv2011);rm(imperv2016); rm(dfi); rm(templatei); rm(impervious); rm(imperviousresults);rm(wshdPolyOptions)



# don't need to do any more bc already have those metrics calculated correctly
