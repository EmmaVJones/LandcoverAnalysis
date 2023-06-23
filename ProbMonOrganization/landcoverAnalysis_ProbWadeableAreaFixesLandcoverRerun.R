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
saveHere <- 'Results/ProbWadeableAreaFixesLandcoverRerun/'

#yearSampled <- 2019

# Bring in watersheds

# first bring in pop2000 bc relatively tiny and has appropriate crs to match further layers
pop2000 <- st_read(paste0(wd,'/pop2000final.shp'))


#Bring in 2001-2020 landcover data
original <- read.csv('C:/HardDriveBackup/R/GitHub/FreshwaterProbMonIntegratedReports/2024ProbChapter/originalData/Wadeable_ProbMon_2001-2020_EVJ.csv')
#Check to see if any watersheds that were updated are in this dataset
# Where did you save files relative to the working directory?
savedHere <- 'E:/evjones/HardDriveBackup/GIS/ProbMonGIS/DelineatedWatersheds/YearlyAnalyses/2023_QAwork/incorrectArea'

# read in new watersheds after manual QA
shapes <- gsub('.prj','',list.files( savedHere, pattern="*.prj", full.names=F))

fixThese <- filter(original, StationID %in% shapes) %>% 
  dplyr::select(DataSource:NLCD)

stations <- fixThese



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




###### VA VPDES Calculations
# Need to rerun Jason's Discoverer query to update layer, Kristy's permit layer is questionable
vaVPDES <- st_read(paste0(wd, '/vpdesalbers.shp')) %>% 
  st_transform(st_crs(wshdPolys))

# Dataframe to store permit count data
vaVPDES1 <- vaVPDES[0,] %>%
  st_drop_geometry() %>%
  add_row() %>%
  mutate(StationID=NA)


for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  permitCount <- pointCount(vaVPDES, wshdPolyOptions[1,])
  vaVPDES1 <- rbind(vaVPDES1, permitCount) 
  vaVPDES1 <- vaVPDES1[complete.cases(vaVPDES1$StationID),] #remove any placeholder rows
}

permitResult <- VPDESdataManagement(vaVPDES1, uniqueWshdListNLCDYear)


# Add to final results
Result <- left_join(Result,permitResult, by=c('StationID','YearSampled','NLCD'))
write.csv(Result,paste(saveHere,'/Result3.csv', sep=''), row.names= F)
write.csv(vaVPDES1,paste(saveHere,'/vaVPDES1.csv', sep=''), row.names= F)
rm(vaVPDES); rm(permitResult); rm(permitCount); rm(vaVPDES1);rm(wshdPolyOptions)#remove shapefile to increase memory availability


#### Dam Calculations 
dams <- st_read(paste0(wd,'/dam_albers.shp'))  %>% 
  st_transform(st_crs(wshdPolys)) %>%
  mutate_at(vars(INSP_DATE, SOURC_DATE, SUPP_DATE), as.character) # change date to character format to work with functions


# Dataframe to store dam data
damResult <- dams[0,] %>%
  st_drop_geometry() %>%
  add_row() %>%
  mutate(StationID=NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  damCountresults <- pointCount(dams, wshdPolyOptions[1,])
  damResult <- rbind(damResult,damCountresults)
  damResult <- damResult[complete.cases(damResult$StationID),]
}
damsummary <- damDataManagement(damResult, uniqueWshdListNLCDYear)

# Add to final results
Result <- merge(Result,damsummary, by=c('StationID','YearSampled','NLCD'))
write.csv(Result,paste(saveHere,'/Result4.csv', sep=''), row.names= F)
write.csv(damsummary,paste(saveHere,'/damsummary.csv', sep=''), row.names= F)
rm(dams);rm(damsummary); rm(damCountresults); rm(damResult);rm(wshdPolyOptions) #remove shapefile to increase memory availability




######## Stream Length & Density Calculations 
# Bring in NHD polyline file
nhd <- st_read(paste0(wd,'/nhd_83albers.shp')) %>% 
  st_transform(st_crs(wshdPolys))

streams <- data.frame(StationID=NA,STRMLEN=NA,STRMDENS=NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  # Subset nhd streams by each polygon
  testnhd <- suppressWarnings(st_intersection(nhd, st_buffer(wshdPolyOptions[1,], 0))) %>% # only need to use one polygon to do analysis
    mutate(StationID = unique(wshdPolyOptions$StationID))
  # st_buffer(polygon, 0) fixes any unintended topology errors in the above step
  
  
  streams1 <- streamCalcs(testnhd, wshdPolyOptions[1,])
  streams <- rbind(streams,streams1)
  streams <- streams[complete.cases(streams$StationID),]
}

# Add to final results
Result <- left_join(Result,streams, by=c('StationID')) # only join on StationID bc stream data same for all years
write.csv(streams,paste(saveHere,'/streams.csv', sep=''), row.names= F)
write.csv(Result,paste(saveHere,'/Results5.csv', sep=''), row.names= F)
rm(nhd); rm(streams); rm(streams1); rm(wshdPolyOptions); rm(testnhd)





######## Elevation Calculations 
DEM <- raster(paste(wd,'/vaelevation.TIF',sep=''))
# Set up dataframe to store elevation data
elev <- data.frame(StationID=NA,ELEVMIN=NA,ELEVMAX=NA,ELEVMEAN=NA,ELEVSD=NA,ELEVRANGE=NA)


for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  e <- areaCalcs(DEM, wshdPolyOptions[1,], 'ELEV')
  elev <- rbind(elev,e)
  elev <- elev[complete.cases(elev$StationID),]
}

# Add to final results
Result <- left_join(Result,elev, by=c('StationID')) # only join on StationID bc elevation data same for all years
write.csv(elev,paste(saveHere,'/elev.csv', sep=''), row.names= F)
write.csv(Result,paste(saveHere,'/Results6.csv', sep=''), row.names= F)
rm(DEM); rm(wshdPolyOptions); rm(elev); rm(e)



####### Slope Calculations 
# Too large to bring in whole layer, clipped to bounding box in GIS first, exported as .TIFF from GIS
slope <-  raster(paste(wd,'/slope2010.TIF',sep=''))

# Set up dataframe to store slope data
slp <- data.frame(StationID=NA,SLPMIN=NA,SLPMAX=NA,SLPMEAN=NA,SLPSD=NA,SLPRANGE=NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  e <- areaCalcs(slope, wshdPolyOptions[1,], 'SLP')
  slp <- rbind(slp,e)
  slp <- slp[complete.cases(slp$StationID),]
}

# Add to final results
Result <- merge(Result,slp, by='StationID')  # only join on StationID bc slope data same for all years
write.csv(slp,paste(saveHere,'/slp.csv', sep=''), row.names= F)
write.csv(Result,paste(saveHere,'/Results7.csv', sep=''), row.names= F)
rm(slope); rm(wshdPolyOptions); rm(slp); rm(e)



####### Rainfall Calculations 
# Bring in rainfall raster
rainfall <- raster(paste(wd,'/rainfall21.TIF',sep=''))
# Set up dataframe to store rainfall data
rain <- data.frame(StationID=NA,wshdRain_mmyr=NA,siteRain_mmyr=NA,wshdRain_inyr=NA,siteRain_inyr=NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  wshdPointOptions <- filter(wshdSites, StationID %in% uniqueWshdList[i])
  
  e <- rainfallCalc(rainfall, wshdPolyOptions[1,], wshdPointOptions[1,])
  rain <- rbind(rain,e)
  rain <- rain[complete.cases(rain$StationID),]
}


# Add to final results
Result <- merge(Result,rain, by='StationID')  # only join on StationID bc rain data same for all years
write.csv(rain,paste(saveHere,'/rain.csv', sep=''), row.names= F)
write.csv(Result,paste(saveHere,'/Result8.csv', sep=''), row.names= F)
rm(rainfall); rm(wshdPolyOptions); rm(wshdPointOptions); rm(rain); rm(e)#remove raster to increase memory availability




########### Population Density Calculations 
# Bring in clipped block census data
pop2000 <- st_read(paste0(wd,'/pop2000final.shp')) %>% 
  st_transform(st_crs(wshdPolys))

pop2000results <- data.frame(StationID=NA,wshdPOP2000=NA,POPDENS2000= NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  pop_ <- popCalculation(pop2000, st_buffer(wshdPolyOptions[1,],0), POP2000, 2000) # buffer helps with topology issues
  pop2000results <- rbind(pop2000results,pop_)
  pop2000results <- pop2000results[complete.cases(pop2000results$StationID),]
}

rm(pop2000); rm(pop_)

# 2000 and 2010 are read in separately to keep functions running faster and minimize data sitting in memory
pop2010 <-  st_read(paste0(wd,'/pop2010final.shp'))

pop2010results <- data.frame(StationID=NA,wshdPOP2010=NA,POPDENS2010= NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  pop_ <- popCalculation(pop2010, st_buffer(wshdPolyOptions[1,],0), POP10, 2010) # buffer helps with topology issues
  pop2010results <- rbind(pop2010results,pop_)
  pop2010results <- pop2010results[complete.cases(pop2010results$StationID),]
}

rm(pop2010);rm(pop_)


# 2020 census population data information
pop2020 <-  st_read(paste0(wd,'/pop2020.shp')) %>% 
  st_transform(st_crs(wshdPolys)) %>% 
  mutate(AREA = as.numeric(st_area(.))) # polygon area in m^2

pop2020results <- data.frame(StationID=NA,wshdPOP2020=NA,POPDENS2020= NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  pop_ <- popCalculation(pop2020, st_buffer(wshdPolyOptions[1,],0), POP20, 2020) # buffer helps with topology issues
  pop2020results <- rbind(pop2020results,pop_)
  pop2020results <- pop2020results[complete.cases(pop2020results$StationID),]
}
# Result <- read.csv(paste0(saveHere,'Result9.csv')) %>% 
#   dplyr::select(StationID:siteRain_inyr)
# 
# pop2000results <- dplyr::select(Result, StationID, wshdPOP2000,  POPDENS2000)
# pop2010results <- dplyr::select(Result, StationID, wshdPOP2010,  POPDENS2010)

population <- left_join(pop2000results, pop2010results, by = 'StationID') %>%
  left_join(pop2020results, by = 'StationID') %>% 
  mutate(POPCHG2000_2010 = ((POPDENS2010 - POPDENS2000) / POPDENS2000) * 100,
         POPCHG2010_2020 = ((POPDENS2020 - POPDENS2010) / POPDENS2010) * 100,
         POPCHG2000_2020 = ((POPDENS2020 - POPDENS2000) / POPDENS2000) * 100)


# Add to final results
Result <- left_join(Result,population,by='StationID') # only join on StationID bc elevation data same for all years
write.csv(Result,paste0(saveHere,'Result9.csv'), row.names= F)
write.csv(population,paste0(saveHere,'/population.csv', sep=''), row.names= F)
rm(population); rm(pop2010results); rm(pop2000results); rm(wshdPolyOptions)
rm(pop2020results)





############################## Road Density Calculations ###################################################
# Bring in NHD polyline file
nhd <- st_read(paste0(wd,'/nhd_83albers.shp')) %>% 
  st_transform(st_crs(wshdPolys))

roaddf <- data.frame(StationID=NA, roadYear= NA, RDLEN=NA, STRMLEN= NA, RDLEN120=NA, wshd_sqkm=NA, area120_sqkm=NA, STXRD_CNT=NA)

# Tiger roads available annually from 2006-2018 (at time of writing script),
# however, the FTP download process changes from a numeric system for localities
# to a more verbose file structure (read: difficult to automate) such that
# road files are only organized from 2010-2018 because of the labor intensive nature of the
# task. Watersheds sampled before 2010 need to be compared to 2010 TIGER roads file
uniqueWshdListYear <- mutate(uniqueWshdListYear, 
                             roadYear = case_when( between(as.numeric(YearSampled), 2000, 2010) ~ 2010,
                                                   TRUE ~ as.numeric(YearSampled) ))


#temporary fix while files backing up to external drive
roadwd <- 'GISdata/TIGERroads'



# First need to build function (loop) that runs each watershed in a given year
for(yr in unique(uniqueWshdListYear$roadYear)){ # only need to bring in road file once per year
  
  stationsToProcess <- filter(uniqueWshdListYear, roadYear %in% yr)
  
  print(paste('Processing roads in year:',unique(stationsToProcess$roadYear)))
  
  # replace this with unique years once that is organize correctly
  roadFile <- st_read(paste0(roadwd, '/', yr, 'tigerRoads.shp'))  %>% 
    st_transform(st_crs(wshdPolys))
  
  for(k in 1:nrow(stationsToProcess)){
    print(paste('       Processing watershed',k,' of', nrow(stationsToProcess)))
    
    # get watershed polygon based on StationID and NLCDyear combination
    wshdPolyOptions <- filter(wshdPolys, StationID %in% stationsToProcess$StationID[k])
    
    # Subset nhd streams by each polygon
    testnhd <- suppressWarnings(st_intersection(nhd, st_buffer(wshdPolyOptions[1,], 0))) %>% # only need to use one polygon to do analysis
      mutate(StationID = unique(wshdPolyOptions$StationID))
    # st_buffer(polygon, 0) fixes any unintended topology errors in the above step
    
    testroads <- suppressWarnings(st_intersection(roadFile, st_buffer(wshdPolyOptions[1,], 0))) %>%  # clip roads to watershed of interest
      mutate(roadYear = unique(stationsToProcess$roadYear)) # if multiple years sampled, correctly designate which year of roads testing
    # st_buffer(polygon, 0) fixes any unintended topology errors in the above step
    
    roads1 <- roadCalculation(testroads, testnhd, wshdPolyOptions[1,], yr)
    roaddf <- rbind(roaddf,roads1)
    roaddf <- roaddf[complete.cases(roaddf$StationID),]
  }
  
  
  rm(roadFile) # clean up memory after each year runs
  
}

roadFinal <- roadSummary(roaddf, uniqueWshdListYear)


# Add to final results
Result <- left_join(Result,roadFinal, by=c('StationID','YearSampled'))
write.csv(Result,paste0(saveHere,'Result10.csv'), row.names= F)
write.csv(roaddf,paste0(saveHere,'roaddf.csv', sep=''), row.names= F)

rm(testnhd);rm(wshdPolyOptions); rm(testroads); rm(roads1); rm(roaddf); rm(roadFinal); rm(stationsToProcess)



