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


# Establish a GIS working directory
# This is where you will source all static GIS data for the project (not your input watersheds)
wd <- "F:/evjones/GIS/ProbMonGIS/GISdata"

# Where do you want to save the outputs? 
saveHere <- 'Results/ProbRedo2001_2016/missing/'


# Bring in watersheds
wshdPolys <- st_read('GISdata/EmmaMessAround/missing2001-2016watersheds.shp') %>%
  #filter(StationID %in% c("4AXOD000.38", "4AXOE001.26", "4AXOK000.29", "4AXOL000.94",'1ACAH001.82')) %>%
  dplyr::select(StationID) %>%
  mutate(StationID = sub("\r\n" ,"",StationID)) # get rid of any stray spaces after StationID in attribute table
#wshdPolys <- st_read('GISdata/AllWatersheds_through2016.shp') %>%
#  #filter(StationID %in% c("4AXOD000.38", "4AXOE001.26", "4AXOK000.29", "4AXOL000.94",'1ACAH001.82')) %>%
#  dplyr::select(StationID) %>%
#mutate(StationID = sub("\r\n" ,"",StationID)) # get rid of any stray spaces after StationID in attribute table

wshdSites <- st_read('GISdata/AllStations_through2016.shp') %>%
  #filter(StationID %in% c("4AXOD000.38", "4AXOE001.26", "4AXOK000.29", "4AXOL000.94",'1ACAH001.82')) %>%
  dplyr::select(StationID)%>%
  mutate(StationID = sub("\r\n" ,"",StationID)) # get rid of any stray spaces after StationID in attribute table


#wshdPolys <- st_read('GISdata/EmmaMessAround/TwinWatersheds.shp')
#wshdSites <- st_read('GISdata/EmmaMessAround/TwinSites.shp')
###wshdPolys@data$StationID <- sub("\r\n" ,"",wshdPolys@data$StationID) # get rid of any stray spaces after StationID in attribute table
###wshdSites@data$StationID <- sub("\r\n" ,"",wshdSites@data$StationID) # get rid of any stray spaces after StationID in attribute table

# Critical Link (file with StationID's linked to year sampled for correct NLCD)
# This file must have fields: StationID and Year_ where StationID=DEQStationID that
#   matches input watershed StationID's and Year_ is year sampled
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')%>%
  dplyr::select(StationID,Year,StationID_Trend,EcoRegion,BioRegion,Order,StreamSizeCat)%>%
  dplyr::rename(YearSampled=Year)

years <- filter(criticalLink, StationID %in% wshdPolys$StationID) %>%
  mutate(year = ifelse(!is.na(YearSampled),YearSampled,2016), # get rid of NA's, replace with 2016 for now bc most recent NLCD release
         NLCD = case_when( between(year, 2000, 2003) ~ 2001, 
                           between(year, 2004, 2008) ~ 2006,
                           between(year, 2009, 2013) ~ 2011,
                           between(year, 2014, 2020) ~ 2016)) # the upper bound of this will need to be changed when new NCLD released



wshdPolys <- left_join(wshdPolys, years, by = 'StationID') %>%
  # make sure there is a point for each watershed or rainfall will bomb out
  filter(StationID %in% unique(wshdSites$StationID)) %>%
  # make sure every watershed has a NLCD year
  filter(!is.na(NLCD))

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
  
  l <- landcoverCounts(template, get(paste0('landcover',unique(wshdPolyOptions$NLCD))),
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
nhd <- st_read(paste0(wd,'/nhd_83albers.shp'))


# Run riparian calculations
finalRiparian <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,RNAT1=NA,RFOR1=NA,RWETL1=NA,RSHRB1=NA
                            ,RNG1=NA,RBAR1=NA,RTotBAR1=NA,RHUM1=NA,RURB1=NA,RMBAR1=NA,RAGT1=NA,RAGP1=NA
                            ,RAGC1=NA,RNAT30=NA,RFOR30=NA,RWETL30=NA,RSHRB30=NA,RNG30=NA,RBAR30=NA
                            ,RTotBAR30=NA,RHUM30=NA,RURB30=NA,RMBAR30=NA,RAGT30=NA,RAGP30=NA,RAGC30=NA
                            ,RNAT120=NA,RFOR120=NA,RWETL120=NA,RSHRB120=NA,RNG120=NA,RBAR120=NA
                            ,RTotBAR120=NA,RHUM120=NA,RURB120=NA,RMBAR120=NA,RAGT120=NA,RAGP120=NA,RAGC120=NA) 


uniqueWshdListNLCD1 <- filter(uniqueWshdListNLCD, !(StationID %in% finalRiparian$StationID & YearSampled %in% finalRiparian$YearSampled))

for(i in 1:nrow(uniqueWshdListNLCD)){
  # Status update
  print(paste0('Processing site ',i, ' of ', nrow(uniqueWshdListNLCD)))
  
  
  # get watershed polygon based on StationID and NLCD year combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdListNLCD$StationID[i] &
                              NLCD %in% uniqueWshdListNLCD$NLCD[i])
  
  # Subset nhd streams by each polygon
  testnhd <- suppressWarnings(st_intersection(nhd, wshdPolyOptions[1,])) %>% # only need to use one polygon to do analysis
    mutate(StationID = unique(wshdPolyOptions$StationID), NLCDyear = unique(wshdPolyOptions$NLCD))
  
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
    finalRiparian <- rbind(finalRiparian, ripCalc(testnhd, get(paste0('landcover',unique(wshdPolyOptions$NLCD))),
                                                  uniqueWshdListNLCDYear))
  }
  finalRiparian <- finalRiparian[complete.cases(finalRiparian$StationID),]
}

Result <- left_join(landusewide,finalRiparian, by=c('StationID','YearSampled','NLCD'))

write.csv(finalRiparian,paste(saveHere,'finalRiparian3.csv',sep=''), row.names= F)
write.csv(Result,paste(saveHere,'Result1.csv',sep=''), row.names= F)
rm(testnhd); rm(finalRiparian)
rm(landcover2001);rm(landcover2006);rm(landcover2011); rm(landcover2016)#remove raster to increase memory availability
rm(template);rm(nhd);rm(wshdPolyOptions); rm(landusewide)



############################### % Impervious Calculations ##############################################

imperv2001 <- raster(paste0(wd,"/NLCD2001imp.TIF"))
imperv2006 <- raster(paste0(wd,"/NLCD2006imp.TIF"))
imperv2011  <- raster(paste0(wd,"/NLCD2011imp.TIF"))
imperv2016  <- raster(paste0(wd,"/NLCD2016imp.TIF")) 


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
  
  impervious <- impervousCounts(templatei, get(paste0('imperv',unique(wshdPolyOptions$NLCD))), wshdPolyOptions[1,])
  
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
vaVPDES <- st_read(paste0(wd, '/vpdesalbers.shp'))

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
dams <- st_read(paste0(wd,'/dam_albers.shp')) %>%
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
nhd <- st_read(paste0(wd,'/nhd_83albers.shp'))

streams <- data.frame(StationID=NA,STRMLEN=NA,STRMDENS=NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  # Subset nhd streams by each polygon
  testnhd <- suppressWarnings(st_intersection(nhd, wshdPolyOptions[1,])) %>% # only need to use one polygon to do analysis
    mutate(StationID = unique(wshdPolyOptions$StationID))
  
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
pop2000 <- st_read(paste0(wd,'/pop2000final.shp'))

pop2000results <- data.frame(StationID=NA,wshdPOP2000=NA,POPDENS2000= NA)

for(i in 1:length(uniqueWshdList)){ # only need to do this calculation once per polygon
  # Status update
  print(paste0('Processing site ',i, ' of ', length(uniqueWshdList)))
  
  # get watershed polygon based on StationID and NLCDyear combination
  wshdPolyOptions <- filter(wshdPolys, StationID %in% uniqueWshdList[i])
  
  pop_ <- popCalculation(pop2000, wshdPolyOptions[1,], POP2000, 2000)
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
  
  pop_ <- popCalculation(pop2010, wshdPolyOptions[1,], POP10, 2010)
  pop2010results <- rbind(pop2010results,pop_)
  pop2010results <- pop2010results[complete.cases(pop2010results$StationID),]
}

rm(pop2010);rm(pop_)
population <- left_join(pop2000results, pop2010results, by = 'StationID') %>%
  mutate(POPCHG2000_2010 = ((POPDENS2010 - POPDENS2000) / POPDENS2000) * 100)


# Add to final results
Result <- left_join(Result,population,by='StationID') # only join on StationID bc elevation data same for all years
write.csv(Result,paste(saveHere,'Result9.csv'), row.names= F)
write.csv(pop,paste(saveHere,'/population.csv', sep=''), row.names= F)
rm(population); rm(pop2010results); rm(pop2000results); rm(wshdPolyOptions)






############################## Road Density Calculations ###################################################
# Bring in NHD polyline file
nhd <- st_read(paste0(wd,'/nhd_83albers.shp'))

roaddf <- data.frame(StationID=NA, roadYear= NA, RDLEN=NA, STRMLEN= NA, RDLEN120=NA, wshd_sqkm=NA, area120_sqkm=NA, STXRD_CNT=NA)

# Tiger roads available annually from 2006-2018 (at time of writing script),
# however, the FTP download process changes from a numeric system for localities
# to a more verbose file structure (read: difficult to automate) such that
# road files are only organized from 2010-2018 because of the labor intensive nature of the
# task. Watersheds sampled before 2010 need to be compared to 2010 TIGER roads file
uniqueWshdListYear <- mutate(uniqueWshdListYear, 
                             roadYear = case_when( between(YearSampled, 2000, 2010) ~ 2010,
                                                    TRUE ~ YearSampled ))


#temporary fix while files backing up to external drive
roadwd <- 'GISdata/TIGERroads'



# First need to build function (loop) that runs each watershed in a given year
for(yr in unique(uniqueWshdListYear$roadYear)){ # only need to bring in road file once per year
  
  stationsToProcess <- filter(uniqueWshdListYear, roadYear %in% yr)
  
  print(paste('Processing roads in year:',unique(stationsToProcess$roadYear)))
  
  # replace this with unique years once that is organize correctly
  roadFile <- st_read(paste0(roadwd, '/', yr, 'tigerRoads.shp')) ######################################################## FIX THIS LATER ############################################################
  
  for(k in 1:nrow(stationsToProcess)){
    print(paste('       Processing watershed',k,' of', nrow(stationsToProcess)))
    
    # get watershed polygon based on StationID and NLCDyear combination
    wshdPolyOptions <- filter(wshdPolys, StationID %in% stationsToProcess$StationID[k])
    
    # Subset nhd streams by each polygon
    testnhd <- suppressWarnings(st_intersection(nhd, wshdPolyOptions[1,])) %>% # only need to use one polygon to do analysis
      mutate(StationID = unique(wshdPolyOptions$StationID))
    
    testroads <- suppressWarnings(st_intersection(roadFile, wshdPolyOptions[1,])) %>%  # clip roads to watershed of interest
      mutate(roadYear = unique(stationsToProcess$roadYear)) # if multiple years sampled, correctly designate which year of roads testing

    roads1 <- roadCalculation(testroads, testnhd, wshdPolyOptions[1,], yr)
    roaddf <- rbind(roaddf,roads1)
    roaddf <- roaddf[complete.cases(roaddf$StationID),]
  }
  
  
  rm(roadFile) # clean up memory after each year runs
  
}

roadFinal <- roadSummary(roaddf, uniqueWshdListYear)


# Add to final results
Result <- left_join(Result,roadFinal, by=c('StationID','YearSampled'))
write.csv(Result,paste(saveHere,'Result10.csv'), row.names= F)
write.csv(roaddf,paste(saveHere,'roaddf.csv', sep=''), row.names= F)

rm(testnhd);rm(wshdPolyOptions); rm(testroads); rm(roads1); rm(roaddf); rm(roadFinal); rm(stationsToProcess)



