
landcoverCounts <- function(template, landcover, wshdPoly){
  e <- extract(landcover, wshdPoly, small=T, na.rm=F)
  # if length of e > 1 (likely for riparian use) then combine to a single list
  if(length(e) > 1){
    e1 <- unlist(e, recursive = FALSE)
    e <- list(e1)  } # have to unlist multiple lists but then still put back into single list to lapply table function
  
  et <- lapply(e,table) %>%
    as.data.frame() %>%
    mutate(StationID = unique(wshdPoly$StationID),
           colNames = paste0('VALUE_',Var1)) %>% # rename so dont start column name with a number
    dplyr::select(-Var1) %>% # drop column so it won't mess things up going wide
    pivot_wider(names_from = colNames, values_from = Freq) 
  results <- suppressWarnings(bind_rows(template,et) %>%
                                mutate_if(is.numeric, ~replace_na(., 0)) %>%
                                mutate(NLCD = unique(wshdPoly$NLCD),
                                       sqMi = sum(as.numeric(st_area(wshdPoly))) * 0.00000038610) %>% # area comes out in m^2 so convert to sq miles
                                dplyr::select(StationID, NLCD, everything(),sqMi) %>%
                                filter(!(StationID == 'template'))) # drop dummy row
  return(results)
}



landuseDataManagement <- function(x, uniqueWshdListNLCDYear){ #x is dataframe 
  z <- x %>%
    group_by(StationID,  NLCD) %>%
    pivot_longer(cols = starts_with("VALUE_"),names_to = "variable", 
                 values_to = 'value') 
  
  landusewide <- z %>%
    mutate(sqMile = value * 0.000347492,
           # Since any cell that is touched by polygon is counted, it is highly likely that the raster
           # area is larger than the polygon area. Thus, we need to do the raster math (percentages)
           # with the sq mileage of the rasters as the denominator instead of the polygon sq mileage to avoid
           # accidentally having percentages over 100%
           rasterTotalArea_sqMile = sum(sqMile), 
           totalArea_sqMile = sqMi, # polygon area used for watershed area reported to accurately represent area for trend sites
           # given thata cells can shift slightly, if watershed area was purely based on cell counts then the area could
           # change from NLCD release to NLCD release
           Water_sqMile = sum(sqMile[which(variable=='VALUE_11')]),
           PWater=(Water_sqMile / rasterTotalArea_sqMile) *100,
           N_sqMile = sum(sqMile[which(variable %in% c('VALUE_31','VALUE_41','VALUE_42',
                                                       'VALUE_43','VALUE_51','VALUE_51',
                                                       'VALUE_71','VALUE_72','VALUE_73',
                                                       'VALUE_74','VALUE_91','VALUE_95'))]),
           N_INDEX = (N_sqMile / rasterTotalArea_sqMile) * 100,
           Forest_sqMile = sum(sqMile[which(variable %in% c('VALUE_41','VALUE_42','VALUE_43'))]),
           PFOR = (Forest_sqMile / rasterTotalArea_sqMile) * 100,
           Wetland_sqMile = sum(sqMile[which(variable %in% c('VALUE_90','VALUE_95'))]),
           PWETL = (Wetland_sqMile / rasterTotalArea_sqMile) * 100,
           Shrub_sqMile = sum(sqMile[which(variable %in% c('VALUE_51','VALUE_52'))]),
           PSHRB = (Shrub_sqMile / rasterTotalArea_sqMile) * 100,
           Ngrasslands_sqMile = sum(sqMile[which(variable %in% c('VALUE_71','VALUE_72',
                                                                 'VALUE_73','VALUE_74'))]),
           PNG = (Ngrasslands_sqMile / rasterTotalArea_sqMile) * 100,
           Barren_sqMile = sum(sqMile[which(variable =='VALUE_31')]),
           PBAR = (Barren_sqMile / rasterTotalArea_sqMile) * 100,
           TotBarren_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_31'))]),
           PTotBAR = (TotBarren_sqMile / rasterTotalArea_sqMile) * 100,
           U_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22','VALUE_23',
                                                       'VALUE_24','VALUE_81','VALUE_82'))]),
           U_INDEX = (U_sqMile / rasterTotalArea_sqMile) * 100,
           Urban_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22',
                                                           'VALUE_23','VALUE_24'))]),
           PURB = (Urban_sqMile / rasterTotalArea_sqMile) * 100,
           MBAR_sqMile = sum(sqMile[which(variable == 'VALUE_21')]),
           PMBAR = (MBAR_sqMile / rasterTotalArea_sqMile) * 100,
           AGT_sqMile = sum(sqMile[which(variable %in% c('VALUE_81','VALUE_82'))]),
           PAGT = (AGT_sqMile / rasterTotalArea_sqMile) * 100,
           AGP_sqMile = sum(sqMile[which(variable == 'VALUE_81')]),
           PAGP = (AGP_sqMile / rasterTotalArea_sqMile) * 100,
           AGC_sqMile = sum(sqMile[which(variable == 'VALUE_82')]),
           PAGC = (AGC_sqMile / rasterTotalArea_sqMile) * 100) %>%
    distinct(StationID, NLCD, .keep_all = T) %>%
    dplyr::select(StationID, NLCD, totalArea_sqMile, PWater, N_INDEX, PFOR, PWETL, 
                  PSHRB, PNG, PBAR, PTotBAR, U_INDEX, PURB, PMBAR, PAGT, PAGP, PAGC)
  
  diversity <- z %>% # still grouped on StationID and Year
    # Complete diversity calculations
    mutate( Count = ifelse(value>0,1,0),
            m = sum(Count),
            Psum = sum(value),
            P_i = value / Psum,
            P_i_ln_1 = P_i*(log(P_i)),
            P_i2_1 = (value / Psum) ^2,
            P_i_ln = sum(P_i_ln_1, na.rm = T),
            P_i2 = sum(P_i2_1, na.rm = T)) %>%
    distinct(StationID, NLCD, .keep_all = T) %>%
    mutate(S = m,
           H = -(P_i_ln),
           Hprime = H / log(S),
           C = 1 - sum(P_i2)) %>%
    dplyr::select(StationID, NLCD, S, H, Hprime, C) 
  
  # Join back landusewide data
  Result <- left_join(landusewide, diversity, by = c('StationID', 'NLCD')) %>%
    left_join(uniqueWshdListNLCDYear, by = c('StationID','NLCD')) %>%
    dplyr::select(StationID, YearSampled, NLCD, everything())
  
  
  return(Result)}







riparianDataManagement <- function(x){ #x is dataframe 
  z <- x %>%
    group_by(StationID, NLCD, bufferWidth) %>%
    pivot_longer(cols = starts_with("VALUE_"),names_to = "variable", 
                 values_to = 'value') 
  
  landusewide <- z %>%
    mutate(sqMile = value * 0.000347492,
           # total area needs to be calculated slightly differently for riparian because any cells
           # touched by the buffer are included, which makes it quite easy to have value areas larger
           # than the input buffer polygon area
           rasterTotalArea_sqMile = sum(sqMile), # old way of calculating total area of watershed but if cells shift then gets wonky for trend sites
           N_sqMile = sum(sqMile[which(variable %in% c('VALUE_31','VALUE_41','VALUE_42',
                                                       'VALUE_43','VALUE_51','VALUE_51',
                                                       'VALUE_71','VALUE_72','VALUE_73',
                                                       'VALUE_74','VALUE_91','VALUE_95'))]),
           RNAT = (N_sqMile / rasterTotalArea_sqMile) * 100,
           Forest_sqMile = sum(sqMile[which(variable %in% c('VALUE_41','VALUE_42','VALUE_43'))]),
           RFOR = (Forest_sqMile / rasterTotalArea_sqMile) * 100,
           Wetland_sqMile = sum(sqMile[which(variable %in% c('VALUE_90','VALUE_95'))]),
           RWETL = (Wetland_sqMile / rasterTotalArea_sqMile) * 100,
           Shrub_sqMile = sum(sqMile[which(variable %in% c('VALUE_51','VALUE_52'))]),
           RSHRB = (Shrub_sqMile / rasterTotalArea_sqMile) * 100,
           Ngrasslands_sqMile = sum(sqMile[which(variable %in% c('VALUE_71','VALUE_72',
                                                                 'VALUE_73','VALUE_74'))]),
           RNG = (Ngrasslands_sqMile / rasterTotalArea_sqMile) * 100,
           Barren_sqMile = sum(sqMile[which(variable =='VALUE_31')]),
           RBAR = (Barren_sqMile / rasterTotalArea_sqMile) * 100,
           TotBarren_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_31'))]),
           RTotBAR = (TotBarren_sqMile / rasterTotalArea_sqMile) * 100,
           U_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22','VALUE_23',
                                                       'VALUE_24','VALUE_81','VALUE_82'))]),
           RHUM = (U_sqMile / rasterTotalArea_sqMile) * 100,
           Urban_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22',
                                                           'VALUE_23','VALUE_24'))]),
           RURB = (Urban_sqMile / rasterTotalArea_sqMile) * 100,
           MBAR_sqMile = sum(sqMile[which(variable == 'VALUE_21')]),
           RMBAR = (MBAR_sqMile / rasterTotalArea_sqMile) * 100,
           AGT_sqMile = sum(sqMile[which(variable %in% c('VALUE_81','VALUE_82'))]),
           RAGT = (AGT_sqMile / rasterTotalArea_sqMile) * 100,
           AGP_sqMile = sum(sqMile[which(variable == 'VALUE_81')]),
           RAGP = (AGP_sqMile / rasterTotalArea_sqMile) * 100,
           AGC_sqMile = sum(sqMile[which(variable == 'VALUE_82')]),
           RAGC = (AGC_sqMile / rasterTotalArea_sqMile) * 100) %>%
    distinct(StationID, NLCD, bufferWidth, .keep_all = T) %>%
    dplyr::select(StationID, NLCD, bufferWidth, RNAT, RFOR, RWETL, 
                  RSHRB, RNG, RBAR, RTotBAR, RHUM, RURB, RMBAR, RAGT, RAGP, RAGC) %>%
    # add buffer distance to variable name
    pivot_longer(cols = starts_with("R"), names_to = "variable", values_to = "percent") %>%
    mutate(finalVarName = paste0(variable,bufferWidth)) %>%
    ungroup() %>%
    group_by(StationID, NLCD) %>%
    dplyr::select(-c(variable, bufferWidth)) %>%
    pivot_wider(names_from = 'finalVarName', values_from = 'percent') %>%
    ungroup() 
  
  return(landusewide)}



ripCalc <- function(testnhd, landcover, uniqueWshdListNLCDYear){
  # Buffer just the stream segments in selected watershed
  riparianLanduse <- landcoverCounts(template, landcover, st_buffer(testnhd, dist = 1)) %>%
    mutate(bufferWidth = 1) %>%
    bind_rows( landcoverCounts(template, landcover, st_buffer(testnhd, dist = 30)) %>%
                 mutate(bufferWidth = 30) ) %>% 
    bind_rows( landcoverCounts(template, landcover, st_buffer(testnhd, dist = 120)) %>%
                 mutate(bufferWidth = 120) )
  riparianDataManagement(riparianLanduse) %>%
    left_join(uniqueWshdListNLCDYear, by = c('StationID', 'NLCD')) %>%
    dplyr::select(StationID, YearSampled, NLCD, everything())
}  



impervousCounts <- function(template, landcover, wshdPoly){
  e <- extract(landcover, wshdPoly, small=T, na.rm=F)
  et <- lapply(e,table) %>%
    as.data.frame() %>%
    mutate(StationID = wshdPoly$StationID,
           colNames = paste0('PCT',Var1)) %>% # rename so dont start column name with a number
    dplyr::select(-Var1) %>% # drop column so it won't mess things up going wide
    pivot_wider(names_from = colNames, values_from = Freq) 
  results <- suppressWarnings(bind_rows(template,et) %>%
                                mutate_if(is.numeric, ~replace_na(., 0)) %>%
                                mutate(NLCD = wshdPoly$NLCD,
                                       sqMi = as.numeric(st_area(wshdPoly)) * 0.00000038610) %>% # area comes out in m^2 so convert to sq miles
                                dplyr::select(StationID,  NLCD, everything(),sqMi) %>%
                                filter(!(StationID == 'template'))) # drop dummy row
  return(results)
} 


imperviousDataManagement <- function(x, uniqueWshdListNLCDYear){# x is a df
  x %>%
    group_by(StationID,  NLCD,sqMi) %>%
    pivot_longer(cols = starts_with("PCT"),names_to = "variable", 
                 values_to = 'value') %>%
    mutate(#sqMile = 900*value*0.0002471053814672*0.0015625, 
      sqMile = value * 0.000347492,
      PCT = as.numeric(substr(variable, 4,length(variable))), 
      sqMileImp=sqMile*(PCT/100)) %>%
    summarize(totalArea_sqMile=sum(sqMile, na.rm = T),
              sqMileImp=sum(sqMileImp, na.rm = T)) %>%
    mutate(wshdImpPCT = (sqMileImp / totalArea_sqMile) * 100) %>%
    dplyr::select(-c(sqMi, totalArea_sqMile)) %>%
    ungroup() %>%
    left_join(uniqueWshdListNLCDYear, by =  c('StationID','NLCD')) %>%
    dplyr::select(StationID, YearSampled, NLCD, everything())
}


pointCount <- function(pointFile, polygonFile){
  s <- pointFile[polygonFile,] %>%
    st_drop_geometry()
  # if no points found then make a dummy row
  if(nrow(s)==0){ suppressWarnings(s[1,] <- 0) }
  s <- mutate(s, StationID=polygonFile$StationID)
  return(s)
}



VPDESdataManagement <- function(x, uniqueWshdListNLCDYear){# x is a dataframe
  # Mark permits according to classification
  vaVPDES2 <- mutate(x,MunMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Municipal'),1,0)
                     ,MunMinor=ifelse(c(MAJOR__MIN=='Minor'& MUNICIPAL_=='Municipal'),1,0)
                     ,IndMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Industrial'),1,0)
                     ,IndMinor=ifelse(c(MAJOR__MIN=='Minor'&MUNICIPAL_=='Industrial'),1,0)) 
  suppressWarnings(vaVPDES2[is.na(vaVPDES2)] <- 0) # make 0 to enable subsequent sum function
  # Sum all permit types based on StationID
  vaVPDES2 %>%
    group_by(StationID) %>%
    summarize(MunMajor=sum(MunMajor),
              MunMinor=sum(MunMinor),
              IndMajor=sum(IndMajor),
              IndMinor=sum(IndMinor)) %>%
    ungroup() %>%
    left_join(uniqueWshdListNLCDYear, by = 'StationID') %>%
    dplyr::select(StationID, YearSampled, NLCD, everything())
}


damDataManagement <- function(x, uniqueWshdListNLCDYear){#x is a dataframe
  x %>%
    group_by(StationID) %>%
    mutate(dam = ifelse(NID_HEIGHT == 0, 0, 1)) %>%
    summarize(damcount = sum(dam)) %>%
    ungroup() %>%
    left_join(uniqueWshdListNLCDYear, by = 'StationID') %>%
    dplyr::select(StationID, YearSampled, NLCD, everything())
  
}




streamCalcs <- function(testnhd, wshdPoly){
  if(nrow(testnhd)==0){
    streams <- data.frame(StationID=wshdPoly$StationID,STRMLEN=NA,STRMDENS=NA)
  }else{
    streams <- wshdPoly %>%
      st_drop_geometry() %>%
      mutate(STRMLEN = as.numeric(sum(st_length(testnhd))),
             STRMDENS = (STRMLEN / 1000) / (as.numeric(st_area(wshdPoly)) / 1000000)) %>% # convert stream length to km and polygon area from m2 to km2 to get density
      dplyr::select(StationID, STRMLEN, STRMDENS)
  }
  return(streams)
}



areaCalcs <- function(rasterLayer, wshdPoly, varName){
  e <- extract(rasterLayer, wshdPoly, small=T, na.rm=F)
  
  df <- data.frame(StationID=wshdPoly$StationID) %>%
    mutate(MIN = as.numeric(sapply(e, FUN=min, na.rm=T)),
           MAX = as.numeric(sapply(e, FUN=max, na.rm=T)),
           MEAN = as.numeric(sapply(e, FUN=mean, na.rm=T)),
           SD = as.numeric(sapply(e, FUN=sd, na.rm=T)),
           RANGE = MAX - MIN) 
  # fix names based on variable testing
  names(df)[2:length(df)] <- paste0(varName,names(df)[2:length(df)])
  
  return(df)
}


rainfallCalc <- function(rasterLayer, wshdPoly, wshdPoint){
  e_poly <- extract(rasterLayer, wshdPoly, small=T, na.rm=F)
  e_point <- extract(rasterLayer, wshdPoint, small=T, na.rm=F)
  
  data.frame(StationID = wshdPoly$StationID) %>%
    mutate(wshdRain_mmyr = as.numeric((sapply(e_poly,sum)/100)),
           siteRain_mmyr = as.numeric((sapply(e_point,sum)/100)),
           wshdRain_inyr = wshdRain_mmyr * 0.0393701,
           siteRain_inyr = siteRain_mmyr * 0.0393701)
}





popCalculation <- function(popYearLayer, wshdPoly, populationField, censusYear){
  
  quoPopulationField <- enquo(populationField)
  
  blockpop <- suppressWarnings(st_intersection(wshdPoly, popYearLayer))
  
  popSummary <- blockpop %>%
    mutate(clippedArea = as.numeric(st_area(blockpop)),  # area of clipped part of block in m^2  
           blockPOP = (clippedArea / AREA) * !! quoPopulationField) %>% # get % of block watershed intersects and multiply that by population in block to get approximate population in that piece of block
    st_drop_geometry() %>%
    summarize(wshdPOP = sum(blockPOP)) %>%
    mutate(POPDENS = wshdPOP / (as.numeric(st_area(wshdPoly)) / 1000000), # convert area in m^2 to km^2 to get population per km^2
           StationID = wshdPoly$StationID) %>%
    dplyr::select(StationID, everything())
  
  # fix column names to reflect year of census
  names(popSummary)[2:3] <- paste0( names(popSummary)[2:3], censusYear)
  
  return(popSummary)
}








roadCalculation <- function(testroads, testnhd, wshdPoly){
  bufferNHD <- st_buffer(testnhd, dist = 120) # buffer all streams in watershed to riparian buffer distance
  roadInRiparianBuffer <- suppressWarnings(st_intersection(testroads, bufferNHD)) # cut roads to riparian buffer
  streamXroad <- suppressWarnings(st_intersection(testroads,testnhd)) # find all stream/road crossings
  
  return(data.frame(StationID = unique(wshdPoly$StationID),
                    roadYear = unique(testroads$roadYear),
                    wshd_sqkm = sum(as.numeric(st_area(wshdPoly)))*1e-6, # save area of watershed
                    area120_sqkm = sum(as.numeric(st_area(bufferNHD)))*1e-6, # save area of 120m buffer
                    RDLEN = sum(as.numeric(st_length(testroads))), # calculate road length throughout watershed (in meters)
                    STRMLEN = sum(as.numeric(st_length(testnhd))), # calculate stream length throughout watershed (in meters)
                    RDLEN120 = sum(as.numeric(st_length(roadInRiparianBuffer))), # calculate road length in riparian buffer (in meters)
                    STXRD_CNT = nrow(streamXroad) ) )# calculate number of stream/road 
}



roadSummary <- function(roaddf, uniqueWshdListYear){
  mutate(roaddf, 
         roadlength_km = RDLEN / 1000,
         RDDENS = roadlength_km / wshd_sqkm,
         roadlength120_km = RDLEN120 / 1000,
         RDDENS = roadlength120_km / area120_sqkm,
         pctRoadLengthInRiparian = (roadlength120_km / roadlength_km) * 100,
         streamlength_km = STRMLEN / 1000,
         STXRD = STXRD_CNT / streamlength_km) %>%
    # fix YearSampled to match actual year sampled for joining
    left_join(uniqueWshdListYear, by= c('StationID','roadYear'))  %>%
  dplyr::select(StationID, YearSampled, RDLEN, RDLEN120, STXRD_CNT, RDDENS, pctRoadLengthInRiparian, STXRD)
}
