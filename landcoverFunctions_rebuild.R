

landcoverCounts <- function(template, landcover, wshdPoly){
  e <- extract(landcover, wshdPoly, small=T, na.rm=F)
  et <- lapply(e,table) %>%
    as.data.frame() %>%
    mutate(StationID = wshdPoly$StationID,
           colNames = paste0('VALUE_',Var1)) %>% # rename so dont start column name with a number
    dplyr::select(-Var1) %>% # drop column so it won't mess things up going wide
    pivot_wider(names_from = colNames, values_from = Freq) 
  results <- suppressWarnings(bind_rows(template,et) %>%
                                mutate_if(is.numeric, ~replace_na(., 0)) %>%
                                mutate(YearSampled = wshdPoly$Year_,
                                       NLCD = wshdPoly$NLCDyear,
                                       sqMi = as.numeric(st_area(wshdPoly)) * 0.00000038610) %>% # area comes out in m^2 so convert to sq miles
                                dplyr::select(StationID, YearSampled, NLCD, everything(),sqMi) %>%
                                filter(!(StationID == 'template'))) # drop dummy row
  return(results)
}



landuseDataManagement <- function(x){ #x is dataframe 
  z <- x %>%
    group_by(StationID, YearSampled, NLCD) %>%
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
    distinct(StationID, YearSampled, NLCD, .keep_all = T) %>%
    dplyr::select(StationID, YearSampled, NLCD, totalArea_sqMile, PWater, N_INDEX, PFOR, PWETL, 
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
    distinct(StationID, YearSampled, NLCD, .keep_all = T) %>%
    mutate(S = m,
           H = -(P_i_ln),
           Hprime = H / log(S),
           C = 1 - sum(P_i2)) %>%
    dplyr::select(StationID, YearSampled, NLCD, S, H, Hprime, C) 
  
  # Join back landusewide data
  Result <- left_join(landusewide, diversity, by = c('StationID', 'YearSampled', 'NLCD')) %>%
    dplyr::select(StationID,YearSampled,NLCD,everything())
  
  return(Result)}
  
  
  
riparianDataManagement <- function(x){ #x is dataframe 
  z <- x %>%
    group_by(StationID, YearSampled, NLCD, bufferWidth) %>%
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
    distinct(StationID, YearSampled, NLCD, bufferWidth, .keep_all = T) %>%
    dplyr::select(StationID, YearSampled, NLCD, bufferWidth, RNAT, RFOR, RWETL, 
                  RSHRB, RNG, RBAR, RTotBAR, RHUM, RURB, RMBAR, RAGT, RAGP, RAGC) %>%
    # add buffer distance to variable name
    pivot_longer(cols = starts_with("R"), names_to = "variable", values_to = "percent") %>%
    mutate(finalVarName = paste0(variable,bufferWidth)) %>%
    ungroup() %>%
    group_by(StationID, YearSampled, NLCD) %>%
    dplyr::select(-c(variable, bufferWidth)) %>%
    pivot_wider(names_from = 'finalVarName', values_from = 'percent') %>%
    ungroup()
  
  return(landusewide)}
  
  
  
  

ripCalc <- function(testnhd, landcover){
  # Buffer just the stream segments in selected watershed
  riparianLanduse <- landcoverCounts(template, landcover, st_buffer(testnhd, dist = 1)) %>%
    mutate(bufferWidth = 1) %>%
    bind_rows( landcoverCounts(landcover, st_buffer(testnhd, dist = 30)) %>%
                 mutate(bufferWidth = 30) ) %>% 
    bind_rows( landcoverCounts(landcover, st_buffer(testnhd, dist = 120)) %>%
                 mutate(bufferWidth = 120) )
  riparianDataManagement(riparianLanduse)
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
                                mutate(YearSampled = wshdPoly$Year_,
                                       NLCD = wshdPoly$NLCDyear,
                                       sqMi = as.numeric(st_area(wshdPoly)) * 0.00000038610) %>% # area comes out in m^2 so convert to sq miles
                                dplyr::select(StationID, YearSampled, NLCD, everything(),sqMi) %>%
                                filter(!(StationID == 'template'))) # drop dummy row
  return(results)
} 




impervious <- impervousCounts(templatei, landcover, wshdPoly)
  


imperviousDataManagement <- function(x){# x is a df
  x %>%
    group_by(StationID, YearSampled, NLCD,sqMi) %>%
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
    ungroup()
}
  
















pointCount <- function(pointFile, polygonFile){
  s <- pointFile[polygonFile,] %>%
    st_drop_geometry()
  # if no points found then make a dummy row
  if(nrow(s)==0){ suppressWarnings(s[1,] <- 0) }
  s <- mutate(s, StationID=polygonFile$StationID,
              YearSampled = polygonFile$Year_,
              NLCD = polygonFile$NLCD)
  return(s)
}

pointCount(vaVPDES, wshdPolys[2,])
pointCount(dams, wshdPolys[2,])




VPDESdataManagement <- function(x){# x is a dataframe
  # Mark permits according to classification
  vaVPDES2 <- mutate(x,MunMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Municipal'),1,0)
                     ,MunMinor=ifelse(c(MAJOR__MIN=='Minor'& MUNICIPAL_=='Municipal'),1,0)
                     ,IndMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Industrial'),1,0)
                     ,IndMinor=ifelse(c(MAJOR__MIN=='Minor'&MUNICIPAL_=='Industrial'),1,0)) 
  suppressWarnings(vaVPDES2[is.na(vaVPDES2)] <- 0) # make 0 to enable subsequent sum function
  # Sum all permit types based on StationID
  vaVPDES2 %>%
    group_by(StationID, YearSampled, NLCD) %>%
    summarize(MunMajor=sum(MunMajor),
              MunMinor=sum(MunMinor),
              IndMajor=sum(IndMajor),
              IndMinor=sum(IndMinor)) %>%
    ungroup()}
            

damDataManagement <- function(x){#x is a dataframe
  x %>%
    group_by(StationID, YearSampled, NLCD) %>%
    mutate(dam = ifelse(NID_HEIGHT == 0, 0, 1)) %>%
    summarize(damcount = sum(dam)) %>%
    ungroup() }



damCount <- function(x){
  s <- dams[wshdPolys[x,],]
  if(!nrow(s@data)==0){
    damselect <- as.data.frame(s)
    damselect <- mutate(damselect, StationID=wshdPolys@data$StationID[x]
                        ,Dam= ifelse(is.na(NID_HEIGHT),0,1))%>%
      summarise(damcount=sum(Dam))%>%mutate(StationID=wshdPolys@data$StationID[x])%>%
      select(StationID,damcount)}
  if(nrow(s@data)==0){
    damselect <- data.frame(StationID=wshdPolys@data$StationID[x],damcount=0)}
  damsummary <- rbind(damsdf,damselect)
  damsummary <- damsummary[complete.cases(damsummary$StationID),]
  return(damsummary)
}


streamCalcs <- function(x){
  testnhd <- raster::intersect(nhd,wshdPolys[x,]) 
  if(length(testnhd)==0){
    streams <- data.frame(StationID=wshdPolys@data$StationID[x],YearSampled=landusewide$YearSampled[i],NLCD=NA,STRMLEN=NA,STRMDENS=NA)
  }else{
    streams <- data.frame(StationID=wshdPolys@data$StationID[x],STRMLEN=gLength(testnhd))%>%
      merge(landusewide[,c('StationID','YearSampled','NLCD','totalArea_sqMile')],by='StationID')%>%
      mutate(streamlength_km=STRMLEN/1000,STRMDENS=streamlength_km/(totalArea_sqMile*2.58999))%>%
      select(StationID,YearSampled,NLCD,STRMLEN,STRMDENS)
  }
  return(streams)
}




imperviousCalc <- function(x){
  years <- subset(criticalLink,StationID %in% wshdPolys[x,]$StationID)%>%
    mutate(year=ifelse(!is.na(Year_),Year_,2011)# get rid of NA's, replace with 2011 for now bc most recent NLCD release
           ,NLCDyear=ifelse(year>2000&year<2004,2001,ifelse(year>=2004&year<2009,2006,2011)))#relabel based on possible NLCD releases
  if(nrow(years)==0){# make up a fake df for years if there is not matching entry for wshdPolys$StationID in Jason's db
    years <- data.frame(sampleID=wshdPolys[x,]$StationID,strahler_order=NA,Longitude_DD=NA
                        ,Latitude_DD=NA,Year_=NA,StationID=wshdPolys[x,]$StationID,year=2011,NLCDyear=2011)}
  if(length(unique(years$NLCDyear))==1){# this means only one NLCD year needs to be run
    impervious <- if(unique(years$NLCDyear)==2001){imperv2001}else{if(unique(years$NLCDyear)==2006){imperv2006}else{imperv2011}}
    e <- extract(impervious, wshdPolys[x,],small=T, na.rm=F)
    et <- lapply(e,table)
    t <- melt(et)
    t.cast <- dcast(t, L1 ~ Var.1, sum)
    names(t.cast)[1] <- "StationID"
    print(t.cast[1,1] <- wshdList[x])
    colnames(t.cast)[2:length(names(t.cast))] <- paste("PCT",colnames(t.cast)
                                                       [2:length(names(t.cast))], sep = "")
    if(length(et[[1]])==101){
      results <- t.cast%>%mutate(NLCD=unique(years$NLCDyear))}
    if(length(et[[1]])<101){
      zeros = templatei[is.na(match(names(templatei), names(t.cast)))]
      zeros[1,] = 0
      results <- cbind(t.cast, zeros)%>%mutate(NLCD=unique(years$NLCDyear))}
    if(nrow(years)>1){# now deal with if more than 1 sample in that NLCD year
      results <- results[rep(seq_len(nrow(results)), each=nrow(years)),]
      results$YearSampled <- ifelse(is.na(years$Year_),years$Year_,years$year)
      row.names(results) <- 1:nrow(results) #change row.names
      results <- select(results,StationID,YearSampled,NLCD,everything())
    }else(#if only one row(year) just need to add on YearSampled
      results <- mutate(results,YearSampled=ifelse(is.na(years$Year_),years$Year_,years$year))%>%#correct for defaulting Year_=2011 when NA
        select(StationID,YearSampled,NLCD,everything()))
  }else{# this means more than 1 NLCD year needs to be run, must loop through them
    results <- templatei%>%mutate(StationID=NA,YearSampled=NA,NLCD=NA)%>%select(StationID,YearSampled,NLCD,everything())
    nNLCDyears <- aggregate(data.frame(count=years[,8]),list(NLCDyrs=years[,8]),length)
    for(i in 1:nrow(nNLCDyears)){
      impervious <- if(nNLCDyears[i,1]==2001){imperv2001}else{if(nNLCDyears[i,1]==2006){imperv2006}else{imperv2011}}
      e <- extract(impervious, wshdPolys[x,],small=T, na.rm=F)
      et <- lapply(e,table)
      t <- melt(et)
      t.cast <- dcast(t, L1 ~ Var.1, sum)
      names(t.cast)[1] <- "StationID"
      print(t.cast[1,1] <- wshdList[x])
      colnames(t.cast)[2:length(names(t.cast))] <- paste("PCT",colnames(t.cast)
                                                         [2:length(names(t.cast))], sep = "")
      if(length(et[[1]])==101){
        results_ <- t.cast%>%mutate(NLCD=nNLCDyears[i,1])}
      if(length(et[[1]])<101){
        zeros = templatei[is.na(match(names(templatei), names(t.cast)))]
        zeros[1,] = 0
        results_ <- cbind(t.cast, zeros)%>%mutate(NLCD=nNLCDyears[i,1])}
      if(nNLCDyears$count[i]>1){# now deal with if more than 1 sample in that NLCD year
        results_ <- results_[rep(seq_len(nrow(results_)), each=nNLCDyears$count[i]),]
        step1 <- subset(years,NLCDyear %in% nNLCDyears[i,1])
        results_$YearSampled <- ifelse(is.na(step1$Year_),step1$Year_,step1$year)
        row.names(results_) <- 1:nrow(results_) #change row.names
        results_ <- select(results_,StationID,YearSampled,NLCD,everything())
      }else{#if only one row(year) just need to add on YearSampled
        step1 <- subset(years,NLCDyear %in% nNLCDyears[i,1])
        results_ <- mutate(results_,YearSampled=ifelse(is.na(step1$Year_),step1$Year_,step1$year))%>%
          select(StationID,YearSampled,NLCD,everything())}
      results <- rbind(results,results_)
    }
  }
  results <- results[complete.cases(results$StationID),] #remove any placeholder rows
  return(results)
}

imperviousDataManagement <- function(x){# x is a df
  imperv <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,sqMileImp=NA,wshdImpPCT=NA)
  for(z in 1:nrow(x)){
    dfi0.5 <- x[z,1:3] # save year sampled and NLCD year info
    dfi1 <- melt(x[z,-c(2:3)],'StationID')
    imperv1 <- mutate(dfi1, sqMile=900*value*0.0002471053814672*0.0015625
                      ,PCT=as.numeric(substr(variable, 4,length(variable))),sqMileImp=sqMile*(PCT/100))
    imperv_ <- summarise(imperv1,StationID=StationID[1],totalArea_sqMile=sum(sqMile)
                         ,sqMileImp=sum(sqMileImp))%>%
      mutate(wshdImpPCT=(sqMileImp/totalArea_sqMile)*100)%>%
      join(dfi0.5,by='StationID')%>%select(StationID,YearSampled,NLCD,sqMileImp,wshdImpPCT)
    imperv <- rbind(imperv,imperv_)}
  imperv <- imperv[complete.cases(imperv$StationID),]
}




slopeCalcs <- function(x){
  print(paste(x,wshdPolys@data$StationID[x],sep=' '))
  e <-  extract(slope, wshdPolys[x,],small=T, na.rm=F) 
  SLPMIN <- as.numeric(sapply(e, FUN=min, na.rm=T))
  SLPMAX <- as.numeric(sapply(e, FUN=max, na.rm=T))
  SLPMEAN <- as.numeric(sapply(e, FUN=mean, na.rm=T))
  SLPSD <- as.numeric(sapply(e, FUN=sd, na.rm=T))
  t <- data.frame(StationID=wshdPolys@data$StationID[x])%>%
    mutate(SLPMIN=SLPMIN,SLPMAX=SLPMAX,SLPMEAN=SLPMEAN,SLPSD=SLPSD,SLPRANGE=SLPMAX-SLPMIN)
  return(t)
}



rainfall_shed <- function(x){
  e <- extract(rainfall, wshdPolys[x,],small=T, na.rm=F) 
  wshedRain_mmyr <- as.numeric((sapply(e,sum)/100))
  rain <- data.frame(StationID=wshdPolys@data$StationID[x])%>%
    mutate(wshdRain_mmyr=wshedRain_mmyr,wshedRain_inyr=wshedRain_mmyr*0.0393701)
  return(rain)
}
rainfall_site <- function(x){
  e2 <- extract(rainfall, wshdSites[x,])
  siteRain_mmyr <- as.numeric((sapply(e2,sum)/100))
  rain2 <- data.frame(StationID=wshdSites@data$StationID[x])%>%
    mutate(siteRain_mmyr=siteRain_mmyr,siteRain_inyr=siteRain_mmyr*0.0393701)
  return(rain2)
}



popCalculation <- function(x){
  print(x)
  # 2000 
  blockpop2000 <- suppressWarnings(raster::intersect(wshdPolys[x,],pop2000))
  if(nrow(blockpop2000)==0){ # special case if watershed polygon falls entirely within population block
    blockpop2000 <- pop2000[wshdPolys[x,],]
    blockpop2000$shapeAREA <- sapply(slot(wshdPolys[x,], "polygons"),slot, "area")
  }else{
    blockpop2000$shapeAREA <- sapply(slot(blockpop2000, "polygons"),slot, "area")}
  blockpop2000@data <- mutate(blockpop2000@data,blockPOP=(shapeAREA/AREA)*POP2000)
  wshdPOP2000 <- summarize(blockpop2000@data, wshdPOP2000=sum(blockPOP))
  # Repeat for 2010
  blockpop2010 <- raster::intersect(wshdPolys[x,],pop2010)
  if(nrow(blockpop2010)==0){ # special case if watershed polygon falls entirely within population block
    blockpop2010 <- pop2010[wshdPolys[x,],]
    blockpop2010$shapeAREA <- sapply(slot(wshdPolys[x,], "polygons"),slot, "area")
  }else{
    blockpop2010$shapeAREA <- sapply(slot(blockpop2010, "polygons"),slot, "area")}
  blockpop2010@data <- mutate(blockpop2010@data,blockPOP=(shapeAREA/AREA)*POP10)
  wshdPOP2010 <- summarize(blockpop2010@data, wshdPOP2010=sum(blockPOP))  
  wshdPOP <- data.frame(StationID=wshdPolys@data$StationID[x],cbind(wshdPOP2000,wshdPOP2010))
  return(wshdPOP)
}



roadCalculation <- function(x){
  testroads <- suppressWarnings(raster::intersect(roads,wshdPolys[x,])) # cut roads to watershed of interest
  wshd_sqkm <- gArea(wshdPolys[x,])*1e-6 # save area of watershed
  if(length(testroads)>0){
    RDLEN <- gLength(testroads) # calculate road length throughout watershed (in meters)
    testnhd <- suppressWarnings(raster::intersect(nhd,wshdPolys[x,])) # cut NHD to watershed of interest
    bufferNHD <- gBuffer(testnhd,width=120) # buffer all streams in watershed to riparian buffer distance
    area120_sqkm <- gArea(bufferNHD)*1e-6 # save area of 120m buffer
    roadInRiparianBuffer <- suppressWarnings(raster::intersect(roads,bufferNHD)) # cut roads to riparian buffer
    if(length(roadInRiparianBuffer)==0){
      RDLEN120 <- 0 # calculate road length in riparian buffer (in meters)
    }else{
      RDLEN120 <- gLength(roadInRiparianBuffer) # calculate road length in riparian buffer (in meters)
    }
    streamXroad <- gIntersection(testroads,testnhd) # find all stream/road crossings
    if(length(streamXroad)>0){
      STXRD_CNT <- nrow(as.matrix(streamXroad@coords))  # calculate number of stream/road crossings
    }else{
      STXRD_CNT <- 0 # set stream/road crossings to 0 if needed
      }
  }else{
    RDLEN <- 0
    RDLEN120 <- 0
    area120_sqkm <- 0
    STXRD_CNT <- 0}
  return(cbind(StationID=wshdList[x],RDLEN,RDLEN120, wshd_sqkm, area120_sqkm,STXRD_CNT))
}