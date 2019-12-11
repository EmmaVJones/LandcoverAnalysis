landcoverCounts <- function(landcover, wshdPoly){
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
                                       NLCD = wshdPoly$NLCDyear) %>%
                                dplyr::select(StationID, YearSampled, NLCD, everything()) %>%
                                filter(!(StationID == 'template'))) # drop dummy row
  return(results)
}



landuseDataManagement <- function(x){ #x is dataframe 
  Result <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,totalArea_sqMile=NA,PWater=NA,N_INDEX=NA
                       ,PFOR=NA,PWETL=NA,PSHRB=NA,PNG=NA,PBAR=NA,PTotBAR=NA,U_INDEX=NA,PURB=NA
                       ,PMBAR=NA,PAGT=NA,PAGP=NA,PAGC=NA,S=NA,H=NA,Hprime=NA,C=NA) 
z <- x %>%
    group_by(StationID, YearSampled) %>%
    pivot_longer(cols = starts_with("VALUE_"),names_to = "variable", 
                 values_to = 'value') 

landusewide_new <- z %>%
    mutate(acre = 900 * value * 0.0002471053814672,
           sqMile = acre*0.0015625,
           hectare = sqMile * 258.9988110336,
           totalArea_sqMile = sum(sqMile),
           Water_sqMile = sum(sqMile[which(variable=='VALUE_11')]),
           PWater=(Water_sqMile / totalArea_sqMile) *100,
           N_sqMile = sum(sqMile[which(variable %in% c('VALUE_31','VALUE_41','VALUE_42',
                                                       'VALUE_43','VALUE_51','VALUE_51',
                                                       'VALUE_71','VALUE_72','VALUE_73',
                                                       'VALUE_74','VALUE_91','VALUE_95'))]),
           N_INDEX = (N_sqMile / totalArea_sqMile) * 100,
           Forest_sqMile = sum(sqMile[which(variable %in% c('VALUE_41','VALUE_42','VALUE_43'))]),
           PFOR = (Forest_sqMile / totalArea_sqMile) * 100,
           Wetland_sqMile = sum(sqMile[which(variable %in% c('VALUE_90','VALUE_95'))]),
           PWETL = (Wetland_sqMile / totalArea_sqMile) * 100,
           Shrub_sqMile = sum(sqMile[which(variable %in% c('VALUE_51','VALUE_52'))]),
           PSHRB = (Shrub_sqMile / totalArea_sqMile) * 100,
           Ngrasslands_sqMile = sum(sqMile[which(variable %in% c('VALUE_71','VALUE_72',
                                                                 'VALUE_73','VALUE_74'))]),
           PNG = (Ngrasslands_sqMile / totalArea_sqMile) * 100,
           Barren_sqMile = sum(sqMile[which(variable =='VALUE_31')]),
           PBAR = (Barren_sqMile / totalArea_sqMile) * 100,
           TotBarren_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_31'))]),
           PTotBAR = (TotBarren_sqMile / totalArea_sqMile) * 100,
           U_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22','VALUE_23',
                                                       'VALUE_24','VALUE_81','VALUE_82'))]),
           U_INDEX = (U_sqMile / totalArea_sqMile) * 100,
           Urban_sqMile = sum(sqMile[which(variable %in% c('VALUE_21','VALUE_22',
                                                           'VALUE_23','VALUE_24'))]),
           PURB = (Urban_sqMile / totalArea_sqMile) * 100,
           MBAR_sqMile = sum(sqMile[which(variable == 'VALUE_21')]),
           PMBAR = (MBAR_sqMile / totalArea_sqMile) * 100,
           AGT_sqMile = sum(sqMile[which(variable %in% c('VALUE_81','VALUE_82'))]),
           PAGT = (AGT_sqMile / totalArea_sqMile) * 100,
           AGP_sqMile = sum(sqMile[which(variable == 'VALUE_81')]),
           PAGP = (AGP_sqMile / totalArea_sqMile) * 100,
           AGC_sqMile = sum(sqMile[which(variable == 'VALUE_82')]),
           PAGC = (AGC_sqMile / totalArea_sqMile) * 100) %>%
  distinct(StationID, YearSampled, NLCD, .keep_all = T) %>%
  dplyr::select(StationID, totalArea_sqMile, PWater, N_INDEX, PFOR, PWETL, 
                PSHRB, PNG, PBAR, U_INDEX, PURB, PMBAR, PAGT, PAGP, PAGC)
           
  
  

  
  for(z in 1:nrow(x)){
    df0.5 <- x[z,1:3] # save year sampled and NLCD year info
    df1 <- melt(x[z,-c(2:3)], 'StationID')
    df2 <- aggregate(. ~ StationID + variable, data=df1,sum)
    landuse <- mutate(df2, acre=900*value*0.0002471053814672,sqMile=acre*0.0015625
                      ,hectare=sqMile*258.9988110336)
    landuse2 <- aggregate(cbind(sqMile) ~ StationID, data=landuse, FUN='sum')
    names(landuse2) <- c('StationID','totalArea_sqMile')
    landuse3 <- merge(landuse, landuse2, by='StationID')
    landuse4 <- plyr::ddply(landuse3,c('StationID','variable'),mutate
                      ,Water_sqMile=sum(sqMile[(variable=='VALUE_11')])
                      ,PWater=(Water_sqMile/totalArea_sqMile)*100
                      ,N_sqMile=sum(sqMile[(variable=='VALUE_31')|(variable=='VALUE_41')
                                           |(variable=='VALUE_42')|(variable=='VALUE_43')
                                           |(variable=='VALUE_51')|(variable=='VALUE_52')
                                           |(variable=='VALUE_71')|(variable=='VALUE_72')
                                           |(variable=='VALUE_73')|(variable=='VALUE_74')
                                           |(variable=='VALUE_91')|(variable=='VALUE_95')])
                      ,N_INDEX=(N_sqMile/totalArea_sqMile)*100
                      ,Forest_sqMile=sum(sqMile[(variable=='VALUE_41')|(variable=='VALUE_42')
                                                |(variable=='VALUE_43')])
                      ,PFOR=(sum(Forest_sqMile)/totalArea_sqMile)*100
                      ,Wetland_sqMile=sum(sqMile[(variable=='VALUE_90')|(variable=='VALUE_95')])
                      ,PWETL=(Wetland_sqMile/totalArea_sqMile)*100
                      ,Shrub_sqMile=sum(sqMile[(variable=='VALUE_51')|(variable=='VALUE_52')])
                      ,PSHRB=(Shrub_sqMile/totalArea_sqMile)*100
                      ,Ngrasslands_sqMile=sum(sqMile[(variable=='VALUE_71')|(variable=='VALUE_72')
                                                     |(variable=='VALUE_73')|(variable=='VALUE_74')])
                      ,PNG=(Ngrasslands_sqMile/totalArea_sqMile)*100
                      ,Barren_sqMile=sum(sqMile[(variable=='VALUE_31')])
                      ,PBAR=(Barren_sqMile/totalArea_sqMile)*100
                      ,TotBarren_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_31')])
                      ,PTotBAR=(TotBarren_sqMile/totalArea_sqMile)*100
                      ,U_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_22')
                                           |(variable=='VALUE_23')|(variable=='VALUE_24')
                                           |(variable=='VALUE_81')|(variable=='VALUE_82')])
                      ,U_INDEX=(U_sqMile/totalArea_sqMile)*100
                      ,Urban_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_22')
                                               |(variable=='VALUE_23')|(variable=='VALUE_24')])
                      ,PURB=(Urban_sqMile/totalArea_sqMile)*100
                      ,MBAR_sqMile=sum(sqMile[(variable=='VALUE_21')])
                      ,PMBAR=(MBAR_sqMile/totalArea_sqMile)*100
                      ,AGT_sqMile=sum(sqMile[(variable=='VALUE_81')|(variable=='VALUE_82')])
                      ,PAGT=(AGT_sqMile/totalArea_sqMile)*100
                      ,AGP_sqMile=sum(sqMile[(variable=='VALUE_81')])
                      ,PAGP=(AGP_sqMile/totalArea_sqMile)*100
                      ,AGC_sqMile=sum(sqMile[(variable=='VALUE_82')])
                      ,PAGC=(AGC_sqMile/totalArea_sqMile)*100)
    landuselong <- melt(landuse4,id.vars=c('StationID','totalArea_sqMile')
                        ,measure.vars=c('PWater','N_INDEX','PFOR','PWETL','PSHRB','PNG','PBAR','PTotBAR','U_INDEX'
                                        ,'PURB','PMBAR','PAGT','PAGP','PAGC'))
    landusewide<- dcast(landuselong,StationID+totalArea_sqMile~variable,value.var='value',sum)
    ## Diversity metrics
    landuse <- mutate(landuse, Count=ifelse(value>0,1,0))
    classcount <- cast(landuse,Count~StationID,length)
    classcount <- if(length(classcount$Count)>1){classcount[2,]}else{classcount[1,]}
    classcount <- melt(classcount, val.name=Count)
    cellcount <- aggregate(.~StationID, data=df2,sum)
    count <- merge(cellcount,classcount, by='StationID')
    names(count) <- c('StationID','_','Psum','_','m')
    D1 <- merge(df2,count, by='StationID')
    D2 <- plyr::ddply(D1, c('StationID','variable','value','Psum','m'),summarise, P_i=value/Psum
                ,P_i_ln= P_i*(log(P_i)),P_i2=(value/Psum)^2)
    D3 <- aggregate(cbind(P_i_ln,P_i2)~StationID,data=D2,sum)
    D4 <- merge(D3, count, by='StationID')
    D5 <- ddply(D4,c('StationID','m','P_i_ln','P_i2'),mutate,S=m,H=-(P_i_ln)
                ,Hprime=H/log(S),C=1-sum(P_i2))
    Diversity <- subset(D5,select=c('StationID','S','H','Hprime','C'))%>%
      join(df0.5,by='StationID')
    ## Combine landusewide and diversity metrics, include NLCD info
    Result_ <- join(landusewide,Diversity,by='StationID')%>%select(StationID,YearSampled,NLCD,everything())
    Result <- rbind(Result,Result_)}
  Result <- Result[complete.cases(Result[,1]),]
  return(Result)
}

ripCalc <- function(x,y){
  # Buffer just the stream segments in selected watershed
  buffer <- gBuffer(testnhd, width=y)
  years <- subset(criticalLink,StationID %in% wshdPolys[x,]$StationID)%>%
    mutate(year=ifelse(!is.na(Year_),Year_,2011)# get rid of NA's, replace with 2011 for now bc most recent NLCD release
           ,NLCDyear=ifelse(year>2000&year<2004,2001,ifelse(year>=2004&year<2009,2006,2011)))#relabel based on possible NLCD releases
  if(nrow(years)==0){# make up a fake df for years if there is not matching entry for wshdPolys$StationID in Jason's db
    years <- data.frame(sampleID=wshdPolys[x,]$StationID,strahler_order=NA,Longitude_DD=NA
                        ,Latitude_DD=NA,Year_=NA,StationID=wshdPolys[x,]$StationID,year=2011,NLCDyear=2011)}
  if(length(unique(years$NLCDyear))==1){# this means only one NLCD year needs to be run
    landcover <- if(unique(years$NLCDyear)==2001){landcover2001}else{if(unique(years$NLCDyear)==2006){landcover2006}else{landcover2011}}
    e = extract(landcover, buffer,small=T, na.rm=F)
    et = lapply(e,table)
    if(!length(et[[1]])==0){ 
      t <- melt(et)
      t.cast <- cast(t, L1 ~ Var.1, sum)
      names(t.cast)[1] <- "StationID"
      t.cast[1,1] <- wshdList[x]
      print(paste(wshdList[x],'Buffer Distance =',y,'m',sep=' '))
      colnames(t.cast)[2:length(names(t.cast))] <- paste("VALUE_",colnames(t.cast)
                                                         [2:length(names(t.cast))], sep = "")}
    if(length(et[[1]])==0){ 
      t.cast <- data.frame(wshdPolys[x,]@data[1])
      rownames(t.cast) <- 1
      t.cast$VALUE_11= 0
      print(paste(wshdList[x],'Buffer Distance =',y,'m',sep=' '))}
    zeros = template[is.na(match(names(template), names(t.cast)))]
    results = data.frame(t.cast, zeros)
    results <- results[,order(names(results))]%>%
      mutate(NLCD=unique(years$NLCDyear))
    if(nrow(years)>1){# now deal with if more than 1 sample in that NLCD year
      results <- results[rep(seq_len(nrow(results)), each=nrow(years)),]
      results$YearSampled <- ifelse(is.na(years$Year_),years$Year_,years$year)
      row.names(results) <- 1:nrow(results) #change row.names
      results <- select(results,StationID,YearSampled,NLCD,everything())
    }else(#if only one row(year) just need to add on YearSampled
      results <- mutate(results,YearSampled=ifelse(is.na(years$Year_[1]),years$Year_[1],years$year[1]))%>%#correct for defaulting Year_=2011 when NA)%>%
        select(StationID,YearSampled,NLCD,everything()))
  }else{# this means more than 1 NLCD year needs to be run, must loop through them
    results <- mutate(template,StationID=NA,YearSampled=NA,NLCD=NA)%>%select(StationID,YearSampled,NLCD,everything())
    nNLCDyears <- aggregate(data.frame(count=years[,8]),list(NLCDyrs=years[,8]),length)
    for(b in 1:nrow(nNLCDyears)){
      landcover <- if(nNLCDyears[b,1]==2001){landcover2001}else{if(nNLCDyears[b,1]==2006){landcover2006}else{landcover2011}}
      e = extract(landcover, buffer,small=T, na.rm=F)
      et = lapply(e,table)
      if(!length(et[[1]])==0){ 
        t <- melt(et)
        t.cast <- cast(t, L1 ~ Var.1, sum)
        print(paste(wshdList[x],'Buffer Distance =',y,'m','; NLCD Year=',nNLCDyears[b,1],sep=' '))
        names(t.cast)[1] <- "StationID"
        t.cast[1,1] <- wshdList[x]
        colnames(t.cast)[2:length(names(t.cast))] <- paste("VALUE_",colnames(t.cast)
                                                           [2:length(names(t.cast))], sep = "")}
      if(length(et[[1]])==0){ 
        t.cast <- data.frame(wshdPolys[x,]@data[1])
        rownames(t.cast) <- 1
        t.cast$VALUE_11= 0
        print(paste(wshdList[x],'Buffer Distance =',y,'m','; NLCD Year=',nNLCDyears[b,1],sep=' '))}
      zeros <- template[is.na(match(names(template), names(t.cast)))]
      results_ <- data.frame(t.cast, zeros)
      results_ <- results_[,order(names(results_))]%>%
        mutate(NLCD=nNLCDyears[b,1])
      if(nNLCDyears$count[b]>1){# now deal with if more than 1 sample in that NLCD year
        results_ <- results_[rep(seq_len(nrow(results_)), each=nNLCDyears$count[b]),]
        step1 <- subset(years,NLCDyear %in% nNLCDyears[b,1])
        results_$YearSampled <- ifelse(is.na(step1$Year_),step1$Year_,step1$year)
        row.names(results_) <- 1:nrow(results_) #change row.names
        results_ <- select(results_,StationID,YearSampled,NLCD,everything())
      }else{#if only one row(year) just need to add on YearSampled
        step1 <- subset(years,NLCDyear %in% nNLCDyears[b,1])
        results_ <- mutate(results_,YearSampled=ifelse(is.na(step1$Year_),step1$Year_,step1$year))%>%
          select(StationID,YearSampled,NLCD,everything())}
      results <- rbind(results,results_)
    }
  }
  results <- results[complete.cases(results$StationID),] #remove any placeholder rows
  return(results)
}

riparianDataManagment <- function(x,y){ 
  rip <- ripCalc(x,y)
  rip.wide <- data.frame(StationID=NA,YearSampled=NA,NLCD=NA,RNAT=NA,RFOR=NA,RWETL=NA
                         ,RSHRB=NA,RNG=NA,RBAR=NA,RTotBAR=NA,RHUM=NA,RURB=NA,RMBAR=NA
                         ,RAGT=NA,RAGP=NA,RAGC=NA)  
  for(z in 1:nrow(rip)){
    rip0.5 <- rip[z,1:3] # save year sampled and NLCD year info
    rip0 <- melt(rip[z,c(1,4:18)], 'StationID')
    rip1 <- mutate(rip0, acre=900*value*0.0002471053814672,sqMile=acre*0.0015625
                   ,hectare=sqMile*258.9988110336)
    rip2 <- summarise(rip1,StationID=wshdList[x],totalArea_sqMile=sum(sqMile))
    rip3 <- merge(rip1,rip2, by='StationID')
    rip4 <- ddply(rip3,c('StationID','variable'),mutate
                  ,N_sqMile=sum(sqMile[(variable=='VALUE_31')|(variable=='VALUE_41')
                                       |(variable=='VALUE_42')|(variable=='VALUE_43')
                                       |(variable=='VALUE_51')|(variable=='VALUE_52')
                                       |(variable=='VALUE_71')|(variable=='VALUE_72')
                                       |(variable=='VALUE_73')|(variable=='VALUE_74')
                                       |(variable=='VALUE_91')|(variable=='VALUE_95')])
                  ,RNAT=(N_sqMile/totalArea_sqMile)*100
                  ,Forest_sqMile=sum(sqMile[(variable=='VALUE_41')|(variable=='VALUE_42')
                                            |(variable=='VALUE_43')])
                  ,RFOR=(sum(Forest_sqMile)/totalArea_sqMile)*100
                  ,Wetland_sqMile=sum(sqMile[(variable=='VALUE_90')|(variable=='VALUE_95')])
                  ,RWETL=(Wetland_sqMile/totalArea_sqMile)*100
                  ,Shrub_sqMile=sum(sqMile[(variable=='VALUE_51')|(variable=='VALUE_52')])
                  ,RSHRB=(Shrub_sqMile/totalArea_sqMile)*100
                  ,Ngrasslands_sqMile=sum(sqMile[(variable=='VALUE_71')|(variable=='VALUE_72')
                                                 |(variable=='VALUE_73')|(variable=='VALUE_74')])
                  ,RNG=(Ngrasslands_sqMile/totalArea_sqMile)*100
                  ,Barren_sqMile=sum(sqMile[(variable=='VALUE_31')])
                  ,RBAR=(Barren_sqMile/totalArea_sqMile)*100
                  ,TotBarren_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_31')])
                  ,RTotBAR=(TotBarren_sqMile/totalArea_sqMile)*100
                  ,U_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_22')
                                       |(variable=='VALUE_23')|(variable=='VALUE_24')
                                       |(variable=='VALUE_81')|(variable=='VALUE_82')])
                  ,RHUM=(U_sqMile/totalArea_sqMile)*100
                  ,Urban_sqMile=sum(sqMile[(variable=='VALUE_21')|(variable=='VALUE_22')
                                           |(variable=='VALUE_23')|(variable=='VALUE_24')])
                  ,RURB=(Urban_sqMile/totalArea_sqMile)*100
                  ,MBAR_sqMile=sum(sqMile[(variable=='VALUE_21')])
                  ,RMBAR=(MBAR_sqMile/totalArea_sqMile)*100
                  ,AGT_sqMile=sum(sqMile[(variable=='VALUE_81')|(variable=='VALUE_82')])
                  ,RAGT=(AGT_sqMile/totalArea_sqMile)*100
                  ,AGP_sqMile=sum(sqMile[(variable=='VALUE_81')])
                  ,RAGP=(AGP_sqMile/totalArea_sqMile)*100
                  ,AGC_sqMile=sum(sqMile[(variable=='VALUE_82')])
                  ,RAGC=(AGC_sqMile/totalArea_sqMile)*100)
    rip.long <- melt(rip4,id.vars=c('StationID')
                     ,measure.vars=c('RNAT','RFOR','RWETL','RSHRB','RNG','RBAR','RTotBAR','RHUM'
                                     ,'RURB','RMBAR','RAGT','RAGP','RAGC'))
    rip.wide_ <- dcast(rip.long,StationID~variable,value.var='value',sum)%>%
      merge(rip0.5,by='StationID')%>%select(StationID,YearSampled,NLCD,everything())
    rip.wide <- rbind(rip.wide,rip.wide_)
  }
  rip.wide <- rip.wide[complete.cases(rip.wide$StationID),]
  return(rip.wide)
}

riparianDataManagment2 <- function(x){
  df1 <- riparianDataManagment(x,1)
  colnames(df1)[4:length(names(df1))] <- paste(colnames(df1)[4:length(names(df1))],"1", sep = "")
  df30 <- riparianDataManagment(x,30)
  colnames(df30)[4:length(names(df30))] <- paste(colnames(df30)[4:length(names(df30))],"30", sep = "")
  df120 <- riparianDataManagment(x,120)
  colnames(df120)[4:length(names(df120))] <- paste(colnames(df120)[4:length(names(df120))],"120", sep = "")
  riparianlanduse <- join_all(list(df1,df30,df120), by=c('StationID','YearSampled','NLCD'))
}



permitCount <- function(x){
  s <- vaVPDES[wshdPolys[x,],]
  if(!nrow(s@data)==0){
    vaVPDESselect <- as.data.frame(s)
    vaVPDESselect <- mutate(vaVPDESselect, StationID=wshdPolys@data$StationID[x])}
  if(nrow(s@data)==0){
    vaVPDESselect <- vaVPDEStemplate
    vaVPDESselect$StationID <- wshdPolys@data$StationID[x]}
  vaVPDES1 <- rbind(vaVPDEStemplate,vaVPDESselect)
  vaVPDES1 <- vaVPDES1[complete.cases(vaVPDES1$StationID),]
  return(vaVPDES1)
}

VPDESdataManagement <- function(x){# x is a dataframe
  # Mark permits according to classification
  vaVPDES2 <- mutate(x,MunMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Municipal'),1,0)
                     ,MunMinor=ifelse(c(MAJOR__MIN=='Minor'& MUNICIPAL_=='Municipal'),1,0)
                     ,IndMajor=ifelse(c(MAJOR__MIN=='Major'&MUNICIPAL_=='Industrial'),1,0)
                     ,IndMinor=ifelse(c(MAJOR__MIN=='Minor'&MUNICIPAL_=='Industrial'),1,0))
  vaVPDES2[is.na(vaVPDES2)] <- 0 # make 0 to enable subsequent sum function
  # Sum all permit types based on StationID
  vaVPDES3 <- ddply(vaVPDES2,'StationID',summarise,MunMajor=sum(MunMajor)
                    ,MunMinor=sum(MunMinor),IndMajor=sum(IndMajor),IndMinor=sum(IndMinor))}



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