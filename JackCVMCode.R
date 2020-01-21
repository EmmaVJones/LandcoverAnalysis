###[64-bit] C:\Users\ktq89598\Documents\R\R-3.3.3
# Loading packages
library(plyr)
library(dplyr)
#library(spsurvey)
library(ggplot2)
library(CDFt)
library(cramer)
library(reshape2)


###Info on p-value creation in Cramer Von Mises
#http://hameddaily.blogspot.com/2015/01/goodness-of-fit-test-in-r.html


###Changing major gears, look at land cover data from Jack the Intern
###Bring in landcover metrics to test
jack <- read.csv('data/CVMdata.csv')

#########################################################################################
###For Area
jackArea <- jack %>%
  select(StationID,BioRegion,Order,totalArea_sqMile_JACK) #select data needed for filter
         
jasonArea <- jack %>%
           select(StationID,BioRegion,Order,totalArea_sqMile) #select data needed for filter

#area <- cbind(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)

#plot data in histogram
plot1 <- hist(jackArea$totalArea_sqMile_JACK,breaks=50, include.lowest=FALSE, right=FALSE)
plot2 <- hist(jasonArea$totalArea_sqMile,breaks=50, include.lowest=FALSE, right=FALSE)

# Jack vs Jason Data Statewide
CVM_Area <- CramerVonMisesTwoSamples(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
CVM_Area
CVM_P_Area <- 1/6*exp(-CVM_Area)  ###Convert U from cramer von mises to p-value
CVM_P_Area <- melt(CVM_P_Area)
CVM_P_Area$ID <- seq.int(nrow(CVM_P_Area))

#anovaArea <- aov(jackArea$totalArea_sqMile_JACK ~ jasonArea$totalArea_sqMile)
#summary(anovaArea)

x <- jackArea$totalArea_sqMile_JACK
y <- jasonArea$totalArea_sqMile
t.test(x,y)

#your.aov = aov(growth ~ sugar)
#summary(your.aov)


### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackAreaCoast <- jackArea[jackArea$BioRegion=='Coast',]; #select data needed by bioregion

jasonAreaCoast <- jasonArea[jasonArea$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_AreaCoast <- CramerVonMisesTwoSamples(jackAreaCoast$totalArea_sqMile_JACK,jasonAreaCoast$totalArea_sqMile)
CVM_AreaCoast
CVM_P_AreaCoast <- 1/6*exp(-CVM_AreaCoast)  ###Convert U from cramer von mises to p-value
CVM_P_AreaCoast <- melt(CVM_P_AreaCoast)
CVM_P_AreaCoast$ID <- seq.int(nrow(CVM_P_AreaCoast))


###
jackAreaPiedmont <- jackArea[jackArea$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonAreaPiedmont <- jasonArea[jasonArea$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_AreaPiedmont <- CramerVonMisesTwoSamples(jackAreaPiedmont$totalArea_sqMile_JACK,jasonAreaPiedmont$totalArea_sqMile)
CVM_AreaPiedmont
CVM_P_AreaPiedmont<- 1/6*exp(-CVM_AreaPiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_AreaPiedmont <- melt(CVM_P_AreaPiedmont)
CVM_P_AreaPiedmont$ID <- seq.int(nrow(CVM_P_AreaPiedmont))

###
jackAreaMountain <- jackArea[jackArea$BioRegion=='Mountain',]; #select data needed by bioregion

jasonAreaMountain <- jasonArea[jasonArea$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_AreaMountain <- CramerVonMisesTwoSamples(jackAreaMountain$totalArea_sqMile_JACK,jasonAreaMountain$totalArea_sqMile)
CVM_AreaMountain
CVM_P_AreaMountain <- 1/6*exp(-CVM_AreaMountain)  ###Convert U from cramer von mises to p-value
CVM_P_AreaMountain <- melt(CVM_P_AreaMountain)
CVM_P_AreaMountain$ID <- seq.int(nrow(CVM_P_AreaMountain))


###Merge the P-results
test <- merge(CVM_P_Area, CVM_P_AreaCoast, by=c("ID"),all=TRUE)
test2 <- merge(test,CVM_P_AreaPiedmont, by=c("ID"), all=TRUE)
Final_P_Area <- merge(test2, CVM_P_AreaMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_Area, 'Final_P_Area.csv', row.names=FALSE) 


##########################################################################################
###For % Watershed Forest
jackForest <- jack %>%
  select(StationID,BioRegion,Order,PFOR_JACK) #select data needed for filter

jasonForest <- jack %>%
  select(StationID,BioRegion,Order,PFOR) #select data needed for filter


#t-test
x <- jackForest$PFOR_JACK
y <- jasonForest$PFOR
t.test(x,y)

#ANOVA
data <- cbind (jackForest$PFOR_JACK,jasonForest$PFOR)
write.csv(data, 'data.csv', row.names=FALSE) 

mydata <- read.csv('data.csv')
aov( values ~ ind, mydata)
summary(aov( values ~ ind, mydata))





# Jack vs Jason Data Statewide
CVM_Forest <- CramerVonMisesTwoSamples(jackForest$PFOR_JACK,jasonForest$PFOR)
CVM_Forest
CVM_P_For<- 1/6*exp(-CVM_Forest)  ###Convert U from cramer von mises to p-value
CVM_P_For <- melt(CVM_P_For)
CVM_P_For$ID <- seq.int(nrow(CVM_P_For))

### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackForestCoast <- jackForest[jackForest$BioRegion=='Coast',]; #select data needed by bioregion

jasonForestCoast <- jasonForest[jasonForest$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ForestCoast <- CramerVonMisesTwoSamples(jackForestCoast$PFOR_JACK,jasonForestCoast$PFOR)
CVM_ForestCoast
CVM_P_ForCoast <- 1/6*exp(-CVM_ForestCoast)  ###Convert U from cramer von mises to p-value
CVM_P_ForCoast <- melt(CVM_P_ForCoast)
CVM_P_ForCoast$ID <- seq.int(nrow(CVM_P_ForCoast))


###
jackForestPiedmont <- jackForest[jackForest$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonForestPiedmont <- jasonForest[jasonForest$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ForestPiedmont <- CramerVonMisesTwoSamples(jackForestPiedmont$PFOR_JACK,jasonForestPiedmont$PFOR)
CVM_ForestPiedmont
CVM_P_ForPiedmont <- 1/6*exp(-CVM_ForestPiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_ForPiedmont <- melt(CVM_P_ForPiedmont)
CVM_P_ForPiedmont$ID <- seq.int(nrow(CVM_P_ForPiedmont))

###
jackForestMountain <- jackForest[jackForest$BioRegion=='Mountain',]; #select data needed by bioregion

jasonForestMountain <- jasonForest[jasonForest$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ForestMountain <- CramerVonMisesTwoSamples(jackForestMountain$PFOR_JACK,jasonForestMountain$PFOR)
CVM_ForestMountain
CVM_P_ForMountain <- 1/6*exp(-CVM_ForestMountain)  ###Convert U from cramer von mises to p-value
CVM_P_ForMountain <- melt(CVM_P_ForMountain)
CVM_P_ForMountain$ID <- seq.int(nrow(CVM_P_ForMountain))

###Merge the P-results
test <- merge(CVM_P_For, CVM_P_ForCoast, by=c("ID"),all=TRUE)
test2 <- merge(test,CVM_P_ForPiedmont, by=c("ID"), all=TRUE)
Final_P_For <- merge(test2, CVM_P_ForMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_For, 'Final_P_For.csv', row.names=FALSE) 




##########################################################################################
###For % Watershed Wetland
jackWetl <- jack %>%
  select(StationID,BioRegion,Order,PWETL_JACK) #select data needed for filter

jasonWetl <- jack %>%
  select(StationID,BioRegion,Order,PWETL) #select data needed for filter


#x = rnorm(10)
#y = rnorm(10)
#t.test(x,y)

x <- jackWetl$PWETL_JACK
y <- jasonWetl$PWETL
t.test(x,y)

a <- jackWetlCoast$PWETL_JACK
b <- jasonWetlCoast$PWETL
t.test(a,b)

# Jack vs Jason Data Statewide
CVM_Wetl <- CramerVonMisesTwoSamples(jackWetl$PWETL_JACK,jasonWetl$PWETL)
CVM_Wetl
CVM_P_Wetl<- 1/6*exp(-CVM_Wetl)  ###Convert U from cramer von mises to p-value
CVM_P_Wetl <- melt(CVM_P_Wetl)
CVM_P_Wetl$ID <- seq.int(nrow(CVM_P_Wetl))

### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackWetlCoast <- jackWetl[jackWetl$BioRegion=='Coast',]; #select data needed by bioregion

jasonWetlCoast <- jasonWetl[jasonWetl$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_WetlCoast <- CramerVonMisesTwoSamples(jackWetlCoast$PWETL_JACK,jasonWetlCoast$PWETL)
CVM_WetlCoast
CVM_P_WetlCoast <- 1/6*exp(-CVM_WetlCoast)  ###Convert U from cramer von mises to p-value
CVM_P_WetlCoast <- melt(CVM_P_WetlCoast)
CVM_P_WetlCoast$ID <- seq.int(nrow(CVM_P_WetlCoast))


###
jackWetlPiedmont <- jackWetl[jackWetl$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonWetlPiedmont <- jasonWetl[jasonWetl$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_WetlPiedmont <- CramerVonMisesTwoSamples(jackWetlPiedmont$PWETL_JACK,jasonWetlPiedmont$PWETL)
CVM_WetlPiedmont
CVM_P_WetlPiedmont <- 1/6*exp(-CVM_WetlPiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_WetlPiedmont <- melt(CVM_P_WetlPiedmont)
CVM_P_WetlPiedmont$ID <- seq.int(nrow(CVM_P_WetlPiedmont))

###
jackWetlMountain <- jackWetl[jackWetl$BioRegion=='Mountain',]; #select data needed by bioregion

jasonWetlMountain <- jasonWetl[jasonWetl$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_WetlMountain <- CramerVonMisesTwoSamples(jackWetlMountain$PWETL_JACK,jasonWetlMountain$PWETL)
CVM_WetlMountain
CVM_P_WetlMountain <- 1/6*exp(-CVM_WetlMountain)  ###Convert U from cramer von mises to p-value
CVM_P_WetlMountain <- melt(CVM_P_WetlMountain)
CVM_P_WetlMountain$ID <- seq.int(nrow(CVM_P_WetlMountain))

###Merge the P-results
test <- merge(CVM_P_Wetl, CVM_P_WetlCoast, by=c("ID"),all=TRUE)
test2 <- merge(test,CVM_P_WetlPiedmont, by=c("ID"), all=TRUE)
Final_P_Wetl <- merge(test2, CVM_P_WetlMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_Wetl, 'Final_P_Wetl.csv', row.names=FALSE) 




##########################################################################################
###For % Riparain Forest by 30 meters
jackFor30 <- jack %>%
  select(StationID,BioRegion,Order,RFOR30_JACK) #select data needed for filter

jasonFor30 <- jack %>%
  select(StationID,BioRegion,Order,RFOR30) #select data needed for filter



x <- jackFor30$RFOR30_JACK
y <- jasonFor30$RFOR30
t.test(x,y)

# Jack vs Jason Data Statewide
CVM_Rfor30 <- CramerVonMisesTwoSamples(jackFor30$RFOR30_JACK,jasonFor30$RFOR30)
CVM_Rfor30
CVM_P_Rfor30<- 1/6*exp(-CVM_Rfor30)  ###Convert U from cramer von mises to p-value
CVM_P_Rfor30 <- melt(CVM_P_Rfor30)
CVM_P_Rfor30$ID <- seq.int(nrow(CVM_P_Rfor30))

### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackRfor30Coast <- jackFor30[jackFor30$BioRegion=='Coast',]; #select data needed by bioregion

jasonRfor30Coast <- jasonFor30[jasonFor30$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_Rfor30Coast <- CramerVonMisesTwoSamples(jackRfor30Coast$RFOR30_JACK,jasonRfor30Coast$RFOR30)
CVM_Rfor30Coast
CVM_P_Rfor30Coast <- 1/6*exp(-CVM_Rfor30Coast)  ###Convert U from cramer von mises to p-value
CVM_P_Rfor30Coast <- melt(CVM_P_Rfor30Coast)
CVM_P_Rfor30Coast$ID <- seq.int(nrow(CVM_P_Rfor30Coast))


###
jackRfor30Piedmont <- jackFor30[jackFor30$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonRfor30Piedmont <- jasonFor30[jasonFor30$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_Rfor30Piedmont <- CramerVonMisesTwoSamples(jackRfor30Piedmont$RFOR30_JACK,jasonRfor30Piedmont$RFOR30)
CVM_Rfor30Piedmont
CVM_P_Rfor30Piedmont <- 1/6*exp(-CVM_Rfor30Piedmont)  ###Convert U from cramer von mises to p-value
CVM_P_Rfor30Piedmont <- melt(CVM_P_Rfor30Piedmont)
CVM_P_Rfor30Piedmont$ID <- seq.int(nrow(CVM_P_Rfor30Piedmont))

###
jackRfor30Mountain <- jackFor30[jackFor30$BioRegion=='Mountain',]; #select data needed by bioregion

jasonRfor30Mountain <- jasonFor30[jasonFor30$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_Rfor30Mountain <- CramerVonMisesTwoSamples(jackRfor30Mountain$RFOR30_JACK,jasonRfor30Mountain$RFOR30)
CVM_Rfor30Mountain
CVM_P_Rfor30Mountain <- 1/6*exp(-CVM_Rfor30Mountain)  ###Convert U from cramer von mises to p-value
CVM_P_Rfor30Mountain <- melt(CVM_P_Rfor30Mountain)
CVM_P_Rfor30Mountain$ID <- seq.int(nrow(CVM_P_Rfor30Mountain))

###Merge the P-results
test <- merge(CVM_P_Rfor30, CVM_P_Rfor30Coast, by=c("ID"),all=TRUE)
test2 <- merge(test, CVM_P_Rfor30Piedmont, by=c("ID"), all=TRUE)
Final_P_Rfor30 <- merge(test2, CVM_P_Rfor30Mountain, by=c("ID"),all=TRUE)

write.csv(Final_P_Rfor30, 'Final_P_Rfor30.csv', row.names=FALSE) 




##########################################################################################
###Mun Minor Count by Watershed
jackMinor <- jack %>%
  select(StationID,BioRegion,Order,MunMinor_JACK) #select data needed for filter

jasonMinor <- jack %>%
  select(StationID,BioRegion,Order,MunMinor) #select data needed for filter

jackMinor2 <- jackMinor[jackMinor$MunMinor>0,]; # filter out zeros

jasonMinor2  <- jasonMinor[jasonMinor$MunMinor>0,]; # filter out zeros

MINOR <- cbind(jackMinor,jasonMinor)



x <- jackMinor$MunMinor_JACK
y <- jasonMinor$MunMinor
t.test(x,y)

# Jack vs Jason Data Statewide
CVM_Minor <- CramerVonMisesTwoSamples(jackMinor2$MunMinor_JACK,jasonMinor2$MunMinor)
CVM_Minor
CVM_Minor<- 1/6*exp(-CVM_Minor)  ###Convert U from cramer von mises to p-value
CVM_P_Minor <- melt(CVM_Minor)
CVM_P_Minor$ID <- seq.int(nrow(CVM_P_Minor)) ###seems fooled due to zeros

### Use only CramerVonMises for this Paper
###now K-S test
KS_Minor <- ks.test(jackMinor2$MunMinor_JACK,jasonMinor2$MunMinor)
KS_Area ####Will not calculate due to zeros

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
Cramer_Area <- cramer.test(jackMinor$MunMinor_JACK,jasonMinor$MunMinor,conf.level=0.95)
Cramer_Area 

###
jackMinorCoast <- jackMinor[jackMinor$BioRegion=='Coast',]; #select data needed by bioregion

jasonMinorCoast <- jasonMinor[jasonMinor$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_MinorCoast <- CramerVonMisesTwoSamples(jackMinorCoast$MunMinor_JACK,jasonMinorCoast$MunMinor)
CVM_MinorCoast
CVM_P_MinorCoast <- 1/6*exp(-CVM_MinorCoast)  ###Convert U from cramer von mises to p-value
CVM_P_MinorCoast <- melt(CVM_P_MinorCoast)
CVM_P_MinorCoast$ID <- seq.int(nrow(CVM_P_MinorCoast))


###
jackMinorPiedmont <- jackMinor[jackMinor$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonMinorPiedmont <- jasonMinor[jasonMinor$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_MinorPiedmont <- CramerVonMisesTwoSamples(jackMinorPiedmont$MunMinor_JACK,jasonMinorPiedmont$MunMinor)
CVM_MinorPiedmont
CVM_P_MinorPiedmont <- 1/6*exp(-CVM_MinorPiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_MinorPiedmont <- melt(CVM_P_MinorPiedmont)
CVM_P_MinorPiedmont$ID <- seq.int(nrow(CVM_P_MinorPiedmont))

###
jackMinorMountain <- jackMinor[jackMinor$BioRegion=='Mountain',]; #select data needed by bioregion

jasonMinorMountain <- jasonMinor[jasonMinor$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_MinorMountain <- CramerVonMisesTwoSamples(jackMinorMountain$MunMinor_JACK,jasonMinorMountain$MunMinor)
CVM_MinorMountain
CVM_P_MinorMountain <- 1/6*exp(-CVM_MinorMountain)  ###Convert U from cramer von mises to p-value
CVM_P_MinorMountain <- melt(CVM_P_MinorMountain)
CVM_P_MinorMountain$ID <- seq.int(nrow(CVM_P_MinorMountain))

###Merge the P-results
test <- merge(CVM_P_Minor, CVM_P_MinorCoast, by=c("ID"),all=TRUE)
test2 <- merge(test, CVM_P_MinorPiedmont, by=c("ID"), all=TRUE)
Final_P_Minor <- merge(test2, CVM_P_MinorMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_Minor, 'Final_P_Minor.csv', row.names=FALSE) 



##########################################################################################
###Site Rain by Watershed
jackSite <- jack %>%
  select(StationID,BioRegion,Order,siteRain_mmyr_JACK) #select data needed for filter

jasonSite <- jack %>%
  select(StationID,BioRegion,Order,siteRain_mmyr) #select data needed for filter

x <- jackSite$siteRain_mmyr_JACK
y <- jasonSite$siteRain_mmyr
t.test(x,y)


# Jack vs Jason Data Statewide
CVM_Site <- CramerVonMisesTwoSamples(jackSite$siteRain_mmyr_JACK,jasonSite$siteRain_mmyr)
CVM_Site
CVM_Site<- 1/6*exp(-CVM_Site)  ###Convert U from cramer von mises to p-value
CVM_P_Site <- melt(CVM_Site)
CVM_P_Site$ID <- seq.int(nrow(CVM_P_Site))

### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackSiteCoast <- jackSite[jackSite$BioRegion=='Coast',]; #select data needed by bioregion

jasonSiteCoast <- jasonSite[jasonSite$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_SiteCoast <- CramerVonMisesTwoSamples(jackSiteCoast$siteRain_mmyr_JACK,jasonSiteCoast$siteRain_mmyr)
CVM_SiteCoast
CVM_P_SiteCoast <- 1/6*exp(-CVM_SiteCoast)  ###Convert U from cramer von mises to p-value
CVM_P_SiteCoast <- melt(CVM_P_SiteCoast)
CVM_P_SiteCoast$ID <- seq.int(nrow(CVM_P_SiteCoast))


###
jackSitePiedmont <- jackSite[jackSite$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonSitePiedmont <- jasonSite[jasonSite$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_SitePiedmont <- CramerVonMisesTwoSamples(jackSitePiedmont$siteRain_mmyr_JACK,jasonSitePiedmont$siteRain_mmyr)
CVM_SitePiedmont
CVM_P_SitePiedmont <- 1/6*exp(-CVM_SitePiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_SitePiedmont <- melt(CVM_P_SitePiedmont)
CVM_P_SitePiedmont$ID <- seq.int(nrow(CVM_P_SitePiedmont))

###
jackSiteMountain <- jackSite[jackSite$BioRegion=='Mountain',]; #select data needed by bioregion

jasonSiteMountain <- jasonSite[jasonSite$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_SiteMountain <- CramerVonMisesTwoSamples(jackSiteMountain$siteRain_mmyr_JACK,jasonSiteMountain$siteRain_mmyr)
CVM_SiteMountain
CVM_P_SiteMountain <- 1/6*exp(-CVM_SiteMountain)  ###Convert U from cramer von mises to p-value
CVM_P_SiteMountain <- melt(CVM_P_SiteMountain)
CVM_P_SiteMountain$ID <- seq.int(nrow(CVM_P_SiteMountain))

###Merge the P-results
test <- merge(CVM_P_Site, CVM_P_SiteCoast, by=c("ID"),all=TRUE)
test2 <- merge(test, CVM_P_SitePiedmont, by=c("ID"), all=TRUE)
Final_P_Site <- merge(test2, CVM_P_SiteMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_Site, 'Final_P_Site.csv', row.names=FALSE) 






##########################################################################################
###Elevation Mean by Watershed
jackELEV <- jack %>%
  select(StationID,BioRegion,Order,ELEVMEAN_JACK) #select data needed for filter

jasonELEV <- jack %>%
  select(StationID,BioRegion,Order,ELEVMEAN) #select data needed for filter


x <- jackELEV$ELEVMEAN_JACK
y <- jasonELEV$ELEVMEAN
t.test(x,y)

Elie <- cbind (jackELEV,jasonELEV)

# Jack vs Jason Data Statewide
CVM_ELEV <- CramerVonMisesTwoSamples(jackELEV$ELEVMEAN_JACK,jasonELEV$ELEVMEAN)
CVM_ELEV
CVM_ELEV<- 1/6*exp(-CVM_ELEV)  ###Convert U from cramer von mises to p-value
CVM_P_ELEV <- melt(CVM_ELEV)
CVM_P_ELEV$ID <- seq.int(nrow(CVM_P_ELEV))

### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###
jackELEVCoast <- jackELEV[jackELEV$BioRegion=='Coast',]; #select data needed by bioregion

jasonELEVCoast <- jasonELEV[jasonELEV$BioRegion=='Coast',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ELEVCoast <- CramerVonMisesTwoSamples(jackELEVCoast$ELEVMEAN_JACK,jasonELEVCoast$ELEVMEAN)
CVM_ELEVCoast
CVM_P_ELEVCoast <- 1/6*exp(-CVM_ELEVCoast)  ###Convert U from cramer von mises to p-value
CVM_P_ELEVCoast <- melt(CVM_P_ELEVCoast)
CVM_P_ELEVCoast$ID <- seq.int(nrow(CVM_P_ELEVCoast))


###
jackELEVPiedmont <- jackELEV[jackELEV$BioRegion=='Piedmont',]; #select data needed by bioregion

jasonELEVPiedmont <- jasonELEV[jasonELEV$BioRegion=='Piedmont',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ELEVPiedmont <- CramerVonMisesTwoSamples(jackELEVPiedmont$ELEVMEAN_JACK,jasonELEVPiedmont$ELEVMEAN)
CVM_ELEVPiedmont
CVM_P_ELEVPiedmont <- 1/6*exp(-CVM_ELEVPiedmont)  ###Convert U from cramer von mises to p-value
CVM_P_ELEVPiedmont <- melt(CVM_P_ELEVPiedmont)
CVM_P_ELEVPiedmont$ID <- seq.int(nrow(CVM_P_ELEVPiedmont))

###
jackELEVMountain <- jackELEV[jackELEV$BioRegion=='Mountain',]; #select data needed by bioregion

jasonELEVMountain <- jasonELEV[jasonELEV$BioRegion=='Mountain',]; #select data needed by bioregion

###
# Jack vs Jason Data Statewide
CVM_ELEVMountain <- CramerVonMisesTwoSamples(jackELEVMountain$ELEVMEAN_JACK,jasonELEVMountain$ELEVMEAN)
CVM_ELEVMountain
CVM_P_ELEVMountain <- 1/6*exp(-CVM_ELEVMountain)  ###Convert U from cramer von mises to p-value
CVM_P_ELEVMountain <- melt(CVM_P_ELEVMountain)
CVM_P_ELEVMountain$ID <- seq.int(nrow(CVM_P_ELEVMountain))

###Merge the P-results
test <- merge(CVM_P_ELEV, CVM_P_ELEVCoast, by=c("ID"),all=TRUE)
test2 <- merge(test, CVM_P_ELEVPiedmont, by=c("ID"), all=TRUE)
Final_P_ELEV <- merge(test2, CVM_P_ELEVMountain, by=c("ID"),all=TRUE)

write.csv(Final_P_ELEV, 'Final_P_ELEV.csv', row.names=FALSE) 


#########################################################################################
### Area By Order
jackArea <- jack %>%
  select(StationID,BioRegion,Order,totalArea_sqMile_JACK) #select data needed for filter

jasonArea <- jack %>%
  select(StationID,BioRegion,Order,totalArea_sqMile) #select data needed for filter



# Jack vs Jason Data Statewide
#CVM_Area <- CramerVonMisesTwoSamples(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#CVM_Area
#CVM_P_Area <- 1/6*exp(-CVM_Area)  ###Convert U from cramer von mises to p-value
#CVM_P_Area <- melt(CVM_P_Area)
#CVM_P_Area$ID <- seq.int(nrow(CVM_P_Area))



### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###Get By Order
jackOneArea <- jackArea[jackArea$Order=='1',]; #select data needed by bioregion

jasonOneArea <- jasonArea[jasonArea$Order=='1',]; #select data needed by bioregion

OneArea <- cbind(jackOneArea, jasonOneArea)  #C-bind to look at the data, should work!!!

###
# Jack vs Jason 1st Order 
CVM_OneArea <- CramerVonMisesTwoSamples(jackOneArea$totalArea_sqMile_JACK,jasonOneArea$totalArea_sqMile)
CVM_OneArea
CVM_P_OneArea <- 1/6*exp(-CVM_OneArea)  ###Convert U from cramer von mises to p-value
CVM_P_OneArea <- melt(CVM_P_OneArea)
CVM_P_OneArea$ID <- seq.int(nrow(CVM_P_OneArea))

#k-s test
KS_OneArea <- ks.test(jackOneArea$totalArea_sqMile_JACK,jasonOneArea$totalArea_sqMile)
KS_OneArea

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
Cramer_Area <- cramer.test(jackOneArea$totalArea_sqMile_JACK,jasonOneArea$totalArea_sqMile,conf.level=0.95)
Cramer_Area 


###Get By Order
jackTwoArea <- jackArea[jackArea$Order=='2',]; #select data needed by bioregion

jasonTwoArea <- jasonArea[jasonArea$Order=='2',]; #select data needed by bioregion

###
# Jack vs Jason 2nd Order 
CVM_TwoArea <- CramerVonMisesTwoSamples(jackTwoArea$totalArea_sqMile_JACK,jasonTwoArea$totalArea_sqMile)
CVM_TwoArea
CVM_P_TwoArea <- 1/6*exp(-CVM_TwoArea)  ###Convert U from cramer von mises to p-value
CVM_P_TwoArea <- melt(CVM_P_TwoArea)
CVM_P_TwoArea$ID <- seq.int(nrow(CVM_P_TwoArea))

###Get By Order
jackThreeArea <- jackArea[jackArea$Order=='3',]; #select data needed by bioregion

jasonThreeArea <- jasonArea[jasonArea$Order=='3',]; #select data needed by bioregion

###
# Jack vs Jason 3rd Order 
CVM_ThreeArea <- CramerVonMisesTwoSamples(jackThreeArea$totalArea_sqMile_JACK,jasonThreeArea$totalArea_sqMile)
CVM_ThreeArea
CVM_P_ThreeArea <- 1/6*exp(-CVM_ThreeArea)  ###Convert U from cramer von mises to p-value
CVM_P_ThreeArea <- melt(CVM_P_ThreeArea)
CVM_P_ThreeArea$ID <- seq.int(nrow(CVM_P_ThreeArea))



###Get By Order
jackFourArea <- jackArea[jackArea$Order=='4',]; #select data needed by bioregion

jasonFourArea <- jasonArea[jasonArea$Order=='4',]; #select data needed by bioregion

###
# Jack vs Jason 4th Order 
CVM_FourArea <- CramerVonMisesTwoSamples(jackFourArea$totalArea_sqMile_JACK,jasonFourArea$totalArea_sqMile)
CVM_FourArea
CVM_P_FourArea <- 1/6*exp(-CVM_FourArea)  ###Convert U from cramer von mises to p-value
CVM_P_FourArea <- melt(CVM_P_FourArea)
CVM_P_FourArea$ID <- seq.int(nrow(CVM_P_FourArea))



###Get By Order
jackFiveArea <- jackArea[jackArea$Order=='5',]; #select data needed by bioregion

jasonFiveArea <- jasonArea[jasonArea$Order=='5',]; #select data needed by bioregion

###
# Jack vs Jason 5th Order 
CVM_FiveArea <- CramerVonMisesTwoSamples(jackFiveArea$totalArea_sqMile_JACK,jasonFiveArea$totalArea_sqMile)
CVM_FiveArea
CVM_P_FiveArea <- 1/6*exp(-CVM_FiveArea)  ###Convert U from cramer von mises to p-value
CVM_P_FiveArea <- melt(CVM_P_FiveArea)
CVM_P_FiveArea$ID <- seq.int(nrow(CVM_P_FiveArea))

###now K-S test
KS_FiveArea <- ks.test(jackFiveArea$totalArea_sqMile_JACK,jasonFiveArea$totalArea_sqMile)
KS_FiveArea 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
Cramer_Area <- cramer.test(jackFiveArea$totalArea_sqMile_JACK,jasonFiveArea$totalArea_sqMile,conf.level=0.95)
Cramer_Area 


###Merge the P-results
test <- merge(CVM_P_OneArea, CVM_P_TwoArea, by=c("ID"),all=TRUE)
test2 <- merge(test,CVM_P_ThreeArea, by=c("ID"), all=TRUE)
test3 <- merge(test2,CVM_P_FourArea, by=c("ID"), all=TRUE)
Final_P_OrderArea <- merge(test3, CVM_P_FiveArea, by=c("ID"),all=TRUE)

write.csv(Final_P_OrderArea, 'Final_P_OrderArea.csv', row.names=FALSE) 


###Another way to merge
#OrderAreaTest <- cbind(CVM_P_OneArea, CVM_P_TwoArea, CVM_P_ThreeArea, CVM_P_FourArea, CVM_P_FiveArea)  




#########################################################################################
### Percent Forest By Order
jackForestOrder <- jack %>%
  select(StationID,BioRegion,Order,PFOR_JACK) #select data needed for filter

jasonForestOrder <- jack %>%
  select(StationID,BioRegion,Order,PFOR) #select data needed for filter



# Jack vs Jason Data Statewide
#CVM_Area <- CramerVonMisesTwoSamples(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#CVM_Area
#CVM_P_Area <- 1/6*exp(-CVM_Area)  ###Convert U from cramer von mises to p-value
#CVM_P_Area <- melt(CVM_P_Area)
#CVM_P_Area$ID <- seq.int(nrow(CVM_P_Area))



### Use only CramerVonMises for this Paper
###now K-S test
#KS_Area <- ks.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile)
#KS_Area 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackArea$totalArea_sqMile_JACK,jasonArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 

###Get By Order
jackOneFor <- jackForestOrder[jackForestOrder$Order=='1',]; #select data needed by bioregion

jasonOneFor <- jasonForestOrder[jasonForestOrder$Order=='1',]; #select data needed by bioregion

#OneArea <- cbind(jackOneArea, jasonOneArea)  #C-bind to look at the data, should work!!!

###
# Jack vs Jason 1st Order 
CVM_OneFor <- CramerVonMisesTwoSamples(jackOneFor$PFOR_JACK,jasonOneFor$PFOR)
CVM_OneFor
CVM_P_OneFor <- 1/6*exp(-CVM_OneFor)  ###Convert U from cramer von mises to p-value
CVM_P_OneFor <- melt(CVM_P_OneFor)
CVM_P_OneFor$ID <- seq.int(nrow(CVM_P_OneFor))

#k-s test
#KS_OneArea <- ks.test(jackOneArea$totalArea_sqMile_JACK,jasonOneArea$totalArea_sqMile)
#KS_OneArea

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackOneArea$totalArea_sqMile_JACK,jasonOneArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 


###Get By Order
jackTwoFor <- jackForestOrder[jackForestOrder$Order=='2',]; #select data needed by bioregion

jasonTwoFor <- jasonForestOrder[jasonForestOrder$Order=='2',]; #select data needed by bioregion

###
# Jack vs Jason 2nd Order 
CVM_TwoFor <- CramerVonMisesTwoSamples(jackTwoFor$PFOR_JACK,jasonTwoFor$PFOR)
CVM_TwoFor
CVM_P_TwoFor <- 1/6*exp(-CVM_TwoFor)  ###Convert U from cramer von mises to p-value
CVM_P_TwoFor <- melt(CVM_P_TwoFor)
CVM_P_TwoFor$ID <- seq.int(nrow(CVM_P_TwoFor))

###Get By Order
jackThreeFor <- jackForestOrder[jackForestOrder$Order=='3',]; #select data needed by bioregion

jasonThreeFor <- jasonForestOrder[jasonForestOrder$Order=='3',]; #select data needed by bioregion

###
# Jack vs Jason 3rd Order 
CVM_ThreeFor <- CramerVonMisesTwoSamples(jackThreeFor$PFOR_JACK,jasonThreeFor$PFOR)
CVM_ThreeFor
CVM_P_ThreeFor <- 1/6*exp(-CVM_ThreeFor)  ###Convert U from cramer von mises to p-value
CVM_P_ThreeFor <- melt(CVM_P_ThreeFor)
CVM_P_ThreeFor$ID <- seq.int(nrow(CVM_P_ThreeFor))



###Get By Order
jackFourFor <- jackForestOrder[jackForestOrder$Order=='4',]; #select data needed by bioregion

jasonFourFor <- jasonForestOrder[jasonForestOrder$Order=='4',]; #select data needed by bioregion

###
# Jack vs Jason 4th Order 
CVM_FourFor <- CramerVonMisesTwoSamples(jackFourFor$PFOR_JACK,jasonFourFor$PFOR)
CVM_FourFor
CVM_P_FourFor <- 1/6*exp(-CVM_FourFor)  ###Convert U from cramer von mises to p-value
CVM_P_FourFor <- melt(CVM_P_FourFor)
CVM_P_FourFor$ID <- seq.int(nrow(CVM_P_FourFor))



###Get By Order
jackFiveFor <- jackForestOrder[jackForestOrder$Order=='5',]; #select data needed by bioregion

jasonFiveFor <- jasonForestOrder[jasonForestOrder$Order=='5',]; #select data needed by bioregion

###
# Jack vs Jason 5th Order 
CVM_FiveFor <- CramerVonMisesTwoSamples(jackFiveFor$PFOR_JACK,jasonFiveFor$PFOR)
CVM_FiveFor
CVM_P_FiveFor <- 1/6*exp(-CVM_FiveFor)  ###Convert U from cramer von mises to p-value
CVM_P_FiveFor <- melt(CVM_P_FiveFor)
CVM_P_FiveFor$ID <- seq.int(nrow(CVM_P_FiveFor))

###now K-S test
#KS_FiveArea <- ks.test(jackFiveArea$totalArea_sqMile_JACK,jasonFiveArea$totalArea_sqMile)
#KS_FiveArea 

# Use cramer test
# Baringhaus, L. and Franz, C. (2004) On a new multivariate two-sample test
#Cramer_Area <- cramer.test(jackFiveArea$totalArea_sqMile_JACK,jasonFiveArea$totalArea_sqMile,conf.level=0.95)
#Cramer_Area 


###Merge the P-results
test <- merge(CVM_P_OneFor, CVM_P_TwoFor, by=c("ID"),all=TRUE)
test2 <- merge(test,CVM_P_ThreeFor, by=c("ID"), all=TRUE)
test3 <- merge(test2,CVM_P_FourFor, by=c("ID"), all=TRUE)
Final_P_OrderFor <- merge(test3, CVM_P_FiveFor, by=c("ID"),all=TRUE)

write.csv(Final_P_OrderFor, 'Final_P_OrderFor.csv', row.names=FALSE) 


###Another way to merge
#OrderAreaTest <- cbind(CVM_P_OneArea, CVM_P_TwoArea, CVM_P_ThreeArea, CVM_P_FourArea, CVM_P_FiveArea)  



