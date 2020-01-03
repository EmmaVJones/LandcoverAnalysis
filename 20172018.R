# this script identifies which watersheds need landcover information to update the 2018 
#  IR dataset for 2020IR release

# Libraries
library(tidyverse)
library(sf)


# Bring in 2018 IR dataset
criticalLink <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2018/processedData/Wadeable_ProbMon_2001-2016_EVJ.csv')

# Bring in 2017 sites sampled
x2017 <- readxl::read_excel('C:/HardDriveBackup/ProbMon/2017/Regional_Results_Spring2017_Final.xlsx',
                            sheet = 'GIScrosswalk (2)')



#Bring in 2020 IR LRBS dataset
LRBS <- readxl::read_excel('C:/HardDriveBackup/PHAB_R/TMDLsummary_2019-12-17.xlsx')


# Identify differences, okay to just use StationID bc follow up final datasets will rerun
#  appropriate NLCD info for trend sites if the watershed exists
wshdDiff <- filter(LRBS, !(StationID %in% unique(criticalLink$StationID)))

# that's a lot of missing sites, but this dataset also includes targeted stress/ref/TMDL, etc.

