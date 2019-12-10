
# Built in R 3.6.1

# This script scrapes all available tiger road data for a given year from the US census FTP site.
# The script then unzips all files for Virginia and combines into a single shapefile for further
#   spatial analyses. 
# This script was built to ease the labor intensive process of acquiring and combining tiger
#   road data (available by county only at higher resolutions) for the watershed landcover
#   analysis tool. The data is available yearly and requires many hours to organize.

# Author: Emma Jones (emma.jones@deq.virginia.gov)
# Last Updated: 11/22/2019


# Load libraries
library(rvest) # 0.3.4
library(tidyverse) #1.2.1
library(stringr) #1.4.0
library(sf) # 0.7-7



# Step 1: Identfy year to pull data from. This is critical to getting the FTP web address correct for 
#         all subsequent analyses. If the web address fed to the subsequent function is incorrect, then
#         no data can be pulled.

# e.g. https://www2.census.gov/geo/tiger/TIGER2018/  is the appropriate web address for 2018 data.

year <- 2010
FTPaddress <- paste0("https://www2.census.gov/geo/tiger/TIGER",year,"/ROADS")
dirName <- 'tigerRoadsPull' # create a folder in the project directory with this exact name to store
                            # all downloaded zip files





# Step 2: Scrape FTP page for headers.  The scraping_tiger object will contain all available files for 
#scraping_tiger <- read_html(FTPaddress) %>% # edit this 
 # html_nodes("tr") %>% # find only table records
#  html_text() # convert to character object

# Step 2.1: Identify all counties that intersect the VA NHD
#This was done in GIS by clipping US county data to the VA NHD. All the state and county FIPS codes that 
# could fall into a given probmon watershed are then able to be efficiently downloaded from the TIGER FTP
#VANHD <- st_read('G:/evjones/GIS/ProbMonGIS/GISdata/nhd_83albers.shp')
#UScounties <- st_read('GISdata/tl_2017_us_county.shp') %>%
#  st_transform(st_crs(VANHD))

#nhdCounties <-  UScounties[VANHD, ] 
#st_write(nhdCounties, "GISdata/counties.shp")

#rm(UScounties);rm(VANHD)

# Don't need to repeat above step more than once, here are counties we need
nhdCounties <- st_read('GISdata/counties.shp') %>%
  st_set_geometry(NULL) %>%
  distinct(GEOID) # keep only unique FIPS codes (already concatinated with state + county)
  


# Step 3: Download all appropriate files.

# custom function to do so
downloadTigerRoadsByYear <- function(year,fileName,outDirectory){
  for(i in 1:length(fileName)){
    download.file(url = paste0('https://www2.census.gov/geo/tiger/TIGER',
                               year,'/ROADS/', fileName[i]),
                  destfile = paste0(outDirectory,'/', fileName[i]))
  }
}

downloadTigerRoadsByYear(year,
                         paste0('tl_',year,'_', as.character(nhdCounties$GEOID),'_roads.zip'),
                         paste0(dirName,'/', year))
#in unzip folder there is tl_2016_10005_roads.zip

# Step 4: Unzip and combine all files in the dirName directory into one shapefile
filenames <- list.files( paste0(dirName,'/', year), pattern="*.zip", full.names=TRUE)
#filenames <- filenames[-length(filenames)] # remove the last bogus record, only for 2018
lapply(filenames, unzip, exdir=paste0('tigerRoadsPull/unzipped/',year)) # unzip all filenames that end in zip into folder named unzipped
filenames_slim <- gsub('.zip', '' , gsub(paste0('tigerRoadsPull/',year,'/'),'', filenames ))

# check to make sure all downloaded files were unzipped correctly
filenamesUnzipped <- list.files(paste0(dirName,'/unzipped/',year), pattern="*.shx", full.names=F) # search by .cpg bc .shp has a few extension options and duplicates unique names
filenamesUnzipped_slim <- gsub('.shx','',filenamesUnzipped)

all(filenames_slim %in% filenamesUnzipped_slim )
# if true then cool

# if not then find out which missing
filenames_slim [!(filenames_slim %in% filenamesUnzipped_slim )]
# an output of character(0) is good because it means there are none missing

# Step 5: Read in unzipped files and combine to single object
filenamesUnzipped <- paste0(dirName,'/unzipped/',year,'/',gsub('.shx','.shp', filenamesUnzipped)) # change .cpg to .shp for file reading in

shapefiles <- filenamesUnzipped %>%
  map(st_read) %>%
  reduce(rbind)
  

# Step 6: Save out shapefile
st_write(shapefiles, paste0('GISdata/TIGERroads/', year, 'tigerRoads.shp'))


