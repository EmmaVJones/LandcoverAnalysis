# download file to R for documentation on file organization



download.file(url = 'https://www2.census.gov/geo/tiger/TIGER2018/ROADS/tl_2018_51740_roads.zip',
              destfile = 'tigerRoadsPull/tl_2018_51740_roads.zip')

# worked!


# now need to figure out a way to get it to only pull VA (e.g. search for only tl_201X_51XXX_roads.zip)


# first view the source of the FTP site that I want to hit: (in chrome ctrl + U)

#http://bradleyboehmke.github.io/2015/12/scraping-html-text.html

# use rvest package?
install.packages('rvest')
library(rvest)
library(tidyverse)

# scrape ftp page for headers
scraping_tiger <- read_html("https://www2.census.gov/geo/tiger/TIGER2018/ROADS")

z <- scraping_tiger %>%
  html_nodes("tr") %>% # find only table records
  html_text() #%>%
  #as.tibble() # maybe?

# time to find all string options we want
library(stringr)

ztest <- z[1:10]

str_detect('tl_2018_01003', ztest)# no
str_subset('tl_2018_01003', ztest)# no
str_locate('tl_2018_01003', ztest)# no
str_match('tl_2018_01003', ztest) # no

grepl('tl_2018_01003', ztest) # success
grepl('tl_2018_010*', ztest) # success with wildcard!
subset(ztest, grepl('tl_2018_010*', ztest)) # success subsetting just those that match wildcard string!

# now try with virginia code 51XXX
any(grepl('tl_2018_51*', z))
zVA <- subset(z,grepl('tl_2018_51*', z)) # success with wildcard!

# now split string on .zip and keep everything before .zip
zVA[1]

strsplit(zVA[1],'.zip') # not quite, dropped .zip
strsplit(zVA[1],'[.zip]') # yikes
strsplit(zVA[1],'(.zip)') # nope
str_split(zVA[1],'.zip', simplify = TRUE, n = 2)

# maybe don't need to worry about dropping zip because can always add that back

# moving on, find all files that I need to download for VA
VAoptions <- paste0(str_split(zVA,'.zip', simplify = TRUE, n = 2)[,1],'.zip')


# brilliant, now use that to download all files in said vector

year <- 2018
fileName <- VAoptions[1:2]
outDirectory <- 'tigerRoadsPull'

downloadTigerRoadsByYear <- function(year,fileName,outDirectory){
  for(i in 1:length(fileName)){
    download.file(url = paste0('https://www2.census.gov/geo/tiger/TIGER',
                             year,'/ROADS/', fileName[i]),
                destfile = paste0(outDirectory,'/', fileName[i]))
  }
}
 
downloadTigerRoadsByYear(2018,VAoptions[1:2], 'tigerRoadsPull')


# now need to build something to extract and combine all files into single shapefile
