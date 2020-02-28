

VA <- subset(scraping_tiger,grepl('tl_2018_51*', scraping_tiger)) # find everything for VA (51 is the code)
VAoptions <- paste0(str_split(VA,'.zip', simplify = TRUE, n = 2)[,1],'.zip') # get names of all zip files

# This actually includes other states, so filter to real VA options

# actually need tl_2018_51001 : tl_2018_51199
VArealOptions <- paste0(c(paste0('tl_2018_5100',1:9),
                          paste0('tl_2018_510',10:99),
                          paste0('tl_2018_51',100:199)),
                        '_roads.zip')

VAoptions <- VAoptions[VAoptions %in% VArealOptions]

# need to add in border counties in other states to ensure watersheds that go out of state are still accurate
# this was done by looking at county maps in adjacent states and finding the corresponding code in https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2018&layergroup=Roads

#https://wiki.radioreference.com/index.php/Maryland_Counties

otherCounties <- paste0(c(
  # Maryland
  paste0('tl_2018_', c(24023,24001,24043......)),
  '_roads.zip')