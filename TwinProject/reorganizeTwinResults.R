# find missing watersheds

twinResult <- read_csv(paste(saveHere,'Result10.csv'))
humanResult <- read_csv('C:/HardDriveBackup/R/GitHub/LandcoverAnalysis/Results/Interns2019/ Result10.csv')

allResult <- readxl::read_excel('C:/HardDriveBackup/R/GitHub/LandcoverAnalysis/Results/ProbRedo2001_2016/2001_2016final.xlsx',
                                sheet = 'Results')

uniqueWshdListYearT <- twinResult %>%
  group_by(StationID, YearSampled) %>%
  distinct(StationID) %>% ungroup()

uniqueWshdListYearH <- humanResult %>%
  group_by(StationID, YearSampled) %>%
  distinct(StationID) %>% ungroup()

uniqueWshdListYearA <- allResult %>%
  group_by(StationID, YearSampled) %>%
  distinct(StationID) %>% ungroup()



uniqueWshdListYearH$StationID == uniqueWshdListYearT$StationID

uniqueWshdListYearT$StationID[!uniqueWshdListYearT$StationID %in% uniqueWshdListYearH$StationID]

twin <- c("1BBRY006.55", "2-XRA000.47", "2AGOC000.54", "2BXAW001.16",
          "3-GAR004.93", "4ABKN008.26","4AXOK000.29", "8-XKA000.91")

uniqueWshdListYearT$StationID %in% uniqueWshdListYearA$StationID


z <- filter(allResult, StationID %in% twinResult$StationID)

uniqueWshdListYearZ <- z %>%
  group_by(StationID, YearSampled) %>%
  distinct(StationID) %>% ungroup()

uniqueWshdListYearZ %>% arrange(StationID) == uniqueWshdListYearT %>% arrange(StationID)

write.csv(z, 'Results/Interns2019/FinalHandDelineationTwinResults.csv', row.names = F)
