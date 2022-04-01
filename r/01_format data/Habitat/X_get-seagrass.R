library(dplyr)

setwd(getwd())


dat <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Habitat.csv")%>%
  dplyr::filter(broad.seagrasses>0,depth>30)%>%
  dplyr::select(campaignid,sample, longitude, latitude, depth, broad.seagrasses)%>%
  dplyr::mutate(id = paste(campaignid, sample, sep = "."))%>%
  dplyr::filter(!id%in%c("2021-03_West-Coast_BOSS.58","2021-03_West-Coast_BOSS.20"))%>% #remove 2 suspect samples
  glimpse()

write.csv(dat, file = "data/tidy/swc_seagrass_locations.csv",row.names = F)
