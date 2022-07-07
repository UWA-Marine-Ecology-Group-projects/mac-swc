library(dplyr)

setwd(getwd())


dat <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Habitat.csv")%>%
  # dplyr::filter(broad.seagrasses>0,depth>30) %>% # Turn on to make the old dataset
  dplyr::select(campaignid,sample, longitude, latitude, depth, broad.seagrasses,date)%>%
  dplyr::mutate(id = paste(campaignid, sample, sep = "."))%>%
  dplyr::filter(!id%in%c("2021-03_West-Coast_BOSS.58","2021-03_West-Coast_BOSS.20"))%>% #remove 2 samples wrong annotation
  dplyr::mutate(method = ifelse(campaignid %in% "2020-10_south-west_stereo-BRUVs", "BRUV", "BOSS")) %>%
  glimpse()

# write.csv(dat, file = "data/tidy/swc_seagrass_locations.csv",row.names = F)

write.csv(dat, file = "data/tidy/swc_seagrass_locations.csv",row.names = F)
