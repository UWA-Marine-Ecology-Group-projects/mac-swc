##### Fixing metadata lat long issue
#Claude

rm(list=ls())

library(dplyr)
library(argosfilter)

# Bring in and format the data----
habi  <- readRDS("data/tidy/dat.full.habitat.rds")%>%                              # merged data from ??
  dplyr::mutate(id=paste(campaignid,sample, sep = "."))%>%
  distinct() #there is one sample with a duplicate

test.locations <- habi %>% 
  dplyr::filter(campaignid%in%"2020-10_south-west_BOSS")%>%
  dplyr::select(sample, latitude, longitude)

mbh <- read.csv("data/raw/2020-10_BOSS_MBH.csv")%>%
  dplyr::rename(sample=BRUVid, latitude.mbh = y2, longitude.mbh = x2)%>%
  dplyr::mutate(sample=as.character(sample))%>%
  dplyr::select(sample, latitude.mbh, longitude.mbh)%>%
  glimpse()

fh <- read.csv("data/raw/fishing_hwy_locations.csv")%>%
  dplyr::rename(sample=Sample, latitude.mbh = Latitude, longitude.mbh = Longitude)%>%
  dplyr::select(sample, latitude.mbh, longitude.mbh)%>%
  glimpse()

mbh <- bind_rows(mbh, fh)%>%
  glimpse()

# mbh <- read.csv("data/raw/in_and_out_locations.csv")%>%
#   dplyr::rename(sample=Sample, latitude.mbh = Latitude, longitude.mbh = Longitude)%>%
#   dplyr::mutate(sample=as.character(sample))%>%
#   glimpse()

locs <- test.locations %>%
  dplyr::left_join(mbh)%>%
  dplyr::select(sample, latitude.mbh,longitude.mbh)%>%
  dplyr::rename(Sample=sample)%>%
  # dplyr::filter(!is.na(longitude.mbh))%>%
  glimpse()

metadata <- read.csv("data/raw/em export/2020-10_south-west_BOSS_Metadata.csv")%>%
  glimpse()

fixed.metadata <- metadata %>%
  left_join(locs)%>%
  glimpse()

write.csv(fixed.metadata, file = "data/raw/2020-10_BOSS_fixed.metadata.csv")
