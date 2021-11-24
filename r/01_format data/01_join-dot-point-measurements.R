#join the two campaigns

rm(list=ls())

library(dplyr)
library(GlobalArchive)

working.dir <- "H:/GitHub/mac-swc/data/tidy"
setwd(working.dir)

dir()

march  <- read.csv("2021-03_West-Coast_BOSS._broad.habitat.csv")%>%
  ga.clean.names()%>%
  dplyr::select(-c(mean.relief, sd.relief, location, fov.open))%>%
  dplyr::mutate(broad.crinoids = c(0))%>%
  dplyr::mutate(date = as.character(date))%>%
  dplyr::mutate(campaignid = c('2021-03_West-Coast_BOSS'))%>%
  dplyr::select(campaignid, everything())%>%
  glimpse()
names(march)
str(march)

october <- read.csv("2020-10_south-west_BOSS_random-points_broad.habitat.csv")%>%
  ga.clean.names()%>%
  dplyr::select(-c(successful.count, location, fov.total.points.annotated, site, fov.facing.up, fov.limited, fov.open))%>%
  dplyr::mutate(date = as.character(date))%>%
  dplyr::mutate(broad.unknown = c(0))%>%
  dplyr::mutate(campaignid = c('2020-10_south-west_BOSS'))%>%
  dplyr::select(campaignid, everything())%>%
  glimpse()
names(october)
str(march)

full <- bind_rows(march,october)%>%
  glimpse()


#416 samples
#262 from october
#154 from march



full$sum <- as.numeric(apply(full[,8:20], 1, sum))


write.csv(full, file = "2020-2021_south-west_broad.habitat.csv", row.names = F)





