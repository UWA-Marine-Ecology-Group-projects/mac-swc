#join the two campaigns

library(dplyr)
library(GlobalArchive)

working.dir <- "H:/GitHub/mac-swc/data/tidy"
setwd(working.dir)

dir()

march  <- read.csv("2021-03_West-Coast_BOSS._broad.habitat.csv")%>%
  ga.clean.names()%>%
  dplyr::select(-c(mean.relief, sd.relief, location))%>%
  dplyr::mutate(broad.crinoids = c(0))%>%
  dplyr::mutate(date = as.character(date))%>%
  glimpse()
names(march)
str(march)

october <- read.csv("2020-10_south-west_BOSS_random-points_broad.habitat.csv")%>%
  ga.clean.names()%>%
  dplyr::select(-c(successful.count, location, -broad.total.points.annotated, site, fov.facing.up, fov.limited, fov.open))%>%
  dplyr::mutate(date = as.character(date))%>%
  glimpse()
names(october)
str(march)

full <- bind_rows(march,october)
