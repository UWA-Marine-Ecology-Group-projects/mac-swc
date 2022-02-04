setwd("//uniwa.uwa.edu.au/userhome/staff0/00104220/My Documents/R Scripts/SWC Lat Longs")
dir()

library(dplyr)

swc.boss<-read.csv("MEG_Labsheets_2021 - 2021-03_West-Coast_BOSS.csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample)) 


latlong <- read.csv("boss_swc_2021_0.csv")%>%
  select(ObjectId, x, y)%>%
  rename(Sample=ObjectId, Latitude = y, Longitude = x)

latlong$Sample <- as.character(latlong$Sample)

str(swc.boss)
str(latlong)

dat <- swc.boss %>%
  left_join(latlong)

write.csv(dat, "swc.lat.lon.csv")
