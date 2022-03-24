###
# Project: mac - South-west Corner
# Data:    BRUV and BOSS habitat
# Task:    join four campaigns - june bruv,october BOSS, october BRUV, march BOSS
# author:  Claude
# date:    February 2022
##

rm(list=ls())

library(dplyr)
library(GlobalArchive)
library(janitor)
library(rgdal)

working.dir <- getwd()
setwd(working.dir)

# Habitat ----
habitat.2020.10.bruv <- read.csv("data/staging/2020-10_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count,habitat.backwards.image.saved,fov.total.points.annotated)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs",method = "BRUV") %>%
  dplyr::glimpse()

habitat.2020.10.boss <- read.csv("data/staging/2020-10_south-west_BOSS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time.bottom,site,location,successful.count,fov.total.points.annotated,depth)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_BOSS",method = "BOSS") %>%
  dplyr::glimpse()

habitat.2020.06 <- read.csv("data/staging/2020-06_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count,fov.total.points.annotated)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs",method = "BRUV") %>%
  dplyr::glimpse()

habitat.2021.03 <- read.csv("data/staging/2021-03_West-Coast_BOSS._broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,location,depth)) %>%
  dplyr::mutate(campaignid = "2021-03_West-Coast_BOSS",method = "BOSS") %>%
  ga.clean.names()%>%
  dplyr::rename(broad.total.points.annotated=total.points.annotated)%>%
  dplyr::glimpse()

names(habitat.2020.10.bruv)
names(habitat.2020.10.boss)
names(habitat.2020.06)
names(habitat.2021.03)

janitor::compare_df_cols(habitat.2020.10.bruv,habitat.2020.10.boss,habitat.2020.06,habitat.2021.03)   #fixed missing relief for 2020-10_BOSS

habitat <-bind_rows(habitat.2020.06, habitat.2020.10.bruv,habitat.2020.10.boss,habitat.2021.03) %>%
  tidyr::replace_na(list(broad.consolidated=0,
                         broad.macroalgae=0,
                         broad.seagrasses=0,
                         broad.sponges=0,
                         broad.unconsolidated=0,
                         broad.bryozoa=0,
                         broad.hydroids=0,
                         broad.octocoral.black=0,
                         broad.stony.corals=0,
                         fov.facing.up=0,
                         broad.ascidians=0,
                         broad.true.anemones=0,
                         broad.crinoids=0)) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + 
                  broad.seagrasses + broad.sponges + broad.stony.corals + broad.crinoids + broad.ascidians + broad.invertebrate.complex +
                  broad.true.anemones) %>%
  # dplyr::mutate(broad.ascidians = broad.ascidians/broad.total.points.annotated,
  #               broad.bryozoa = broad.bryozoa/broad.total.points.annotated,
  #               broad.consolidated = broad.consolidated/broad.total.points.annotated,
  #               broad.crinoids = broad.crinoids/broad.total.points.annotated,
  #               broad.hydroids = broad.hydroids/broad.total.points.annotated,
  #               broad.invertebrate.complex = broad.invertebrate.complex/broad.total.points.annotated,
  #               broad.macroalgae = broad.macroalgae/broad.total.points.annotated,
  #               broad.octocoral.black = broad.octocoral.black/broad.total.points.annotated,
  #               broad.reef = broad.reef/broad.total.points.annotated,
  #               broad.seagrasses = broad.seagrasses/broad.total.points.annotated,
  #               broad.sponges = broad.sponges/broad.total.points.annotated,
  #               broad.stony.corals = broad.stony.corals/broad.total.points.annotated,
  #               broad.true.anemones = broad.true.anemones/broad.total.points.annotated,
  #               broad.unconsolidated = broad.unconsolidated/broad.total.points.annotated)%>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(campaignid,sample,everything()) %>% # re-ordering hab columns 
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  # dplyr::mutate(test = rowSums(.[,c(3:10,12:14,16:17)]))%>%                     #all good             
  dplyr::glimpse()

#bring in metadata to join lat longs for Kingsley
# Metadata ----
#bruv
metadata.bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(method = "BRUV")%>%
  dplyr::glimpse()
metadata.bruv$date <- as.character(metadata.bruv$date)
#boss
metadata.boss.10 <- read.csv("data/raw/em export/2020-10_south-west_BOSS_Metadata.csv") %>%
  ga.clean.names()%>%
  dplyr::mutate(campaignid="2020-10_south-west_BOSS")%>%
  dplyr::select("campaignid","sample","latitude",
                "longitude","date","time.bottom","location","status",
                 "site","depth","observer","successful.count","successful.length",
                "raw.hdd.number","con.hdd.number")%>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  #dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
 # dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(site=seq(1:263))%>%
  dplyr::mutate(site=paste(method,site,sep = ""))%>%
  dplyr::rename(time=time.bottom)%>%
  dplyr::filter(!sample%in%"287")%>%
  dplyr::glimpse()

metadata.boss.03 <- read.csv("data/raw/em export/2021-03_West-Coast_BOSS_Metadata.csv") %>%
  ga.clean.names()%>%
  dplyr::mutate(campaignid="2021-03_West-Coast_BOSS")%>%
  dplyr::select("campaignid","sample","latitude",
                "longitude","date","time.bottom","location","status",
                "site","depth","observer","successful.count","successful.length",
                "raw.hdd.number","con.hdd.number")%>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  #dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(site=seq(126:279))%>%
  dplyr::mutate(site=paste(method,site,sep = ""))%>%
  dplyr::rename(time=time.bottom)%>%
  #dplyr::select(-id)%>%
  dplyr::glimpse()

metadata.boss.03$date <- as.character(metadata.boss.03$date)

janitor::compare_df_cols(metadata.boss.10,metadata.boss.03)

metadata.boss <- bind_rows(metadata.boss.10,metadata.boss.03)


#compare column names
janitor::compare_df_cols(metadata.bruv,metadata.boss)
metadata <- bind_rows(metadata.bruv,metadata.boss)
raw.metadata <- metadata

test <- metadata %>%
  group_by(campaignid, sample)%>%
  dplyr::summarise(n=n())  

#join in state/commonwealth zone and fishing status to all metadata columns
#we already have this for 2021-03 but will just add again for all
# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)

wa.marineparks <- readOGR(dsn="data/spatial/shapefiles/test1.shp")
proj4string(wa.marineparks)

proj4string(commonwealth.marineparks)<-CRS(wgs.84)
proj4string(wa.marineparks)<-CRS(wgs.84)

str(metadata)
metadata$latitude <- as.numeric(metadata$latitude)
metadata$longitude <- as.numeric(metadata$longitude)
coordinates(metadata) <- c('longitude','latitude')
proj4string(metadata)<-CRS(wgs.84)

metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks) %>%
  dplyr::select(ZoneName)

unique(metadata.commonwealth.marineparks$ZoneName)

metadata.state.marineparks <- over(metadata, wa.marineparks) %>%
  dplyr::select(Name)

unique(metadata.state.marineparks$Name)

names(metadata.commonwealth.marineparks)

metadata<-bind_cols(raw.metadata,metadata.commonwealth.marineparks)%>%
  bind_cols(.,metadata.state.marineparks)%>%
  dplyr::rename(Commonwealth.zone=ZoneName, State.zone=Name)%>%
  mutate(Status = if_else((Commonwealth.zone%in%c("National Park Zone")|
                             State.zone%in%c("Injidup Sanctuary Zone","Cape Freycinet Sanctuary Zone")),"No-take","Fished"))%>%
  dplyr::select(-c(status,commonwealth.zone,state.zone))%>%
  ga.clean.names()%>%
  glimpse()

#write metadata to a csv
write.csv(metadata, file = "data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv",row.names = F)

joined.habitat <- habitat %>%
  dplyr::left_join(metadata)%>%
  dplyr::select(-c(observer,raw.hdd.number,con.hdd.number,dataset))%>%
  dplyr::filter(!sample%in%c("IO282","11 - failed","08 - failed"))%>%                    #failed samples
  dplyr::filter(!sample%in%"19"|!campaignid%in%"2020-06_south-west_stereo-BRUVs")%>%     #more failed samples
  dplyr::filter(!sample%in%"FH36"|!campaignid%in%"2020-10_south-west_stereo-BRUVs")%>%   #and more
  dplyr::mutate(location="South-west")%>%
  distinct()%>%
  glimpse()

names(joined.habitat)

test <- joined.habitat %>%
  group_by(campaignid, sample)%>%
  dplyr::summarise(n=n())  

#save RDS
saveRDS(joined.habitat,'data/tidy/dat.full.habitat.rds')      

#save csv
write.csv(joined.habitat, file = "data/tidy/2020-2021_south-west_BOSS-BRUV.Habitat.csv",row.names = F)
