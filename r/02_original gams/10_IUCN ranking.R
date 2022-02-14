#Claude has edited this and it is pretty poxy
#need to fix up

# Set directories----
rm(list=ls())

# Study name ----
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

# Libraries required
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(tidyverse)
library(stringr)

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once
boss <- read_csv("data/staging/2020-2021_south-west_BOSS.complete.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  glimpse()

bruv <- read_csv("data/staging/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  glimpse()

maxn <- bind_rows(boss,bruv)

# Read in metadata ----
metadata<-read_csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  dplyr::select(sample,latitude,longitude,date,time,depth)

# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name)%>% 
  distinct()%>%
  mutate(scientific = paste(genus,species, sep = " "))%>%
  glimpse()

unique(master$fishing.type)
names(master)

length(unique(metadata$sample))

total.number.fish <- sum(maxn$maxn)  #25525

#species list
species.list <- as.data.frame(unique(maxn$scientific)) %>%
  dplyr::rename(scientific = 'unique(maxn$scientific)')%>%
  glimpse()

spp.species <- maxn %>%
  dplyr::filter(species%in%c("spp","sp","sp10"))%>%
  select(scientific)%>%
  distinct()%>%
  glimpse()

fished.species <- species.list %>%
  left_join(master)%>%
    dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                         "Carangidae Unknown spp",
                                                         "Platycephalidae Platycephalus spp",
                                                         "Platycephalidae Leviprora spp",
                                                         "Scombridae Sarda spp",
                                                         "Scombridae Unknown spp",
                                                         "Sillaginidae Sillago spp",
                                                         "Lethrinidae Gymnocranius spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type%in%c("R","C/R","B/C","B/C/R","C","B/R" ))%>%
  dplyr::select(scientific,australian.common.name, fishing.type)%>%
  glimpse()

write.csv(fished.species, file = "plots/original gamms/swc.fished.species.csv", row.names = F)

unique(spp.species$scientific)

#IUCN species 
unique(master$iucn.ranking)

iucn.species <- species.list %>%
  left_join(master)%>%
  dplyr::filter(iucn.ranking%in%c("Vulnerable","Near Threatened","Critically Endangered","Endangered"))%>%
  dplyr::select(scientific,australian.common.name, iucn.ranking)%>%
  glimpse()

write.csv(iucn.species, file = "plots/original gamms/swc.iucn.species.csv", row.names = F)

