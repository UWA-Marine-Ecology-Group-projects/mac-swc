#Claude has edited this and it is pretty poxy
#need to fix up

# Set directories----
rm(list=ls())

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
library(GlobalArchive)

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once

#read in the data 
species.list <- read_csv("data/tidy/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::select(family,genus,species,scientific,status,maxn)%>%
  glimpse()

num.fish <- species.list%>%
  summarise(fish = sum(maxn))%>%
  glimpse()

unique.species <- species.list %>%
  dplyr::select(-maxn)%>%
  unique()

length(unique(unique.species$scientific)) #146 species
length(unique(unique.species$genus)) #104 genera
length(unique(unique.species$family)) #62 families

# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- unique.species %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))%>%
  dplyr::select(status,scientific,fishing.type,australian.common.name)%>%
  glimpse()

iucn.species <- unique.species %>%
  dplyr::left_join(master) %>%
  dplyr::filter(!is.na(iucn.ranking))%>%
  dplyr::filter(!iucn.ranking%in%c("Least Concern", "Data Deficient")) %>%
 glimpse()


# 
# ubiquity <- maxn%>%
#   filter(maxn>0) %>%
#   group_by(family,genus,species,scientific)%>%
#   summarise(no.of.deployments=n())%>%
#   ungroup() %>%
#   mutate(ubiquity=(no.of.deployments/200)*100)

