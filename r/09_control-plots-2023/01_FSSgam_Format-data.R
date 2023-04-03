###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Make update greater than legal size data and size of first maturity
# author:  Claude
# date:    March 2023
##


#load packages
#devtools::install_github("beckyfisher/FSSgam_package") #run once
# library(rstanarm)
library(tidyverse)
library(dplyr)
library(mgcv)
library(FSSgam)
library(MuMIn)
library(doBy)
library(GlobalArchive)
library(googlesheets4)
library(stringr)
library(data.table)
library(googlesheets4)
library(ggplot2)
library(corrr)

rm(list=ls())

# Set the study name
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

# Bathymetry derivatives ----
bathy <- read.csv('data/tidy/2020-2021_south-west_BOSS-BRUV.bathy.derivatives.csv') %>%      #from r/02-original gams/X_Get_bathy-derivatives.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::select(ga.depth,tpi,roughness,detrended,id)%>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv('data/tidy/2020-2021_south-west_BRUVs-BOSS.distance.to.ramp.csv') %>%  #from r/01_format data/Spatial/06_Get_distance_from_boat_ramps.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::select(id, distance.to.ramp)%>%
  dplyr::glimpse()

#habitat
habitat <- readRDS("data/tidy/dat.full.habitat.rds")%>%                                 #from r/01_format data/Habitat/08_join-dot-point-measurements.R
  dplyr::select(1:23)%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::select(-c(campaignid, sample, method))%>%
  glimpse()

# # Create abundance of all recreational fished species ----
maturity <- readRDS("data/tidy/maturity.RDS") %>%
  ga.clean.names() %>%
  glimpse()

url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  dplyr::distinct()%>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% 
                                       c("Glaucosoma hebraicum", "Choerodon rubescens",
                                         "Epinephelides armatus"), NA, minlegal.wa)) %>%
  left_join(maturity, by = c("family", "genus", "species")) %>%
  dplyr::mutate(fb.length.at.maturity = fb.length.at.maturity.cm * 10) %>%
  select(-scientific) %>%
  dplyr::glimpse()

#####LENGTHS#####
# Bring in and format the data----
# Length ----
length <- read.csv("data/staging/2020_south-west_stereo-BRUVs.complete.length.csv") %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

test <- length %>%
  group_by(sample) %>%
  dplyr::summarise(n = n())

metadata.bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::filter(successful.length%in%"Yes")%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid, sample, sep = "."))%>%
  glimpse()

spp.species<-length%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Platycephalidae Leviprora spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp",
                                                       "Lethrinidae Gymnocranius spp",
                                                       "Berycidae Centroberyx sp1"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","tephraeops","lineolatus","cirratus",
                           "purpurissatus","lewini","nigroruber"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Leviprora spp"),300,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Berycidae Centroberyx sp1"),300,minlegal.wa)) %>%
  dplyr::mutate(minlegal.wa = ifelse(is.na(minlegal.wa), 0, minlegal.wa))

test <- fished.species %>%
  group_by(sample) %>%
  dplyr::summarise(n = n())

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # Checked all of these with rec fish app - all don't have one

# # Come back to maybe getting rid of some of these, but for now we continue on
legal <- fished.species %>%
  dplyr::filter(length > minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

mature <- fished.species %>%
  dplyr::filter(length > fb.length.at.maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than size of maturity") %>%
  dplyr::glimpse()

immature <- fished.species %>%
  dplyr::filter(length < fb.length.at.maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than size of maturity") %>%
  dplyr::glimpse()

# Just for the west coast indicator species
# Include both shallow and deep demersal species - even though we don't have all in the data
legal.ind <- fished.species %>%
  dplyr::filter(scientific %in% c("Labridae Choerodon rubescens",               # Baldchin
                                  "Glaucosomatidae Glaucosoma hebraicum",       # Dhufish
                                  "Sparidae Chrysophrys auratus",               # Snapper
                                  "Polyprionidae Polyprion oxygeneios",         # Hapuka
                                  "Polyprionidae Polyprion americanus",         # Bass grouper
                                  "Centrolophidae Hyperoglyphe antarctica")) %>%# Blue eye
  dplyr::filter(length > minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size indicator") %>%
  dplyr::glimpse()

sublegal.ind <- fished.species %>%
  dplyr::filter(scientific %in% c("Labridae Choerodon rubescens",               # Baldchin
                                  "Glaucosomatidae Glaucosoma hebraicum",       # Dhufish
                                  "Sparidae Chrysophrys auratus",               # Snapper
                                  "Polyprionidae Polyprion oxygeneios",         # Hapuka
                                  "Polyprionidae Polyprion americanus",         # Bass grouper
                                  "Centrolophidae Hyperoglyphe antarctica")) %>%# Blue eye
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size indicator") %>%
  dplyr::glimpse()

mature.ind <- fished.species %>%
  dplyr::filter(scientific %in% c("Labridae Choerodon rubescens",               # Baldchin
                                  "Glaucosomatidae Glaucosoma hebraicum",       # Dhufish
                                  "Sparidae Chrysophrys auratus",               # Snapper
                                  "Polyprionidae Polyprion oxygeneios",         # Hapuka
                                  "Polyprionidae Polyprion americanus",         # Bass grouper
                                  "Centrolophidae Hyperoglyphe antarctica")) %>%# Blue eye
  dplyr::filter(length > fb.length.at.maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than size of maturity indicator") %>%
  dplyr::glimpse()

immature.ind <- fished.species %>%
  dplyr::filter(scientific %in% c("Labridae Choerodon rubescens",               # Baldchin
                                  "Glaucosomatidae Glaucosoma hebraicum",       # Dhufish
                                  "Sparidae Chrysophrys auratus",               # Snapper
                                  "Polyprionidae Polyprion oxygeneios",         # Hapuka
                                  "Polyprionidae Polyprion americanus",         # Bass grouper
                                  "Centrolophidae Hyperoglyphe antarctica")) %>%# Blue eye
  dplyr::filter(length < fb.length.at.maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than size of maturity indicator") %>%
  dplyr::glimpse()

## Combine all the maxn data to be modeled into a single data frame
combined.length <- bind_rows(legal, sublegal, mature, immature,
                             legal.ind, sublegal.ind, mature.ind, immature.ind) # removed all other taxa

unique(combined.length$scientific)

# samples <- metadata.bruv %>%
#   dplyr::select(sample)

complete.length <- combined.length %>%
  dplyr::right_join(.,metadata.bruv) %>% # Add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # Remove NAs added in by complete
  dplyr::right_join(.,metadata.bruv) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

test <- complete.length %>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(n=n())

unique(complete.length$scientific)
length(unique(complete.length$sample))                                          #good still

# Remove any unused columns from the dataset 
dat.length <- complete.length %>%
  dplyr::filter(is.na(state.zone))%>%
  dplyr::mutate(broad.macroalgae=broad.macroalgae/broad.total.points.annotated)%>%
  dplyr::mutate(broad.reef=broad.reef/broad.total.points.annotated)%>%
  dplyr::select(id,campaignid,sample, status, site, planned.or.exploratory, scientific, number,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth","detrended") %>%
  dplyr::filter(!sample%in%c("S1","S2","S3","343","IO343","IO267")) %>%#sea cubes and weird TPI point
  as.data.frame()

ggplot()+
  geom_point(data = dat.length,aes(sample,number),alpha = 0.2)+
  theme_classic()+facet_wrap(~scientific)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

test <- dat.length %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n = n())

test <- metadata.bruv %>%
  dplyr::filter(successful.length %in% "Yes")

saveRDS(dat.length, "data/tidy/2023-03_dat.length-maturity.rds")

