###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, fine bathymetry derivatives
# Task:    Check predictors and combine data for FSSgam fish length and abundance - multibeam patch
# author:  Claude & Brooke
# date:    February 2022
##

#devtools::install_github("beckyfisher/FSSgam_package") #run once
library(rstanarm)
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
library(corrr)

rm(list=ls())

# Set the study name
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

###################### MAXN ################
# Bring in and format the data----
#MaxN
#BRUV
maxn.bruv <-read.csv("data/staging/2020_south-west_stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()
length(unique(maxn.bruv$id))

#BOSS
maxn.boss <-read.csv("data/staging/2020-2021_south-west_BOSS.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::glimpse()
length(unique(maxn.boss$id))
maxn <- bind_rows(maxn.bruv,maxn.boss)   

# Metadata ----
metadata <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv") %>%    #from 01_format data/BRUV format/01-03
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()

# Bathymetry derivatives ---- # Em change this so read in Anita's file, filter out NAs 
bathy <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.multibeam-derivatives.csv") %>%    #from r/03_multibeam gam/01.prepare_covariates.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::filter(!flowdir=="NA")%>%
  dplyr::select(-depth)%>%
  unique()%>%
  dplyr::glimpse()

length(unique(bathy$sample))

# Distance to boat ramp ----
ramps <- read.csv('data/tidy/2020-2021_south-west_BOSS-BRUV.distance.to.ramp.csv') %>%  #from r/01_format data/Spatial/06_Get_distance_from_boat_ramps.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  distinct()%>%
  dplyr::glimpse()

#habitat
habitat <- readRDS("data/tidy/dat.full.habitat.rds")%>%                                 #from r/01_format data/Habitat/08_join-dot-point-measurements.R
  dplyr::select(1:23)%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  distinct()%>%                                                                 #there are some duplicates here, need to fix in original
  glimpse()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,id) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(total.abundance=rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,3:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(id,total.abundance,species.richness) %>%
  tidyr::gather(.,"scientific","maxn",2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
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
  dplyr::glimpse()

unique(master$fishing.type)

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(ta.sr)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  filter(!tpi=="NA")%>%           #should crop to extent of multibeam
  distinct()%>%
  glimpse()

testboss <- combined.maxn %>%
  dplyr::filter(method%in%"BOSS")
testbruv <- combined.maxn %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$id))   #208
length(unique(testbruv$id))   #129
(208+129)*2   #674 - all good

# Set predictor variables---
pred.vars=c("depth", "depth.multibeam","slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated", "detrended")

dat.maxn <- combined.maxn

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
correlate(combined.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8) %>%
  dplyr::filter(row_number() %% 2 == 1)      #remove every second row, they are just duplicates
# reef and sand correlated
#slope and roughness - just use roughness
#depth and multibeam depth - using multibeam depth

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.maxn[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#sponge is a bit dodgy
par(mfrow=c(1,1))
ggplot()+
  geom_point(data = combined.maxn,aes(sample,maxn),alpha = 0.2)+
  theme_classic()+facet_wrap(~scientific)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#some pretty suspect points here in total abundance, but will leave in for now

# # remove sample with maxn >600? Prob an outlier and driving a lot of relationships
# outlier <- dat.maxn %>%
#   filter(maxn>600)%>%
#   glimpse()
# 
# # Max N = 745!! 
# dat.maxn <- dat.maxn %>%
#   filter(maxn <600)%>%
#   glimpse()

plot(dat.maxn$maxn)
##### USE 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth.multibeam - use non-transformed   
# broad.reef - use non-transformed
# tpi - use non-transformed       
# roughness - use non-transformed
#detrended bathymetry - use non-transformed

##### REMOVE
#in this dataset relief and reef are pretty highly correlated (0.92) could be an issue but leave for now
#depth and multibeam depth highly correlated - this is good i guess
# sand - correlated with reef and mean relief
# slope - correlated with roughness
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few
# sponges - too few

# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","broad.macroalgae","broad.reef",
            "distance.to.ramp","aspect", "tpi","roughness","depth.multibeam","detrended")


# Remove any unused columns from the dataset
dat.maxn <- dat.maxn %>%
  dplyr::mutate(broad.macroalgae=broad.macroalgae/broad.total.points.annotated)%>%
  dplyr::mutate(broad.reef=broad.reef/broad.total.points.annotated)%>%
  dplyr::select(campaignid,sample, method,status, site, scientific, maxn,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth.multibeam","detrended",method) %>%
  as.data.frame()

saveRDS(dat.maxn, "data/tidy/dat.maxn.multibeam.rds")

########## LENGTHS ###############
#bring in and format data
# Length ----
length <-read.csv('data/staging/2020_south-west_stereo-BRUVs.complete.length.csv') %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

metadata.bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::filter(successful.length%in%"Yes")%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  glimpse()

length(unique(length$sample)) #277

test <- length %>%
  filter(number>0)%>%
  distinct(sample)

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
                                                       "Lethrinidae Gymnocranius spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","tephraeops","lineolatus","cirratus",
                              "purpurissatus","lewini","nigroruber"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Leviprora spp"),300,minlegal.wa))

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # Checked all of these with rec fish app - all don't have one

# Come back to maybe getting rid of some of these, but for now we continue on
legal <- fished.species %>%
  dplyr::filter(length>minlegal.wa) %>%
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

## Combine all the maxn data to be modeled into a single data frame
combined.length <- bind_rows(legal, sublegal) # taken out all individual species, only doing legal and sublegals

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata.bruv) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::filter(!tpi=="NA")%>%
  dplyr::glimpse()

test <- complete.length %>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(n=n())

#set predictor variables
pred.vars=c("depth", "depth.multibeam","slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated")

dat.length <- complete.length

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
correlate(combined.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8) %>%
  dplyr::filter(row_number() %% 2 == 1)      #remove every second row, they are just duplicates
# reef and sand correlated

#check for outliers
par(mfrow=c(1,1))
ggplot()+
  geom_point(data = dat.length,aes(sample,number),alpha = 0.2)+
  theme_classic()+facet_grid(~scientific)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#one outlier in smaller than legal size
# Tim says keep it in for now
# outlier <- dat.length %>%
#   filter(number>40)%>%
#   glimpse()
# 
# #filter out this outlier, its a big school of swallowtail nannys
# dat.length <- dat.length %>%
#   filter(number <600)%>%
#   glimpse()

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

##### USE 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth.multibeam - use non-transformed   
# broad.reef - use non-transformed
# tpi - use non-transformed       
# roughness - use non-transformed
#detrended bathymetry - use non-transformed

##### REMOVE
#in this dataset relief and reef are pretty highly correlated (0.92) could be an issue but leave for now
#depth and multibeam depth highly correlated - this is good i guess
# sand - correlated with reef and mean relief
# slope - correlated with roughness
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few
# sponges - too few

# Re set predictor variables 
pred.vars=c("mean.relief","sd.relief","broad.macroalgae","broad.reef",
            "distance.to.ramp", "tpi","roughness","depth.multibeam","detrended")

# Remove any unused columns from the dataset 
dat.length <- complete.length %>%
  dplyr::mutate(broad.macroalgae=broad.macroalgae/broad.total.points.annotated)%>%
  dplyr::mutate(broad.reef=broad.reef/broad.total.points.annotated)%>%
  dplyr::select(sample, status, site, scientific, number,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth.multibeam","detrended")%>%
  as.data.frame()

saveRDS(dat.length, "data/tidy/dat.length.multibeam.rds")
