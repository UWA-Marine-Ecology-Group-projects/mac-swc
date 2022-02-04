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

rm(list=ls())

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

###################### MAXN ################
# Bring in and format the data----
# MaxN ----
maxn <-read.csv("data/tidy/2020_south-west_stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Metadata ----
metadata <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Bathymetry derivatives ---- # Em change this so read in Anita's file, filter out NAs 
bathy <- read.csv("data/tidy/2020_south-west_multibeam-derivatives.csv") %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::filter(!flowdir=="NA")%>%
  dplyr::select(-depth)%>%
  unique()%>%
  dplyr::glimpse()

length(unique(bathy$sample))

# Distance to boat ramp ----
ramps <- read.csv('data/tidy/2020_south-west_stereo-BRUVs.distance.to.ramp.csv') %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Habitat ----
habitat.2020.10 <- read.csv("data/tidy/2020-10_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count,habitat.backwards.image.saved)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.10)

habitat.2020.06 <- read.csv("data/tidy/2020-06_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.06) # 0-100

habitat <-bind_rows(habitat.2020.06, habitat.2020.10) %>%
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
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + broad.seagrasses + broad.sponges + broad.stony.corals) %>%
  dplyr::mutate(broad.ascidians = broad.ascidians/broad.total.points.annotated,
                broad.bryozoa = broad.bryozoa/broad.total.points.annotated,
                broad.consolidated = broad.consolidated/broad.total.points.annotated,
                broad.crinoids = broad.crinoids/broad.total.points.annotated,
                broad.hydroids = broad.hydroids/broad.total.points.annotated,
                broad.invertebrate.complex = broad.invertebrate.complex/broad.total.points.annotated,
                broad.macroalgae = broad.macroalgae/broad.total.points.annotated,
                broad.octocoral.black = broad.octocoral.black/broad.total.points.annotated,
                broad.reef = broad.reef/broad.total.points.annotated,
                broad.seagrasses = broad.seagrasses/broad.total.points.annotated,
                broad.sponges = broad.sponges/broad.total.points.annotated,
                broad.stony.corals = broad.stony.corals/broad.total.points.annotated,
                broad.true.anemones = broad.true.anemones/broad.total.points.annotated,
                broad.unconsolidated = broad.unconsolidated/broad.total.points.annotated)%>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(campaignid,sample,everything()) %>% # re-ordering hab columns 
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()


# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample,total.abundance,species.richness) %>%
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

spp.species<-maxn%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) # Brooke removed leatherjackets, sea sweeps and goat fish

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse()

# Select species of interest to model ----
# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  dplyr::top_n(10)%>%
  ungroup()
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",            
                                  "Labridae Coris auricularis",
                                  "Scorpididae Neatypus obliquus",
                                  "Pomacentridae Chromis klunzingeri"
  ))%>%
  dplyr::select(sample,scientific,maxn)%>%
  distinct()%>%
  glimpse()

test.samples <- species.maxn %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n())

unique(species.maxn$scientific)

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(#fished.maxn, species.maxn, 
                           ta.sr)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  filter(!tpi=="NA")%>%           #should crop to extent of multibeam
  distinct()%>%
  glimpse()

length(unique(combined.maxn$sample))   #129
129*2       #258 - all good

# Set predictor variables---
pred.vars=c("depth", "depth.multibeam","slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated", "detrended")

dat.maxn <- combined.maxn

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.maxn[,pred.vars], use = "complete.obs"),2)
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
plot(dat.maxn$maxn)
# remove sample with maxn >600? Prob an outlier and driving a lot of relationships
outlier <- dat.maxn %>%
  filter(maxn>600)%>%
  glimpse()

# Max N = 745!! 
dat.maxn <- dat.maxn %>%
  filter(maxn <600)%>%
  glimpse()

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
  dplyr::select(sample, status, site, scientific, maxn,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth.multibeam","detrended") %>%
  as.data.frame()

saveRDS(dat.maxn, "data/tidy/dat.maxn.multibeam.rds")

########## LENGTHS ###############
#bring in and format data
# Length ----
length <-read.csv('data/tidy/2020_south-west_stereo-BRUVs.complete.length.csv') %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

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
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))

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

pinksnapper.legal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size pink snapper") %>%
  dplyr::glimpse()

pinksnapper.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size pink snapper") %>%
  dplyr::glimpse()

fished.bigger.20cm <- fished.species %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 20 cm") %>%
  dplyr::glimpse()

fished.bigger.30cm <- fished.species %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 30 cm") %>%
  dplyr::glimpse()

all.bigger.20cm <- length %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 20 cm") %>%
  dplyr::glimpse()

all.bigger.30cm <- length %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 30 cm") %>%
  dplyr::glimpse()

## Combine all the maxn data to be modeled into a single data frame
combined.length <- bind_rows(legal, sublegal) # taken out all individual species, only doing legal and sublegals

unique(combined.length$scientific)

complete.length <- combined.length %>%
  #dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(campaignid,sample,scientific,number) %>%
  tidyr::complete(nesting(campaignid,sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  filter(!tpi=="NA")%>%  # make sure only multibeam data in here
  dplyr::glimpse()

length(unique(complete.length$sample)) #116 - there are samples with successful count yes and successful length no

#set predictor variables
pred.vars=c("depth", "depth.multibeam","slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated")

dat.length <- complete.length

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.length[,pred.vars], use = "complete.obs"),2)
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
  dplyr::select(sample, status, site, scientific, number,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth.multibeam","detrended")%>%
  as.data.frame()

saveRDS(dat.length, "data/tidy/dat.length.multibeam.rds")
