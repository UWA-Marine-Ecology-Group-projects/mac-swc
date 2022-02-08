###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Check predictors and combine data for FSSgam fish length and abundance - full extent of BRUV samples
# author:  Claude & Brooke
# date:    February 2022
##


#load packages
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
library(googlesheets4)
library(ggplot2)
library(corrr)

rm(list=ls())

# Set the study name
name <- '2020-2021_south-west_BRUVs-BOSS' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

######    MAXN    ###############
# Bring in and format the data----
# MaxN ----
#BRUV
maxn.bruv <-read.csv("data/tidy/2020_south-west_stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()
length(unique(maxn.bruv$id))

#BOSS
maxn.boss <-read.csv("data/tidy/2020-2021_south-west_BOSS.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::glimpse()
length(unique(maxn.boss$id))

maxn <- bind_rows(maxn.bruv,maxn.boss)

# Metadata ----
#bruv
metadata.bruv <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()

#boss
metadata.boss <- read.csv("data/tidy/2020-2021_south-west_BOSS.checked.metadata.csv") %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(site=seq(1:279))%>%
  dplyr::mutate(site=paste(method,site,sep = ""))%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()

metadata <- bind_rows(metadata.bruv,metadata.boss)

#have a look at points in state waters
ggplot()+geom_point(data=metadata,aes(x=longitude,y=latitude,color = state.zone))+theme_classic()
#need to yeet all those in state water sanctuary zones

# Bathymetry derivatives ----
bathy <- read.csv('data/tidy/2020-2021_south-west_BOSS-BRUV.bathymetry.derivatives.csv') %>%      #from r/02-original gams/X_Get_bathy-derivatives.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv('data/tidy/2020-2021_south-west_BRUVs-BOSS.distance.to.ramp.csv') %>%            #from r/01_format data/Spatial/06_Get_distance_from_boat_ramps.R
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  dplyr::glimpse()

#habitat
habitat <- readRDS("data/tidy/dat.full.habitat.rds")%>%
  dplyr::select(1:23)%>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = "."))%>%
  glimpse()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,id,sample,method) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(total.abundance=rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,4:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(id,sample,total.abundance,species.richness,method) %>%
  tidyr::gather(.,"scientific","maxn",3:4) %>%
  dplyr::glimpse()

#check number of samples
testboss <- ta.sr %>%filter(method=="BOSS")                                     #558/2 = 279 ~ good
testbruv <- ta.sr %>%filter(method=="BRUV")                                     #622/2 = 311 ~ good

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
                                                       "Platycephalidae Leviprora spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp",
                                                       "Lethrinidae Gymnocranius spp"),"R",fishing.type))%>%
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
## Total frequency of occurrence
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
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",               #not many snapper will likely get excluded
                                  "Labridae Coris auricularis",
                                  "Scorpididae Neatypus obliquus",
                                  "Pomacentridae Chromis klunzingeri"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()%>%
  glimpse()

test.samples <- species.maxn %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n())

unique(species.maxn$scientific)
unique(maxn$scientific)

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(ta.sr)%>%                                            #removed all other taxa
  left_join(metadata) %>%                                                       #joins by id sample & method
  left_join(bathy) %>%                                                          #joins by id sample method & campaignid
  left_join(ramps) %>%                                                          #joins by id sample method & campaignid
  left_join(habitat) %>%                                                        #joins by id sample method & campaignid
  distinct()

#1180 observations
1180/2  #590 samples
279+311 #590 - we good

unique(combined.maxn$scientific)

# Set predictor variables---
pred.vars=c("depth", "slope", "detrended","aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated")

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
correlate(combined.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  filter(abs(cor) > 0.8)

#mean relief and reef are 0.93 correlated after adding boss data 
par(mfrow=c(1,1))
ggplot()+
  geom_point(data = combined.maxn,aes(sample,maxn),alpha = 0.2)+
  theme_classic()+facet_wrap(~scientific)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# remove samples with maxn >400? Prob an outlier and driving a lot of relationships
# outlier <- combined.maxn %>%
#   filter(maxn>400)%>%
#   glimpse()
#  
# combined.maxn <- combined.maxn %>%
#   filter(maxn <400)%>%
#   glimpse()


# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-combined.maxn[ ,i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# use 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# sponges - use non-transformed
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth - use non-transformed
# broad.reef - use non-transformed
# tpi - use non-transformed
# roughness - use non-transformed

# remove
# sand - correlated with reef and mean relief
# slope - correlated with roughness
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few

# Set predictor variables 
pred.vars=c("mean.relief","detrended","sd.relief","broad.macroalgae","broad.reef",
            "distance.to.ramp", "tpi","roughness","depth")

# Remove any unused columns from the dataset
dat.maxn <- combined.maxn %>%
  dplyr::filter(is.na(state.zone))%>%
  dplyr::select(sample, method,status, site, planned.or.exploratory, scientific, maxn,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth","detrended") %>%
  dplyr::filter(!sample%in%c("IO267"))%>%   #remove one weird TPI value (-11) come back to try and check on it
  as.data.frame()

saveRDS(dat.maxn, "data/tidy/dat.maxn.full.rds")

#####LENGTHS#####
# Bring in and format the data----
# Length ----
length <-read.csv("data/tidy/2020_south-west_stereo-BRUVs.complete.length.csv") %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

metadata.bruv <- metadata.bruv %>%
  dplyr::filter(successful.length%in%"Yes")%>%
  glimpse()

length(unique(length$sample)) #277

test <- length %>%
  filter(number>0)%>%
  distinct(sample)

total.no.pinkies <- length %>%
  dplyr::filter(species=="auratus") %>%
  filter(number>0)

sum(total.no.pinkies$number) # 225
test <- total.no.pinkies %>%
  filter(length>0)
sum(test$number) # 188 measured

188/225*100 # 84% measured

unique(master$fishing.type)

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
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
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
combined.length <- bind_rows(legal, sublegal) # removed all other taxa

unique(combined.length$scientific)

complete.length <- combined.length %>%
  #dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::right_join(metadata.bruv, by = c("sample")) %>% # add in all samples
  dplyr::select(campaignid,sample,scientific,number) %>%
  tidyr::complete(nesting(campaignid,sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata.bruv) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

test <- complete.length %>%
  group_by(sample)%>%
  summarise(n=n())

unique(complete.length$scientific)
length(unique(complete.length$sample))                                          #good still

# Remove any unused columns from the dataset 
dat.length <- complete.length%>%
  dplyr::filter(is.na(state.zone))%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, number,
                "mean.relief","sd.relief","broad.macroalgae","broad.reef",
                "distance.to.ramp", "tpi","roughness","depth","detrended") %>%
  dplyr::filter(!sample%in%c("IO267"))%>%   #remove one weird TPI value (-11) come back to try and check on it
  as.data.frame()

ggplot()+
  geom_point(data = dat.length,aes(sample,number),alpha = 0.2)+
  theme_classic()+facet_wrap(~scientific)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#a few potential outliers, leave them in for now

saveRDS(dat.length, "data/tidy/dat.length.full.rds")

