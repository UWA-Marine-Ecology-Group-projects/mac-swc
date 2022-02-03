#Claude has edited this and it is pretty poxy
#need to fix up

# Set directories----
rm(list=ls())

# Study name ----
study <- "2021-05_Abrolhos" 

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
boss <- read_csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.length.csv")%>%
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::filter(number>0)%>%
  glimpse()

boss.species <- boss %>%
  dplyr::select(sample,family,genus,species,location)%>%
  
  unique()%>%
  glimpse()

bruv <- read_csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.length.csv")%>%
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::filter(number>0)%>%
  glimpse()

bruv.species <- bruv %>%
  dplyr::select(sample,family,genus,species,location)%>%
  unique()%>%
  glimpse()

species.list <- bind_rows(bruv.species,boss.species)%>%
  dplyr::select(-sample)%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  unique()%>%
  glimpse()

num.fish <- bind_rows(bruv,boss)%>%
  summarise(fish = sum(number))

length(unique(species.list$scientific)) #90 species
length(unique(species.list$genus)) #55 genera
length(unique(species.list$family)) #35 families

# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

spp.species<-species.list%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- species.list %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%
  dplyr::select(location,scientific,fishing.type,australian.common.name)%>%
  glimpse()

npz6.fished <- fished.species %>%
  dplyr::filter(location%in%"NPZ6")%>%
  glimpse()

npz9.fished <- fished.species %>%
  dplyr::filter(location%in%"NPZ9")%>%
  glimpse()

write.csv(npz6.fished, file = "output/fssgam - fish/npz6-fished-species.csv")
write.csv(npz9.fished, file = "output/fssgam - fish/npz9-fished-species.csv")

iucn.species <- species.list %>%
  dplyr::left_join(master) %>%
  dplyr::filter(!is.na(iucn.ranking))%>%
  dplyr::filter(!iucn.ranking%in%c("Least Concern", "Data Deficient")) %>%
 glimpse()

# mass <- read.csv("data/tidy/montebello.synthesis.complete.mass.csv")%>%
#   glimpse()
# 
# # Read in metadata ----
# metadata<-read_csv("data/tidy/montebello.synthesis.checked.metadata.csv")%>%
#   dplyr::select(sample,latitude,longitude,date,time,depth)
# 
# 
# 
# names(master)
# 
# length(unique(metadata$sample))
# 
# total.number.fish <- sum(maxn$maxn) # 13901
# # total.number.measured <- length%>%
# #   filter(length>0)
# # sum(total.number.measured$number) # 7575
# # 7575/13596 # 55%
# 
# # total.measurable <- maxn%>%filter(!successful.length%in%c("No"))%>%filter(successful.count%in%c("Yes"))
# # sum(total.measurable$maxn)
# # 
# # no.lengths <- length %>% filter(number>0)
# # videos.not.measured <- anti_join(metadata,no.lengths, by = c("sample", "latitude", "longitude"))
# # 
# # fish.to.measure <- semi_join(maxn,videos.not.measured)
# # sum(fish.to.measure$maxn)
# 
# 
# ###### NEED TO READ IN LUMPED COMMON NAMES FOR PSEUDOCARANX
# ###### WILL ALSO NEED TO ADD INTO CHECKEM AND VISUALISER!!!!!!!
# 
# # Create Species list ----
# species.table <- maxn%>%
#   group_by(family,genus,species,scientific)%>%
#   summarise_at(vars("maxn"),funs(sum,mean,sd,se=sd(.)/sqrt(n())))%>%
#   ungroup()%>%
#   mutate(mean=round(mean,digits=2))%>%
#   mutate(sd=round(sd,digits=2))%>%
#   mutate(se=round(se,digits=2))%>%
#   mutate(genus.species=paste(genus,species,sep=" "))%>%
#   arrange(family)%>%
#   left_join(master)%>%
#   dplyr::select(-c(scientific))%>%
#   dplyr::mutate(mean.relative.abundance.per.deployment.plus.minus.SE=paste(mean,"+/-",se,sep=" "))%>%
#   dplyr::rename(total.relative.abundance = sum)%>%
#   ungroup()
# 
# unique(species.table$fishing.type)
# 
# ubiquity <- maxn%>%
#   filter(maxn>0) %>%
#   group_by(family,genus,species,scientific)%>%
#   summarise(no.of.deployments=n())%>%
#   ungroup() %>%
#   mutate(ubiquity=(no.of.deployments/200)*100)
# 
# cleaned<-species.table%>%
#   dplyr::select(family,genus.species,australian.common.name,fishing.type,iucn.ranking)%>%
#   ## fix up variables
#   mutate(fishing.type=ifelse(fishing.type%in%c("C/R","C","B/C"),"Commercial","")) %>%
#   dplyr::filter(iucn.ranking %in% c('Near Threatened', "Endangered", "Critically Endangered", "Vulnerable"))
#   # left_join(arch)
# ## Make names nicer for table
# write.csv(cleaned,"data/tidy/endangered.species.table.csv")
# 
