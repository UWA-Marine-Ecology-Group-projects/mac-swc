# Set directories----
rm(list=ls())

# Study name ----
study <- "2020_south-west_stereo-BRUVs" 

# Libraries required
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)

## Set your working directory ----
working.dir <- "Z:/SWC-multibeam-GAMMS"

# Set sub directories----
plots.dir=paste(working.dir,"Plots",sep="/")
tidy.dir=paste(working.dir,"Data/Tidy",sep="/")

# Read in maxn ----
setwd(tidy.dir)
dir()

maxn <- read_csv("2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  glimpse()

length <-read.csv("2020_south-west_stereo-BRUVs.complete.length.csv")

# Read in metadata ----
metadata<-read_csv("2020_south-west_stereo-BRUVs.checked.metadata.csv")%>%
  dplyr::select(sample,latitude,longitude,date,time,depth)

# Read in life history
setwd(tidy.dir)

master <- read.csv("australia.life.history.csv") %>% 
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name)%>% 
  distinct()%>%
  glimpse()

names(master)

unique(metadata$sample)

total.number.fish <- sum(maxn$maxn) # 13901
total.number.measured <- length%>%
  filter(length>0)
sum(total.number.measured$number) # 7575  # Em gets 8452
7575/13596 # 55%

total.measurable <- maxn%>%filter(!successful.length%in%c("No"))%>%filter(successful.count%in%c("Yes"))
sum(total.measurable$maxn)

no.lengths <- length %>% filter(number>0)
videos.not.measured <- anti_join(metadata,no.lengths, by = c("sample", "latitude", "longitude"))

fish.to.measure <- semi_join(maxn,videos.not.measured)
sum(fish.to.measure$maxn)


###### NEED TO READ IN LUMPED COMMON NAMES FOR PSEUDOCARANX
###### WILL ALSO NEED TO ADD INTO CHECKEM AND VISUALISER!!!!!!!

# Create Species list ----
species.table <- maxn%>%
  group_by(family,genus,species,scientific)%>%
  summarise_at(vars("maxn"),funs(sum,mean,sd,se=sd(.)/sqrt(n())))%>%
  ungroup()%>%
  mutate(mean=round(mean,digits=2))%>%
  mutate(sd=round(sd,digits=2))%>%
  mutate(se=round(se,digits=2))%>%
  mutate(genus.species=paste(genus,species,sep=" "))%>%
  arrange(family)%>%
  left_join(master)%>%
  dplyr::select(-c(scientific))%>%
  dplyr::mutate(mean.relative.abundance.per.deployment.plus.minus.SE=paste(mean,"+/-",se,sep=" "))%>%
  dplyr::rename(total.relative.abundance = sum)%>%
  ungroup()
unique(species.table$fishing.type)

ubiquity <- maxn%>%
  filter(maxn>0) %>%
  group_by(family,genus,species,scientific)%>%
  summarise(no.of.deployments=n())%>%
  ungroup() %>%
  mutate(ubiquity=(no.of.deployments/295)*100)


#arch <-read.csv("archytype.list.csv") %>%
#  mutate(genus.species=sp.names) %>%
#  dplyr::select(genus.species,archetype)



cleaned<-species.table%>%
  dplyr::select(family,genus.species,australian.common.name,fishing.type,iucn.ranking,mean.relative.abundance.per.deployment.plus.minus.SE,total.relative.abundance)%>%
  ## fix up variables
  mutate(fishing.type=ifelse(fishing.type%in%c("C/R","C","B/C"),"Commercial","")) %>%
  glimpse()
## Make names nicer for table


unique(cleaned$fishing.type)

dir()
write.csv(cleaned,"species.table.csv")

# Descriptive stats

# total abundance # these will all go down BG 2/2/21
sum(maxn$maxn) # 13596
length(unique(maxn$scientific)) # 147
length(unique(maxn$family)) # 63 genus
unique(maxn$sample)

king.wrasse <- maxn%>%
  filter(species%in%c("auricularis"))
sum(king.wrasse$maxn)

sweep <- maxn%>%
  filter(species%in%c("obliquus"))
sum(sweep$maxn)

puller <- maxn%>%
  filter(species%in%c("klunzingeri"))%>%
  filter(genus%in%c("Chromis"))
sum(puller$maxn)

# # Make data for anita ----
# summary<-maxn%>%
#   #filter(species%in%c("bathybius","carpenteri","tabl","equula","virgatus","variegatus"))%>%
#   group_by(sample,scientific)%>%
#   dplyr::summarise_at(vars("maxn"),funs(sum))%>%
#   mutate(presence=as.integer(maxn != 0))
# 
# presence<-summary%>%
#   dplyr::select(-c(maxn))%>%
#   spread(scientific,value=presence)
# 
# maxn.summary<-summary%>%
#   dplyr::select(-c(presence))%>%
#   spread(scientific,value=maxn)
# 
# ningaloo.presence<-left_join(metadata,habitat)%>%
#   left_join(presence)
# 
# ningaloo.maxn<-left_join(metadata,habitat)%>%
#   left_join(maxn.summary)
# 
# setwd(tidy.dir)
# write.csv(ningaloo.presence,"presence.spatial.model.csv",row.names = FALSE)
# write.csv(ningaloo.maxn,"maxn.spatial.model.csv",row.names = FALSE)