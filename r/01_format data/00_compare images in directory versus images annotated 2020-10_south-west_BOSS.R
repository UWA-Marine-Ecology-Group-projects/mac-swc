# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "2020-10_south-west_BOSS"  

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
tidy.dir <- paste(data.dir,"tidy",sep="/")
tm.export.dir <- paste(raw.dir,"tm export",sep="/") 
em.export.dir <- paste(raw.dir, "em export", sep = "/")
error.dir <- paste(data.dir,"errors to check",sep="/") 

# Read in the metadata----
setwd(em.export.dir)
dir()

# Read in metadata----
metadata <- read_csv("2020-10_south-west_BOSS_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, time.bottom, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

june.points <- read.delim("20201119_Multibeamed_BOSSstyle_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""
                                             ,"N"="","E"="","S"="","W"=""
  ))) %>%
  mutate(sample=as.character(sample)) %>%
  dplyr::select(sample,filename,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  mutate(direction=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="", '[0-9]+'="","J"="")))%>% # need this line as the june drops were annotated differently
  glimpse() # preview

length(unique(june.points$sample)) # 37

june.no.annotations <- june.points%>%
  group_by(sample)%>%
  summarise(june.points.annotated=n())

june.north <- june.points%>%
  filter(direction %in%c("N"))%>%
  group_by(sample)%>%
  summarise(north.points.annotated=n())

june.east <- june.points%>%
  filter(direction %in%c("E"))%>%
  group_by(sample)%>%
  summarise(east.points.annotated=n())

june.south <- june.points%>%
  filter(direction %in%c("S"))%>%
  group_by(sample)%>%
  summarise(south.points.annotated=n())

june.west <- june.points%>%
  filter(direction %in%c("W"))%>%
  group_by(sample)%>%
  summarise(west.points.annotated=n())

# read in the points annotations ----
north.points <- read.delim("2020-10_south-west_BOSS_north_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","N"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,filename,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(north.points$sample)) # 168 samples

north.no.annotations <- north.points%>%
  group_by(sample)%>%
  summarise(north.points.annotated=n())%>%
  bind_rows(june.north)

test <- north.points %>%
  filter(broad%in%c("",NA))

east.points <- read.delim("2020-10_south-west_BOSS_east_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","E"="","RDO"="REDO"))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,filename,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(east.points$sample)) # 169

east.no.annotations <- east.points%>%
  group_by(sample)%>%
  summarise(east.points.annotated=n())%>%
  bind_rows(june.east)

test <- east.points %>%
  filter(broad%in%c("",NA))

south.points <- read.delim("2020-10_south-west_BOSS_south_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","S"="","256"="356"))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,filename,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(south.points$sample)) # 169

south.no.annotations <- south.points%>%
  group_by(sample)%>%
  summarise(south.points.annotated=n())%>%
  bind_rows(june.south)

test <- south.points %>%
  filter(broad%in%c("",NA))

west.points <- read.delim("2020-10_south-west_BOSS_west_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","W"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,filename,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(west.points$sample)) # 169

west.no.annotations <- west.points%>%
  group_by(sample)%>%
  summarise(west.points.annotated=n())%>%
  bind_rows(june.west)

# Point to images folders ----
north.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/BOSS/Habitat Images/2020-10/North"
east.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/BOSS/Habitat Images/2020-10/East"
south.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/BOSS/Habitat Images/2020-10/South"
west.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/BOSS/Habitat Images/2020-10/West"

north.list <- dir(north.dir)%>%as.data.frame()%>%rename(north.image.name=1)%>%
  filter(!north.image.name%in%c("2020_10_SouthWest_BOSS_Habitat_North.TMObs", "2020_10_SouthWest_BOSS_Habitat_North.TMObs_AUTO"))%>%
  mutate(sample=str_replace_all(.$north.image.name,c(".png"="",".jpg"="",".JPG"="","N"="")))

east.list <- dir(east.dir)%>%as.data.frame()%>%rename(east.image.name=1)%>%
  filter(!east.image.name%in%c("2020_10_SouthWest_BOSS_Habitat_East.TMObs", "2020_10_SouthWest_BOSS_Habitat_East.TMObs_AUTO"))%>%
  mutate(sample=str_replace_all(.$east.image.name,c(".png"="",".jpg"="",".JPG"="","E"="")))

south.list <- dir(south.dir)%>%as.data.frame()%>%rename(south.image.name=1)%>%
  filter(!south.image.name%in%c("2020_10_SouthWest_BOSS_Habitat_South.TMObs", "2020_10_SouthWest_BOSS_Habitat_South.TMObs_AUTO"))%>%
  mutate(sample=str_replace_all(.$south.image.name,c(".png"="",".jpg"="",".JPG"="","S"="")))

west.list <- dir(west.dir)%>%as.data.frame()%>%rename(west.image.name=1)%>%
  filter(!west.image.name%in%c("2020_10_SouthWest_BOSS_Habitat_West.TMObs", "2020_10_SouthWest_BOSS_Habitat_West.TMObs_AUTO"))%>%
  mutate(sample=str_replace_all(.$west.image.name,c(".png"="",".jpg"="",".JPG"="","W"="")))


# Create checking dataframe for North ----
qaqc <- metadata %>%
  dplyr::select(sample, date, time.bottom) %>%
  dplyr::left_join(north.list)%>%
  dplyr::left_join(north.no.annotations)%>%
  dplyr::left_join(east.list)%>%
  dplyr::left_join(east.no.annotations)%>%
  dplyr::left_join(south.list)%>%
  dplyr::left_join(south.no.annotations)%>%
  dplyr::left_join(west.list)%>%
  dplyr::left_join(west.no.annotations)%>%
  glimpse()

# Find samples where images and annotations are missing:
north.missing.image.not.annotated <- qaqc%>%filter(north.image.name%in%c("NA",NA))%>%filter(north.points.annotated%in%c("NA",NA))
east.missing.image.not.annotated <- qaqc%>%filter(east.image.name%in%c("NA",NA))%>%filter(east.points.annotated%in%c("NA",NA))
south.missing.image.not.annotated <- qaqc%>%filter(south.image.name%in%c("NA",NA))%>%filter(south.points.annotated%in%c("NA",NA))
west.missing.image.not.annotated <- qaqc%>%filter(west.image.name%in%c("NA",NA))%>%filter(west.points.annotated%in%c("NA",NA))

# Find samples where image is exported but missing annotations:
north.missing.annotation <- qaqc%>%filter(!north.image.name%in%c("NA",NA))%>%filter(north.points.annotated%in%c("NA",NA))
east.missing.annotation <- qaqc%>%filter(!east.image.name%in%c("NA",NA))%>%filter(east.points.annotated%in%c("NA",NA))
south.missing.annotation <- qaqc%>%filter(!south.image.name%in%c("NA",NA))%>%filter(south.points.annotated%in%c("NA",NA))
west.missing.annotation <- qaqc%>%filter(!west.image.name%in%c("NA",NA))%>%filter(west.points.annotated%in%c("NA",NA))

# Find samples annotated but missing images:
north.missing.image <- qaqc%>%filter(north.image.name%in%c("NA",NA))%>%filter(!north.points.annotated%in%c("NA",NA))
east.missing.image <- qaqc%>%filter(east.image.name%in%c("NA",NA))%>%filter(!east.points.annotated%in%c("NA",NA))
south.missing.image <- qaqc%>%filter(south.image.name%in%c("NA",NA))%>%filter(!south.points.annotated%in%c("NA",NA))
west.missing.image <- qaqc%>%filter(west.image.name%in%c("NA",NA))%>%filter(!west.points.annotated%in%c("NA",NA))

setwd(error.dir)
dir()

write.csv(qaqc, paste(study,"random-points","images-and-annotations-missing.csv",sep="_"),row.names=FALSE)  



### USE THIS ONLY ONCE ###
setwd("C:/GitHub/SWC gits/mac-swc/data/raw/habitat checking")

dir()

labsheet <- read.csv("MEG_Labsheets_2021 - 2020-10_south-west_BOSS.csv")

names(qaqc)

annotated <- qaqc%>%
  
  dplyr::rename(forwards.habitat.north.exported = north.image.name)%>%
  dplyr::rename(forwards.habitat.east.exported = east.image.name)%>%
  dplyr::rename(forwards.habitat.south.exported = south.image.name)%>%
  dplyr::rename(forwards.habitat.west.exported = west.image.name)%>%
  
  dplyr::rename(forwards.habitat.north.annotated = north.points.annotated)%>%
  dplyr::rename(forwards.habitat.east.annotated = east.points.annotated)%>%
  dplyr::rename(forwards.habitat.south.annotated = south.points.annotated)%>%
  dplyr::rename(forwards.habitat.west.annotated = west.points.annotated)%>%
  
  dplyr::select(-c(date,time.bottom))%>%
  
  dplyr::mutate(forwards.habitat.north.exported = ifelse(!forwards.habitat.north.exported%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.east.exported = ifelse(!forwards.habitat.east.exported%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.south.exported = ifelse(!forwards.habitat.south.exported%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.west.exported = ifelse(!forwards.habitat.west.exported%in%c(NA),"Yes",NA)) %>%
  
  dplyr::mutate(forwards.habitat.north.annotated = ifelse(!forwards.habitat.north.annotated%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.east.annotated = ifelse(!forwards.habitat.east.annotated%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.south.annotated = ifelse(!forwards.habitat.south.annotated%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.west.annotated = ifelse(!forwards.habitat.west.annotated%in%c(NA),"Yes",NA)) %>%
  
  dplyr::rename(Sample = sample) %>%
  
  glimpse()


join <- left_join(labsheet,annotated)  

write.csv(join,"new-labsheet.csv",row.names = FALSE)                
       
                
                