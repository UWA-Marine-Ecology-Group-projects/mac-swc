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
study <- "2020-10_south-west_stereo-BRUVS"  

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
metadata <- read_csv("2020-10_south-west_stereo-BRUVs_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the points annotations ----
habitat.forwards.points <- read.delim("2020-10_south-west_stereo-BRUVs_random-points_forwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""," take 2"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

forwards.no.annotations <- habitat.forwards.points%>%
  group_by(sample)%>%
  summarise(forwards.no.annotations=n())

habitat.backwards.points <- read.delim("2020-10_south-west_stereo-BRUVs_random-points_backwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

backwards.no.annotations <- habitat.backwards.points%>%
  group_by(sample)%>%
  summarise(backwards.no.annotations=n())

habitat.points <- bind_rows(habitat.forwards.points,habitat.backwards.points)

# Point to images folders ----
forwards.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/Habitat Images/Forwards"
backwards.dir <- "Y:/Project Folders/2020-10_south-west_stereo_BRUVs_BOSS/Working/Video Analysis/Habitat Images/Backwards"

forwards.list <- dir(forwards.dir)%>%as.data.frame()%>%rename(forwards.image.name=1)%>%
  mutate(sample=str_replace_all(.$forwards.image.name,c(".png"="",".jpg"="",".JPG"="")))

backwards.list <- dir(backwards.dir)%>%as.data.frame()%>%rename(backwards.image.name=1) %>%
  dplyr::filter(!backwards.image.name %in%c("Thumbs.db")) %>%
  mutate(sample=str_replace_all(.$backwards.image.name,c(".png"="",".jpg"="",".JPG"="")))

# Create checking dataframe ----
qaqc <- metadata %>%
  dplyr::select(sample, date) %>%
  dplyr::left_join(forwards.list)%>%
  dplyr::left_join(forwards.no.annotations)%>%
  dplyr::left_join(backwards.list)%>%
  dplyr::left_join(backwards.no.annotations)%>%
  glimpse()

# Find samples where images and annotations are missing:
forwards.missing.image.not.annotated <- qaqc%>%filter(forwards.image.name%in%c("NA",NA))%>%filter(forwards.no.annotations%in%c("NA",NA))
backwards.missing.image.not.annotated <- qaqc%>%filter(backwards.image.name%in%c("NA",NA))%>%filter(backwards.no.annotations%in%c("NA",NA))

# Find samples where image is exported but missing annotations:
forwards.missing.annotation <- qaqc%>%filter(!forwards.image.name%in%c("NA",NA))%>%filter(forwards.no.annotations%in%c("NA",NA))
backwards.missing.annotation <- qaqc%>%filter(!backwards.image.name%in%c("NA",NA))%>%filter(backwards.no.annotations%in%c("NA",NA))

# Find samples annotated but missing images:
forwards.missing.image <- qaqc%>%filter(forwards.image.name%in%c("NA",NA))%>%filter(!forwards.no.annotations%in%c("NA",NA))
backwards.missing.image <- qaqc%>%filter(backwards.image.name%in%c("NA",NA))%>%filter(!backwards.no.annotations%in%c("NA",NA))

setwd(error.dir)
dir()

write.csv(qaqc, paste(study,"random-points","images-and-annotations-missing.csv",sep="_"),row.names=FALSE)  



### USE THIS ONLY ONCE ###
setwd("C:/GitHub/SWC gits/mac-swc/data/raw/habitat checking")

dir()

labsheet <- read.csv("MEG_Labsheets_2021 - 2020-10_south-west_BOSS.csv")

names(qaqc)

annotated <- qaqc%>%
  
  dplyr::rename(forwards.habitat.forwards.exported = forwards.image.name)%>%
  dplyr::rename(forwards.habitat.backwards.exported = backwards.image.name)%>%
  
  dplyr::rename(forwards.habitat.forwards.annotated = forwards.points.annotated)%>%
  dplyr::rename(forwards.habitat.backwards.annotated = backwards.points.annotated)%>%
  
  dplyr::select(-c(date))%>%
  
  dplyr::mutate(forwards.habitat.forwards.exported = ifelse(!forwards.habitat.forwards.exported%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.backwards.exported = ifelse(!forwards.habitat.backwards.exported%in%c(NA),"Yes",NA)) %>%
  
  dplyr::mutate(forwards.habitat.forwards.annotated = ifelse(!forwards.habitat.forwards.annotated%in%c(NA),"Yes",NA)) %>%
  dplyr::mutate(forwards.habitat.backwards.annotated = ifelse(!forwards.habitat.backwards.annotated%in%c(NA),"Yes",NA)) %>%
  
  dplyr::rename(Sample = sample) %>%
  
  glimpse()


join <- left_join(labsheet,annotated)  

write.csv(join,"new-labsheet.csv",row.names = FALSE)                


