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

# read in the points annotations ----
north.points <- read.delim("2020-10_south-west_BOSS_north_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","N"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(north.points$sample)) # 168 samples

north.no.annotations <- north.points%>%
  group_by(sample)%>%
  summarise(north.points.annotated=n())

test <- north.points %>%
  filter(broad%in%c("",NA))

east.points <- read.delim("2020-10_south-west_BOSS_east_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","E"="","RDO"="REDO"))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(east.points$sample)) # 169

east.no.annotations <- east.points%>%
  group_by(sample)%>%
  summarise(east.points.annotated=n())

test <- east.points %>%
  filter(broad%in%c("",NA))

south.points <- read.delim("2020-10_south-west_BOSS_south_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","S"="","256"="356"))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(south.points$sample)) # 169

south.no.annotations <- south.points%>%
  group_by(sample)%>%
  summarise(south.points.annotated=n())

test <- south.points %>%
  filter(broad%in%c("",NA))

west.points <- read.delim("2020-10_south-west_BOSS_west_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","W"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

length(unique(west.points$sample)) # 169

west.no.annotations <- west.points%>%
  group_by(sample)%>%
  summarise(west.points.annotated=n())

test <- west.points %>%
  filter(broad%in%c("",NA))

habitat.points <- bind_rows(north.points,east.points,west.points,south.points)

# Check number of points per image ----
number.of.annotations <- habitat.points%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(number.of.annotations=n()) # count the number of annotations per image

wrong.number<-number.of.annotations%>%
  filter(!number.of.annotations==80) # see images where there is too many or too little annotations (in this example there are none), go back into the *.TMObs file to fix this before re-exporting DO NOT FIX IN THE TXT FILE

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat.points,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat.points, by = c("sample")) # samples in the metadata that don't have a match in habitat

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
  dplyr::left_join(west.no.annotations)

setwd(error.dir)
dir()

write.csv(qaqc, paste(study,"random-points","images-and-annotations-missing.csv",sep="_"),row.names=FALSE)  

# Create %fov----
fov.points <- habitat.points%>%
  dplyr::select(-c(broad,morphology,type))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(fov.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()

# CREATE catami_broad------
broad.points <- habitat.points%>%
  dplyr::select(-c(fieldofview,morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse

# CREATE catami_morphology------
detailed.points <- habitat.points%>%
  dplyr::select(-c(fieldofview))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
  dplyr::select(-c(broad,type))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

# Create relief----
relief.grid<-habitat.grid%>%
  dplyr::filter(!broad%in%c("Open Water","Unknown"))%>%
  dplyr::filter(!relief%in%c(""))%>%
  dplyr::select(-c(broad,morphology,type,fieldofview,image.row,image.col))%>%
  dplyr::mutate(relief.rank=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,
                                   ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                                          ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                                                 ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,
                                                        ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                                                               ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  dplyr::select(-c(relief))%>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()


# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(broad.points, by = "sample")

habitat.detailed.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(detailed.points, by = "sample")

write.csv(habitat.broad.points,file=paste(study,"random-points_broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.points,file=paste(study,"random-points_detailed.habitat.csv",sep = "_"), row.names=FALSE)
