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
  dplyr::select(sample, latitude, longitude, date, time, site, location, successful.count,habitat.backwards.image.saved) %>% # select only these columns to keep
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

length(unique(habitat.forwards.points$sample)) # 273 samples

no.annotations <- habitat.forwards.points%>%
  group_by(sample)%>%
  dplyr::summarise(n=n())

test <- habitat.forwards.points %>%
  filter(broad%in%c("",NA))

habitat.backwards.points <- read.delim("2020-10_south-west_stereo-BRUVs_random-points_backwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

length(unique(habitat.backwards.points$sample)) # 260

no.annotations <- habitat.backwards.points%>%
  group_by(sample)%>%
  dplyr::summarise(n=n())

test <- habitat.backwards.points %>%
  filter(broad%in%c("",NA))

habitat.points <- bind_rows(habitat.forwards.points,habitat.backwards.points)

# Check number of points per image ----
number.of.annotations <- habitat.points%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(number.of.annotations=n()) # count the number of annotations per image

wrong.number<-number.of.annotations%>%
  filter(!number.of.annotations==40) # see images where there is too many or too little annotations (in this example there are none), go back into the *.TMObs file to fix this before re-exporting DO NOT FIX IN THE TXT FILE

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat.points,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat.points, by = c("sample")) # samples in the metadata that don't have a match in habitat

forwards.missing <- anti_join(metadata, habitat.forwards.points, by = c("sample"))%>% 
  filter(successful.count%in%c("Yes"))

setwd(error.dir)

write.csv(forwards.missing, "2020-10_south-west_stereo-BRUV_random-points_forwards_missing.csv",row.names = FALSE)

backwards.missing <- anti_join(metadata, habitat.backwards.points, by = c("sample"))%>% 
  filter(successful.count%in%c("Yes"))

# read in grid annotations ----
setwd(tm.export.dir)
dir()

habitat.forwards.grid <- read.delim("2020-10_south-west_stereo_BRUVs_Habitat_grid_forwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""," take 2"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

length(unique(habitat.forwards.grid$sample)) # 273 samples

no.annotations <- habitat.forwards.grid%>%
  group_by(sample)%>%
  dplyr::summarise(n=n()) # good

test <- habitat.forwards.grid %>%
  filter(broad%in%c("",NA)) # good

habitat.backwards.grid <- read.delim("2020-10_south-west_stereo_BRUVs_Habitat_grid_backwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

length(unique(habitat.backwards.grid$sample)) # 260 samples

no.annotations <- habitat.backwards.grid%>%
  group_by(sample)%>%
  dplyr::summarise(n=n()) # good

test <- habitat.backwards.grid %>%
  filter(broad%in%c("",NA)) # good

habitat.grid <- bind_rows(habitat.forwards.grid,habitat.backwards.grid)

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat.grid,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat.grid, by = c("sample")) # samples in the metadata that don't have a match in habitat

forwards.missing <- anti_join(metadata, habitat.forwards.grid, by = c("sample"))%>% 
  filter(successful.count%in%c("Yes"))

setwd(error.dir)

write.csv(forwards.missing, "2020-10_south-west_stereo-BRUV_grid_forwards_missing.csv",row.names = FALSE)
backwards.missing <- anti_join(metadata, habitat.backwards.grid, by = c("sample"))%>% 
  filter(successful.count%in%c("Yes"))

# Create %fov----
fov.points <- habitat.points%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
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

fov.grid <- habitat.grid%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  tibble::rowid_to_column()%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col,rowid))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(fov.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()

# CREATE catami_broad------
broad.points <- habitat.points%>%
  dplyr::select(-c(fieldofview,morphology,type,relief))%>%
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

broad.grid <- habitat.grid%>%
  dplyr::select(-c(fieldofview,morphology,type,relief))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tibble::rowid_to_column()%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,rowid))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse

# CREATE catami_morphology------
detailed.points <- habitat.points%>%
  dplyr::select(-c(fieldofview,relief))%>%
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

detailed.grid <- habitat.grid%>%
  dplyr::select(-c(fieldofview,relief))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"=".","sand.."="sand.","Sand . mud "="Sand.mud","Sand.mud .c"="Sand.mud.c")))%>%
  dplyr::select(-c(broad,type))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tibble::rowid_to_column()%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,rowid))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

names(detailed.grid)

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
  left_join(broad.points, by = "sample")%>%
  left_join(relief.grid)

habitat.detailed.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(detailed.points, by = "sample")%>%
  left_join(relief.grid)

habitat.broad.grid <- metadata%>%
  left_join(fov.grid, by = "sample")%>%
  left_join(broad.grid, by = "sample")%>%
  left_join(relief.grid)

habitat.detailed.grid <- metadata%>%
  left_join(fov.grid, by = "sample")%>%
  left_join(detailed.grid, by = "sample")%>%
  left_join(relief.grid)

write.csv(habitat.broad.points,file=paste(study,"random-points_broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.broad.grid,file=paste(study,"grid_broad.habitat.csv",sep = "_"), row.names=FALSE)

write.csv(habitat.detailed.points,file=paste(study,"random-points_detailed.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.grid,file=paste(study,"grid_detailed.habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
