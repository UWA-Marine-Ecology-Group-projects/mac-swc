# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") # run everytime to check for updates
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "2020-10_south-west_stereo-BRUVs_BRUV_style"  # use this study name to run the example, or change to your own (you will need to save your files with this prefix - see naming of example files)

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
raw.dir<-paste(working.dir,"Data/Habitat/BRUV Style annotation/raw data",sep="/")
tidy.dir<-paste(working.dir,"Data/Tidy",sep="/")


# Read in the data----
setwd(raw.dir)
dir()

# Read in metadata----
metadata <- read_csv("2020_10_SW_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

# Read in habitat ----
setwd(raw.dir)
dir()

habitat <- read.delim("2020-10_south-west_stereo_BRUVS_Habitat_BRUV_Syle_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  select(sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview

# Check number of points per image ----
number.of.annotations<-habitat%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(number.of.annotations=n()) # count the number of annotations per image

wrong.number<-number.of.annotations%>%
  filter(!number.of.annotations==40) # see images where there is too many or too little annotations (in this example there are none), go back into the *.TMObs file to fix this before re-exporting DO NOT FIX IN THE TXT FILE

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat


habitat$ID<-paste(habitat$sample,habitat$image.row,habitat$image.col)

unique(habitat$ID)%>%sort()

# Create %fov----
fov<-habitat%>%
  tibble::rowid_to_column()%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, ID))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()

# Create relief----
relief<-habitat%>%
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

# CREATE catami_broad------
broad<-habitat%>%
  dplyr::select(-c(fieldofview,morphology,type,relief))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tibble::rowid_to_column()%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,ID))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>% # Kye had left row sums to be included in the calc
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  ## this errors but still seems to work
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse

# CREATE catami_morphology------
detailed<-habitat%>%
  dplyr::select(-c(fieldofview,relief))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="")))%>%
  dplyr::select(-c(broad))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tibble::rowid_to_column()%>%
  tidyr::spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,ID))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_if(is.numeric,sum,na.rm=TRUE)%>% 
  dplyr::mutate(Total.Sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("detailed"))%>%
  dplyr::select(-Total.Sum)%>%
  glimpse()


# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad <- metadata%>%
  left_join(fov,by="sample")%>%
  left_join(relief,by="sample")%>%
  left_join(broad,by="sample")

habitat.detailed <- metadata%>%
  left_join(fov,by="sample")%>%
  left_join(relief,by="sample")%>%
  left_join(detailed,by="sample")
  
write.csv(habitat.broad,file=paste(study,"broad.habitat.csv",sep = "."), row.names=FALSE)
write.csv(habitat.detailed,file=paste(study,"detailed.habitat.csv",sep = "."), row.names=FALSE)
