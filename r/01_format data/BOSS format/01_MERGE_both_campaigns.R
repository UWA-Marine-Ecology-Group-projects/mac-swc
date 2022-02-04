### Merge EventMeasure database output tables into maxn and length files

### OBJECTIVES ###
# combine database tables into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to GitHub
library(RCurl)
library(rgdal)
library(R.utils)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# to connect to googlesheets
library(googlesheets4)
library(sp)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"2021-03_West-Coast_BOSS"

## Folder Structure ----
# This script uses one main folder ('working directory')
# Three subfolders will be created within the 'working directory'. They are 'EM Export','Staging' and 'Tidy data'
# Save the database exports into the 'EM Export' folder
# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)

# Naming in the script is extremely important! Names need to be consistent or the script will break

# **The only folder you will need to create is your working directory**

## Set your working directory ----
working.dir <- getwd() # to directory of current file - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"data",sep="/")

download.dir<-paste(data.dir,"raw/em export",sep="/")

tidy.dir<-paste(data.dir,"tidy",sep="/")
staging.dir<-paste(data.dir,"staging",sep="/") 

setwd(download.dir)

# Combine all data----
# Metadata ----
metadata <- ga.list.files("_Metadata.csv") %>%                                  # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_em.csv(.)) %>%                                   # combine into dataframe
  dplyr::select(campaignid, sample, dataset, planned.or.exploratory, latitude, 
                longitude, date, time, location, status, site, depth, observer, 
                successful.count, successful.length, commonwealth.zone, 
                state.zone,raw.hdd.number,con.hdd.number)%>% 
  dplyr::mutate(sample=as.character(sample)) %>%
  dplyr::filter(successful.count%in%c("Yes"))%>%
  dplyr::filter(campaignid %in% paste(study))%>%                                # only include boss samples here
  #dplyr::select(-c(project))%>%                                                
  glimpse()

unique(metadata$successful.count)

names(metadata)

unique(metadata$campaignid)                                                     # check the number of campaigns in metadata, and the campaign name
length(unique(metadata$sample))                                                 # 154 - all good

double.ups <- metadata %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::filter(n>1)                                                            # No double ups

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
setwd(download.dir)
points.files <-ga.list.files("_Points.txt")                                     # list all files ending in "_Points.txt"
points.files$lines<-sapply(points.files,countLines)                             # Count lines in files (to avoid empty files breaking the script)

points<-as.data.frame(points.files)%>%
  dplyr::mutate(campaign=row.names(.))%>%
  filter(lines>1)%>%                                                            # filter out all empty text files
  dplyr::select(campaign)%>%
  as_vector(.)%>%                                                               # remove all empty files
  purrr::map_df(~ga.read.files_txt(.))%>%
  dplyr::mutate(campaignid=str_replace_all(.$project,c("_Points.txt"="")))%>%
  dplyr::filter(campaignid%in%paste(study))%>%                                  #filter for only BOSS samples
  dplyr::select(-c(project),-sample)%>%                                         #remove sample column - multiple drops in one .emob
  dplyr::rename(sample=period)%>%                                               #rename period as sample
  glimpse()

unique(points$campaignid)

maxn<-points%>%
  dplyr::select(-c(time)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid,sample,filename,periodtime,frame,family,
                  genus,species)%>%                                             # removed comment from here
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::ungroup() %>%
  dplyr::left_join(metadata)%>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>%           # remove any NAs in taxa name
  dplyr::filter(!family%in%c("Unknown"))%>%
  dplyr::filter(successful.count%in%"Yes")%>%
  dplyr::ungroup()                   

unique(maxn$successful.count)
length(unique(maxn$sample))                                                     #154

no.fish <- anti_join(metadata,maxn)                                             #all good

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

#we have no length data, so move on

setwd(working.dir)
