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
study<-"2020-2021_south-west_BOSS"

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
  dplyr::filter(campaignid %in% c("2020-10_south-west_BOSS","2021-03_West-Coast_BOSS"))%>%    # only include boss october and march samples here
  #dplyr::select(-c(project))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep=" "))%>%
  glimpse()
raw.metadata <- metadata                                                        #so we can do the status joining bit

### Have to be careful as we have 2 campaigns with overlapping sample names
unique(metadata$successful.count)
unique(metadata$campaignid)
names(metadata)

length(unique(metadata$id))                                                     # 279 (154 March, 125 october)
154+125                                                                         #good

double.ups <- metadata %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::filter(n>1)                                                            # No double ups

#join in state/commonwealth zone and fishing status to all metadata columns
#we already have this for 2021-03 but will just add again for all
# Spatial files ----
setwd(working.dir)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)

wa.marineparks <- readOGR(dsn="data/spatial/shapefiles/test1.shp")
proj4string(wa.marineparks)

proj4string(commonwealth.marineparks)<-CRS(wgs.84)
proj4string(wa.marineparks)<-CRS(wgs.84)

str(metadata)
metadata$latitude <- as.numeric(metadata$latitude)
metadata$longitude <- as.numeric(metadata$longitude)
coordinates(metadata) <- c('longitude','latitude')
proj4string(metadata)<-CRS(wgs.84)

metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks) %>%
  dplyr::select(ZoneName)

unique(metadata.commonwealth.marineparks$ZoneName)

metadata.state.marineparks <- over(metadata, wa.marineparks) %>%
  dplyr::select(Name)

unique(metadata.state.marineparks$Name)

names(metadata.commonwealth.marineparks)

metadata<-bind_cols(raw.metadata,metadata.commonwealth.marineparks)%>%
  bind_cols(.,metadata.state.marineparks)%>%
  dplyr::rename(Commonwealth.zone=ZoneName, State.zone=Name)%>%
  mutate(Status = if_else((Commonwealth.zone%in%c("National Park Zone")|
                             State.zone%in%c("Injidup Sanctuary Zone","Cape Freycinet Sanctuary Zone")),"No-take","Fished"))%>%
  dplyr::select(-c(status,commonwealth.zone,state.zone))%>%
  ga.clean.names()%>%
  glimpse()

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
  dplyr::filter(campaignid %in% c("2020-10_south-west_BOSS","2021-03_West-Coast_BOSS"))%>%        #filter for only BOSS march and october samples
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
length(unique(maxn$id))                                                         #264

no.fish <- anti_join(metadata,maxn)                                             #15

264+15                                                                          #279 = all good once i add back in no fish samples later

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

#we have no length data, so move on

setwd(working.dir)
