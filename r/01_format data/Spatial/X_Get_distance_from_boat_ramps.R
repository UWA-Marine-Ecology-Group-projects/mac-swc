###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Check predictors and combine data for FSSgam fish length and abundance - full extent of BRUV samples
# author:  Claude & ?
# date:    February 2022
##


############ Import data ##############
## Load libraries --
library(raster)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(spatialEco)

rm(list=ls())

## Set work directory----
working.dir <- getwd()

# Study name---
study<-"2020-2021_south-west_BRUVs-BOSS"

## Set sub directories----
d.dir <- paste(working.dir,"data/tidy",sep="/") 
r.dir <- paste(working.dir,"data",sep="/")
s.dir <- paste(working.dir,"data/spatial/shapefiles",sep="/") # spatial is where I keep spatial data files, rasters and shapefiles
p.dir <- paste(working.dir,"plots",sep="/")

## Load metadata
setwd(d.dir)
dir()

metadata <- read.csv("2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  glimpse()

samples <- metadata %>% 
  dplyr::select("campaignid","sample", "method","latitude", "longitude")

## Convert lat and long of samples into spatial data and assign correct coordinate system
coordinates(samples) <- ~ longitude + latitude

proj4string(samples) # check coordinate system and/or projection
# If no projection give assign one,
proj4string(samples) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
samples <- spTransform(samples, CRS("+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

########## Distance to boat ramps ###########
library(argosfilter)

# Calculate the distance from boat ramp for each sample----
# Ramps
setwd(r.dir)
dir()

ramps <- read.csv("2020_SWC_boat_ramps.csv")# %>%
 # dplyr::rename(x=Longitude,y=Latitude,id=Boat.ramp)

samples.ramps <- metadata %>% 
  dplyr::select("campaignid","sample","method", "latitude", "longitude") #this has OpCode,Latitude,Longitude in it

canal.rocks <- ramps%>%
  filter(Boat.ramp%in%c("Canal Rocks")) 

canal.rocks.lat <- unique(canal.rocks$Latitude)
canal.rocks.lon <- unique(canal.rocks$Longitude)

distance.to.ramp<-samples.ramps%>%
  dplyr::select(campaignid,sample,latitude,longitude,method)%>%
  dplyr::mutate(canal.rocks = distance(ramps[1,3], .$latitude  ,ramps[1,2], .$longitude))%>%
  mutate(gracetown=distance(lat1=ramps[2,3],lat2=.$latitude,lon1=ramps[2,2],lon2=.$longitude))%>%
  mutate(gnarabup=distance(lat1=ramps[3,3],lat2=.$latitude,lon1=ramps[3,2],lon2=.$longitude))%>%
  mutate(hamelin=distance(lat1=ramps[4,3],lat2=.$latitude,lon1=ramps[4,2],lon2=.$longitude))%>%
  mutate(distance.to.ramp=do.call(pmin, .[,6:9]))%>%
  dplyr::select(campaignid,sample,method,distance.to.ramp)%>%
  distinct()%>% #need to be distinct otherwise joins dont work
  glimpse()

# Save to tidy data folder
setwd(d.dir)
dir()

write.csv(distance.to.ramp,paste(study,"distance.to.ramp.csv",sep="."),row.names=FALSE)

setwd(working.dir)
