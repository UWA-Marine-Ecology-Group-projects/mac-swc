###
# Project: mac - South-west Corner
# Data:    Geoscience Australia 250m resolution bathymetry
# Task:    Extract broad bathymetry derivatives for use in FSSGam modelling
# author:  Anita? & Claude
# date:    February 2022
##

rm(list=ls())

## Script to extract bathymetry covariates from raster files ####
# library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spatstat)
library(janitor)

# set working directories ----
w.dir <- getwd()
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "data/tidy", sep='/'))
h.dir <- (paste(w.dir, "data/raw/tm export"))
s.dir <- (paste(w.dir, "data/spatial/shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')
r.dir <- paste(w.dir, "data/spatial/rasters", sep='/')

# Load data ----
study <- "2020-2021_south-west_BOSS-BRUV"

boss <- read.csv("data/tidy/2020-2021_south-west_BOSS.checked.metadata.csv")%>%
  mutate_at(vars(campaignid, sample, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  dplyr::mutate(method = "BOSS")%>%
  glimpse()

bruv <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.checked.metadata.csv")%>%
  mutate_at(vars(campaignid, sample, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(id = paste(campaignid,sample,sep = " "))%>%
  glimpse()

janitor::compare_df_cols(boss,bruv)

df <- bind_rows(bruv,boss)

head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

# Get bathy derivatives ----
s <- raster(paste(r.dir, "SW_slope-to-260m.tif", sep='/'))
a <- raster(paste(r.dir, "SW_aspect-to-260m.tif", sep='/'))
r <- raster(paste(r.dir, "SW_roughness-to-260m.tif", sep='/'))
t <- raster(paste(r.dir, "SW_tpi-to-260m.tif", sep='/'))
b <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
x <- raster(paste(r.dir, "SW_detrend.bathy-to-260m.tif", sep='/'))

d <- stack(s,a,r,t, b)
plot(t)
names(d) <- c("slope", "aspect", "roughness", "tpi", "ga_depth")
extent(d)
plot(d)
# weird <- df %>% 
#   dplyr::filter(sample %in% c("IO267"))%>%
#   glimpse()
# 
# weird <- dfs[dfs$SW_tpi.to.260m < -10, ]
# test <- buffer(weird, 1000)
# plot(test, add = T)
# 
# t_crop <- crop(d, test)
# plot(t_crop)
# plot(weird, add = T)

# Extract bathy derivatives from data points --
ders <- raster::extract(d, dfs, sp = T)
ders <- as.data.frame(ders)

detrended <- raster::extract(x,dfs,sp = T)
detrended <- as.data.frame(detrended) %>%
  dplyr::select(latitude,longitude,SW_detrend.bathy.to.260m)

dfs <- ders %>%
  left_join(detrended)%>%
  dplyr::rename(detrended = SW_detrend.bathy.to.260m)%>%
  glimpse()

# Save dfs --
bathy <- dfs %>%
  dplyr::select(campaignid,sample,method,slope,aspect,roughness,tpi,detrended)

test <- df %>%
  dplyr::filter(id%in%c('2020-10_south-west_BOSS 358',"2020-10_south-west_BOSS 78","2020-10_south-west_BOSS 321","2020-10_south-west_BOSS 284"))%>%
  dplyr::select(latitude,longitude,id)

coordinates(test) <- ~longitude+latitude 

par(mfrow=c(1,1))
plot(t)
plot(test, pch = 20, cex = 1, add=T)

setwd(dt.dir)

write.csv(bathy,paste(study,"bathymetry.derivatives.csv",sep="."),row.names = F)

setwd(w.dir)
