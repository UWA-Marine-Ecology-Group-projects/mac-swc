###
# Project: mac - South-west Corner
# Data:    SwC Multibeam 
# Task:    prepare bathmetry derivatives and write to csv
# author:  Claude & Anita?
# date:    February 2022
##
# library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spatstat)

# clear workspace ----
rm(list = ls())

# Load data ----
study <- "2020-2021_south-west_BOSS-BRUV"

#set working directory
working.dir <- getwd()
setwd(working.dir)

df <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  dplyr::select(campaignid, sample, latitude, longitude, depth)%>%
  glimpse()

head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

#transform to UTM
crs(dfs) <- "+proj=longlat +datum=WGS84"
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
dfs <- spTransform(dfs, sppcrs)

# Load  derivatives ----
d <- raster("data/spatial/rasters/multibeam_derivatives_depth.tif") #depth
x <- raster("data/spatial/rasters/multibeam_derivatives_detrended.tif") #detrended
r <- raster("data/spatial/rasters/multibeam_derivatives_roughness.tif") #roughness
t <- raster("data/spatial/rasters/multibeam_derivatives_tpi.tif") #tpi
bds <- stack(d,x, r, t) #changed to "Multibeam_derivatives.tif"
names(bds)
# names2 <- read.csv("data/spatial/rasters/names.bathy.ders.csv")
# names(bds) <- names2$x
# names(bds)
plot(bds$multibeam_derivatives_depth)
plot(dfs, pch = 20, cex = 1, add=T) 

# Extract bathy derivatives from data points --
dfs <- raster::extract(bds, dfs, sp = T)
head(dfs)

dfs <- as.data.frame(dfs) %>%
  dplyr::filter(!is.na(multibeam_derivatives_tpi))%>%
  dplyr::mutate(multibeam_derivatives_depth = abs(multibeam_derivatives_depth))%>%
  glimpse()

# save covariates ----
write.csv(dfs, "data/tidy/2020-2021_south-west_BOSS-BRUV.multibeam-derivatives.csv",row.names = F)           #Claude changed name

