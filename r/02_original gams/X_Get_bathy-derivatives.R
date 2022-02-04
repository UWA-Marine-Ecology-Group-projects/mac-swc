rm(list=ls())

## Script to extract bathymetry covariates from raster files ####
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spatstat)

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
study <- "2020_south-west_stereo-BRUVs"
name <- "2020_south-west_stereo-BRUVs-BOSS"

boss <- read.csv("data/tidy/2021-03_West-Coast_BOSS.checked.metadata.csv")%>%
  mutate_at(vars(campaignid, sample, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  dplyr::mutate(method = "BOSS")%>%
  glimpse()

bruv <- read.csv(paste(dt.dir, paste(study, "checked.metadata.csv", sep='.'), sep = '/'))%>%
  mutate_at(vars(campaignid, sample, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  dplyr::mutate(method = "BRUV")%>%
  glimpse()

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

#x <- as.data.frame(x, xy = TRUE, na.rm = TRUE)

d <- stack(s,a,r,t, b)
plot(t)
names(d) <- c("slope", "aspect", "roughness", "tpi", "ga_depth")

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

setwd(dt.dir)

write.csv(bathy,paste(name,"bathymetry.derivatives.csv",sep="."),row.names = F)

setwd(w.dir)
