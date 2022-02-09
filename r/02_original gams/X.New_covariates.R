###
# Project: mac - South-west Corner
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# author:  Kingsley Griffin & Claude
# date:    Jul-Oct 2021
##

rm(list=ls())

library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# read in and merge GA coarse bathy
fbath <- raster("data/spatial/rasters/GA_Bathymetry_past-shelf.tif")
plot(fbath)
crs(fbath)

# transform bathy to projected coords for modelling
wgscrs  <- CRS("+proj=longlat +datum=WGS84")
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
proj4string(fbath) <- wgscrs
fbath_t <- projectRaster(fbath, crs = sppcrs)

# calculate terrain on fine bathy
preds <- terrain(fbath_t, neighbors = 8,
                 opt = c("slope", "aspect", "TPI", "TRI", "roughness"))
preds <- stack(fbath_t, preds)
plot(preds)

# detrend bathy to highlight local topo
zstar <- st_as_stars(fbath_t)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
preds <- stack(preds, detre)
plot(preds)

saveRDS(preds, "data/spatial/spatial_covariates.rds")

# clear workspace of large rasters etc
rm(list=ls())
