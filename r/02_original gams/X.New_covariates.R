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

#working dir
# set working directories ----
w.dir <- getwd()
setwd(w.dir)

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
                 opt = c("TPI", "roughness"))
preds <- stack(fbath_t, preds)
plot(preds)

# detrend bathy to highlight local topo
zstar <- st_as_stars(fbath_t)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
preds <- stack(preds, detre[[1]])
plot(preds)

# write rasters
saveRDS(preds, "data/spatial/250m_spatialcovariates_utm.rds")

preds <- projectRaster(preds, crs = wgscrs)
saveRDS(preds, "data/spatial/250m_spatialcovariates_wgs.rds")

#load metadata to get lat longs
df <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  glimpse()

# align crs and check samples over bathy and extract terrain data
allhab_sp <- SpatialPointsDataFrame(coords = df[6:5], data = df, 
                                    proj4string = wgscrs)
#allhab_t  <- spTransform(allhab_sp, CRS = wgscrs)
plot(preds[[1]])
plot(allhab_sp, add=T)
habt_df   <- as.data.frame(allhab_sp, xy = T)
habi_df   <- cbind(habt_df, raster::extract(preds, allhab_sp))
habi_df <- habi_df %>%
  dplyr::rename(ga.depth = GA_Bathymetry_past.shelf)
names(habi_df)

saveRDS(habi_df, "data/spatial/full-extent_spatial_covariates.rds")
write.csv(habi_df, "data/tidy/2020-2021_south-west_BOSS-BRUV.bathy.derivatives.csv")

cbaths <- list.files("data/spatial/rasters/tiles/", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
bath_r <- rasterFromXYZ(cbathy)
crs(bath_r) <- wgscrs

writeRaster(bath_r, "data/spatial/rasters/wadandi-cultural-guidance_bathymetry.tif",
            overwrite = T)

# clear workspace of large rasters etc
rm(list=ls())
