## prepare spatial environmental covariates###
library(plyr)
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
study <- "2020_south-west_stereo-BRUVs"

#set working directory
working.dir <- getwd()
setwd(working.dir)

#Claude just wants to extract only multibeam derivatives

df <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.checked.metadata.csv")%>%
  dplyr::select(campaignid, sample, latitude, longitude, depth)%>%
  glimpse()


# df <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
#   mutate_at(vars(sample, scientific, family, genus,  species, status, site, dataset), list(as.factor)) %>% # make these columns as factors
#   glimpse()
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

plot(dfs$depth)

# Get bathy and detrended derivatives ----
b <- raster("data/spatial/rasters/Multibeam_derivatives.tif") #HS did not run for new bathy

# plot bathy and points
plot(b) 
plot(dfs, pch = 20, cex = 1, add=T) 

# Load  derivatives ----
bds <- stack("data/spatial/rasters/Multibeam_derivatives.tif") #changed to "Multibeam_derivatives.tif"
names(bds)
names2 <- read.csv("data/spatial/rasters/names.bathy.ders.csv")
names(bds) <- names2$x
names(bds)
plot(bds)


# Extract bathy derivatives from data points --
dfs <- raster::extract(bds, dfs, sp = T)
str(dfs)
head(dfs)

dfs <- as.data.frame(dfs) %>%
  dplyr::filter(!tpi=="NA")%>%
  dplyr::rename(depth.multibeam = depth.1)%>%
  dplyr::mutate(depth.multibeam = abs(depth.multibeam))%>%
  glimpse()

# save maxn with covariates ----
write.csv(dfs, "data/tidy/2020_sw_multibeam-derivatives.csv")           #Claude changed name

# ###       ###       ###       ###
# 
# 
# # Get SST covariates ----
# t1 <- raster(paste(r.dir, "SSTmean_SSTARRS.tif", sep='/'))
# t2 <- raster(paste(r.dir, "SSTsterr_SSTARRS.tif", sep='/'))
# t3 <- raster(paste(r.dir, "SSTtrend_SSTARRS.tif", sep='/'))
# 
# ts <- stack(t1, t2, t3)
# plot(ts)
# plot(ts$SSTmean_SSTARRS)
# 
# dfs <- raster::extract(ts, dfs, sp=T)
# head(dfs)
# 
# 
# #### save mxn with bathy ders and temp ----
# write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))
