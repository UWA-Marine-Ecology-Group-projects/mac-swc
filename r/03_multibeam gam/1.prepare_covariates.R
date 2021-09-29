## prepare spatial environmental covariates for species archetype models ###

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


# set working directories ----
w.dir <- "Z:/SWC-multibeam-GAMMS"
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')




# Load data ----

study <- "2020_south-west_stereo-BRUVs"

df <- read.csv(paste(dt.dir, paste(study, "complete.maxn.csv", sep='.'), sep = '/'))%>%
  mutate_at(vars(sample, scientific, family, genus,  species, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

plot(dfs$depth)

# Get bathy and detrended derivatives ----
b <- raster(paste(r.dir, "Multibeam_derivatives.tif", sep='/')) #HS did not run for new bathy

# plot bathy and points
plot(b) 
plot(dfs, pch = 20, cex = 1, add=T) 



# Load  derivatives ----
bds <- stack(paste(r.dir, "Multibeam_derivatives.tif", sep='/')) #changed to "Multibeam_derivatives.tif"
names(bds)
names2 <- read.csv(paste(r.dir, "names.bathy.ders.csv", sep='/'))
names(bds) <- names2$x
names(bds)
plot(bds)


# Extract bathy derivatives from data points --
dfs <- raster::extract(bds, dfs, sp = T)
str(dfs)
head(dfs)



## so far just using bathy covariates ##

# save maxn with covariates ----
write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))


###       ###       ###       ###


# Get SST covariates ----
t1 <- raster(paste(r.dir, "SSTmean_SSTARRS.tif", sep='/'))
t2 <- raster(paste(r.dir, "SSTsterr_SSTARRS.tif", sep='/'))
t3 <- raster(paste(r.dir, "SSTtrend_SSTARRS.tif", sep='/'))

ts <- stack(t1, t2, t3)
plot(ts)
plot(ts$SSTmean_SSTARRS)

dfs <- raster::extract(ts, dfs, sp=T)
head(dfs)


#### save mxn with bathy ders and temp ----
write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))
