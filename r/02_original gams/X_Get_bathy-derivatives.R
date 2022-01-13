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

df <- read.csv(paste(dt.dir, paste(study, "checked.metadata.csv", sep='.'), sep = '/'))%>%
  mutate_at(vars(campaignid, sample, status, site, dataset), list(as.factor)) %>% # make these columns as factors
  glimpse()

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

d <- stack(s,a,r,t, b)
plot(t)
names(d) <- c("slope", "aspect", "roughness", "tpi", "ga_depth")

weird <- df %>% 
  dplyr::filter(sample %in% c("IO267"))%>%
  glimpse()

weird <- dfs[dfs$SW_tpi.to.260m < -10, ]
test <- buffer(weird, 1000)
plot(test, add = T)

t_crop <- crop(d, test)
plot(t_crop)
plot(weird, add = T)

# Extract bathy derivatives from data points --
dfs <- raster::extract(d, dfs, sp = T)
dfs <- as.data.frame(dfs)
str(dfs) # this is the same df initially loaded with added columns for eachderivative

# Save dfs --
bathy <- dfs %>%
  dplyr::select(campaignid,sample,slope,aspect,roughness,tpi)

setwd(dt.dir)

write.csv(bathy,paste(study,"bathymetry.derivatives.csv",sep="."),row.names = F)
