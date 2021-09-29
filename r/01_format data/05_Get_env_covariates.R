### Get environmental covariates for Species Archetype Models ----

# Load libraries ----

#library(devtools)
#devtools::install_github('skiptoniam/ecomix')
#install_github("twitter/AnomalyDetection")
#ibrary(ecomix)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spastat)


# clear workspace ----
rm(list = ls())


# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')


# Load data ----

study <- "2020_south-west_stereo-BRUVs"

df <- read.csv(paste(dt.dir, paste(study, "complete.maxn.with.clusters.csv", sep='.'), sep = '/'))%>%
  mutate_at(vars(sample, family, unique.name, genus, full.name, species, status, cluster, cluster.new, number, n, dataset), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude 

# Get bathy and detrended derivatives ----
b <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
plot(b)
points(dfs)
d <- stack(paste(r.dir, "SW_detrendend.derivatives-to-260m.tif", sep='/'))
plot(d$flowdir)
names(d)
n <- read.csv(paste(r.dir, "names.det.bath.csv", sep='/'))
n
#n$covs <- c("detrended.bathy", "slope", "flowdir", "tri", "tpi", "aspect")
names(d) <- n$x

# Get normal derivatives ----
bds <- stack(paste(r.dir, "SW_bathy.derivatives-to-260m.tif", sep='/'))
names(bds)
names2 <- read.csv(paste(r.dir, "names.bathy.ders.csv", sep='/'))
names(bds) <- names2$x



# Extract bathy derivatives from data points --
dfs <- raster::extract(bds, dfs, sp = T)
str(dfs)

dfs <- raster::extract(d, dfs, sp = T)
str(dfs)
head(dfs)


## so far just using bathy covariates ##

# save up to here ----
#write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))


###       ###       ###       ###

# Still working on this section ----

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
#write.csv(dfs, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))




# load metadata to extract temp covariates ----
# this is because the metadata has less points than the maxn data

md <- read.csv(paste(dt.dir, "2020_south-west_stereo-BRUVs.checked.metadata.csv", sep='/')) %>%
  mutate_at(vars(sample), list(as.factor))
str(md) # 316 levels of factor
which(duplicated(md$sample))
# remove duplicates
md <- md[-288,]

mdsp <- md
coordinates(mdsp) <- ~longitude+latitude

plot(ts$SSTmean_SSTARRS)
points(mdsp)
md2 <- raster::extract(ts$SSTmean_SSTARRS, mdsp, sp = T)
head(md2)
md3 <- as.data.frame(md2)
which(is.na(md3$SSTmean_SSTARRS))
length(which(is.na(md3$SSTmean_SSTARRS))) # 10 bruvs with Nas for temp

md.no.temp <- md[c(which(is.na(md3$SSTmean_SSTARRS))),]
points(md.no.temp$longitude, md.no.temp$latitude) # Plot them

###       ###       ###         ###

# THIS IS NOT WORKINK....

## Remove NA's by replacing with nearest neighbor ----
# https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel

head(md3)
names(md3)
xy <- md3[,c(6,5)]
head(xy)

# use normal extract function to show that NAs are extracted for some points
extracted <- extract(x = t1, y = xy)


# then take the raster value with lowest distance to point AND non-NA value in the raster
sampled <-  apply(X = xy, MARGIN = 1, FUN = function(xy) t1@data@values[which.min(replace(distanceFromPoints(t1, xy), is.na(t1), NA))])


# show output of both procedures
print(data.frame(xy, extracted, sampled))


### ADD this to use habitat coveriates or save ----

# save without habitat covs ----
write.csv(df2, paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep='/'))


###       ###       ###       ###       ###

## Get habitat covariates (BRUV style) ----
df2 <- as.data.frame(dfs)
head(df2)
str(df2)
levels(df2$sample)
names(df2)

# get habitat data - used detailed but can use coarse --
h <- read.csv("G:/My Drive/Anita/NESP_D3/2020-06._detailed.habitat.csv")%>%
  mutate_at(vars(sample, site, location), list(as.factor)) %>% # make these columns as factors
  mutate(sample = recode(sample,
                         "01" = "1", "02" = "2", "03" = "3", "04" ="4", "05" = "5",
                         "06" = "6", "07" = "7", "08" = "8", "09" = "9")) %>% # make sure sample factor names are the same as in df2
  glimpse()
head(h)
str(h)
levels(h$sample)
names(h)

# Merge df2 with habitat data by sample --

dfh <- merge(df2 %>% dplyr::select(-c(X, campaignid.x, campaignid.y, site, observer, successful.count, successful.length, deployment, notes)),
             h %>% dplyr::select(-c(latitude, longitude, date, time, site, location, successful.count, fov.Limited, fov.Open, type)),
             by = 'sample') %>%
  glimpse()

str(dfh)
names(dfh)

dfh <- dfh %>% rename(bathy = SW_bathy.to.260m, slope = detr.slope, flowdir =  detr.flowdir, tri = detr.tri,
                      tpi = detr.tpi, aspect = detr.aspect,
                      temp_mean = SSTmean_SSTARRS, temp_sterr = SSTsterr_SSTARRS, temp_trend = SSTtrend_SSTARRS)

# Save csv of MaxN and covariates ----
write.csv(dfh, paste(dt.dir, "2020-06_sw_maxn.meta.cov.csv", sep='/'))
