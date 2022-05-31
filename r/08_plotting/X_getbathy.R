# get bathy to make the broad plot look better
library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/raster", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

# aggregate raster to reduce size and plotting time etc
aggbath  <- aggregate(bath_r, 10)
abath_df <- as.data.frame(aggbath, xy = TRUE, fun = max, na.rm = TRUE)
saveRDS(abath_df, 'data/ga_bathy_trim.rds')