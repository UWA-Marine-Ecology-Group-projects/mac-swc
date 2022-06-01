# get broad bathy for overview plots
# By Claude
library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters/tiles", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy %>%
  dplyr::filter(X > 109 & X < 123) %>%
  dplyr::filter(Y > -40 & Y < -32)
# cbathy <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

writeRaster(bath_r, filename = "data/spatial/rasters/swc-bathy-siteplots.tif")
