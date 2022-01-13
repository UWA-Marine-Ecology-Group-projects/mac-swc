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
library(rstudioapi)

# clear workspace ----
rm(list = ls())

working.dir <- getwd()
setwd(working.dir)

# Load Multibeam ----
b <- raster("data/spatial/rasters/SwC_Multibeam.tiff")
plot(b)

# crop to extent --
#e <- drawExtent()
e <- extent(288664.7 , 311265.2 , 6220416 , 6234275 )
b <- crop(b, e)
plot(b)
b # 4x4m resolution

#### Transform from utm to lat long ----

# open reference file 
ref <- raster("data/spatial/rasters/GB-SW_250mBathy.tif")
ref.crs <- proj4string(ref)

b <- projectRaster(b, crs = ref.crs)
plot(b)
proj4string(b) # check it worked


# Calculate bathy derivatives ----
s <- terrain(b, 'slope')
plot(s)
a <- terrain(b, 'aspect')
plot(a)
r <- terrain(b, 'roughness')
plot(r)
t <- terrain(b, 'TPI')
plot(t)
f <- terrain(b, 'flowdir')
plot(f)

ders <- stack(b,s,a,r,t,f)
names(ders) <- c("depth", "slope",  "aspect" ,  "roughness"  ,   "tpi" ,   "flowdir")

# save stack of derivatives
writeRaster(ders, "data/spatial/rasters/Multibeam_derivatives.tif")
