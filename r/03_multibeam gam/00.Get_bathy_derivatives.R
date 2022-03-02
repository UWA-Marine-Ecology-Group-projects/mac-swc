###
# Project: mac - South-west Corner
# Data:    SwC Multibeam 
# Task:    Calculate and extract bathymetry derivatives from multibeam data
# author:  Claude & Anita?
# date:    February 2022
##

library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spatstat)
library(rstudioapi)
library(stars)
library(starsExtra)

# clear workspace ----
rm(list = ls())

working.dir <- getwd()
setwd(working.dir)

# Load Multibeam ----
b <- raster("data/spatial/rasters/SwC_Multibeam.tiff")
plot(b)

#not sure what this was for..?
# old.tiff <- b
# old.tiff
# 
# new.tiff <- raster("data/spatial/rasters/ga-4858_south_west_corner_marine_park_cube_05m_egm2008_epsg-4326_20210327.tiff")
# plot(new.tiff)
# new.tiff

# crop to extent --
#e <- drawExtent()
e <- extent(288664.7 , 311265.2 , 6220416 , 6234275 )
b <- crop(b, e)
plot(b)
b # 4x4m resolution
# 
# #### Transform from utm to lat long ----
# 
# # why??
# # open reference file 
# ref <- raster("data/spatial/rasters/GB-SW_250mBathy.tif")
# ref.crs <- proj4string(ref)
# 
# b <- projectRaster(b, crs = ref.crs)
# plot(b)
# proj4string(b) # check it worked


# Calculate bathy derivatives ----
# #slope
# s <- terrain(b, 'slope')
# plot(s)
# #aspect
# a <- terrain(b, 'aspect')
# plot(a)
#roughness
r <- terrain(b, 'roughness', neighbours = 8)
plot(r)
#TPI
t <- terrain(b, 'TPI', neighbours = 8)
plot(t)
# #flow direction
# f <- terrain(b, 'flowdir')
# plot(f)

# detrend bathy to highlight local topo
zstar <- st_as_stars(b)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")

ders <- stack(b, r, t, detre[[1]])
names(ders) <- c("depth", "roughness" , "tpi" , "detrended")
plot(ders)

# save stack of derivatives
writeRaster(ders, "data/spatial/rasters/multibeam_derivatives.tif", 
            overwrite = TRUE, bylayer = TRUE, suffix = "names")
