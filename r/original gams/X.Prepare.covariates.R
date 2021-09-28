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
#install_github("leandroroser/EcoGenetics-devel")
library(EcoGenetics) # for detrended bathy 


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


# Get bathy derivatives ----
b <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
plot(b)
points(dfs)
f <- raster(paste(r.dir, "SW_flowdir-to-260m.tif", sep='/'))
plot(f)
s <- raster(paste(r.dir, "SW_slope-to-260m.tif", sep='/'))
plot(s)


# Calculate detrended bathy ----
# https://blog.valdosta.edu/andersonlab/2018/05/19/point-process-modeling-how-to-detrend-a-raster-surface-in-r/
b2 <- as.data.frame(rasterToPoints(b))
bd <- eco.detrend(Z = b2[,3], XY = b2[,c(1,2)], degree =  1)
b3 <- cbind(bd@XY, bd@RES) # join result
head(b3)
# back to raster
dbathy <- rasterFromXYZ(b3, crs = "+proj=longlat +datum=WGS84 +no_defs")
plot(dbathy)

# save detrended bathy ----
writeRaster(dbathy, paste(r.dir, "SW_detrend.bathy-to-260m.tif", sep = '/'), overwrite = T)

# Compute derivatives based on detrended bathy ----
sl <- terrain(dbathy, 'slope', neighbors=8)
plot(sl)

fl <- terrain(dbathy, 'flowdir')
plot(fl)

tri <- terrain(dbathy, 'TRI')
plot(tri)

tpi <- terrain(dbathy, 'TPI')
plot(tpi)

asp <- terrain(dbathy, 'aspect', neighbors=8)
plot(asp)

# stack detrended derivatives --
detr.deri <- stack(dbathy, sl,fl,tri,tpi,asp)
names(detr.deri) <- c("detrended.bathy" ,"detr.slope" ,"detr.flowdir" ,"detr.tri", "detr.tpi" , "detr.aspect")

names.det.bath <-  c("detrended.bathy" ,"detr.slope" ,"detr.flowdir" ,"detr.tri", "detr.tpi" , "detr.aspect")
write.csv(names.det.bath, paste(r.dir, "names.det.bath.csv", sep='/'))

# save detrended rasters --
writeRaster(detr.deri, paste(r.dir, "SW_detrendend.derivatives-to-260m.tif", sep='/'), overwrite = T)


