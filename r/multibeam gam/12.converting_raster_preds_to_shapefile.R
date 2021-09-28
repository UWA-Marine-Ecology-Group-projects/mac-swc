library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(raster)
library(patchwork)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(visreg)
library(mgcv)
require(sp)

# clear workspace ----
rm(list = ls())

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

# set working directories ----
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

w.dir <- "Z:/SWC-multibeam-GAMMS"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
h.dir <- (paste(w.dir, "Data/Habitat/BRUV Style annotation/tidy data"))
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')

#open your raster with n bands
r<-stack(paste(r.dir, "sr_predictions.tif", sep='/'))

#Plot it just to see if everything is ok
plot(r)

Check the number of bands
nlayers(r)

setwd(dt.dir)
for(i in 1:nlayers(r)){
  band<-r[[i]]
  #save raster in a separate file
  writeRaster(band,paste('band',i,'.tif', sep=''))
}

# clear workspace to free up memory ----
rm(list = ls())

#read in one band at a time
#dir()
#r.preds <- raster("band1.tif", sep='/')

#polys1 = rasterToPolygons(r.preds) # not enough memory for this

#names(p_rast)
#cols = rev(terrain.colors(255))

#spplot(p_rast, "name from code here", col.regions=cols, lwd=0)
