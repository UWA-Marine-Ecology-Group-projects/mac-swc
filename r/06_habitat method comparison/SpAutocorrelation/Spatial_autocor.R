# testing spatial autocorrelation #
# https://rfunctions.blogspot.com/2017/06/how-to-identify-and-remove-spatial.html?fbclid=IwAR0-UzcspeSxap0y3XRs7-Yr3sB6G9OKctlAmAzKM7EkbR7OqMlGxoWqzAk

library(nlme)
library(ape)
library(MuMIn)

library(caret)
library(raster)
library(rgdal)
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(leaflet)
library(plotGoogleMaps)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(plotrix)
library(sp)
library(caTools)

# Moran's I and spatial dependencies
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(pgirmess) # Data Analysis in Ecology

# Attach libraries for point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic

# multivariate
library(vegan)
library(FactoMineR)
library(factoextra)

# Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <- "Y:/boss-habitat-swcmp/tidy data"


#### DOWNWARD FACING ####

# Load data ----

dfname <- "20201119_Multibeamed_Downwards._broad.habitat.csv"

df <- read.csv(paste(dt.dir, dfname, sep='/'), head =T)
head(df)
str(df) # observations duplicated 5 times
# check for duplicates--
duplicated(df)
# extract unique --
df <- unique(df)
str(df) # 37 observations

df$latitude <- df$latitude*(-1)
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude
plot(dfs)


# Project sp points --
crs1 <- CRS("+init=epsg:4979") # latlon
crs2 <- CRS("+init=epsg:32750")

proj4string(dfs) <- crs1
dfs2 <- spTransform(dfs, crs2)
proj4string(dfs2)

head(dfs2)
df2 <- as.data.frame(dfs2)
head(df2)

#### 1. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(df2$longitude, df2$latitude)))
maxDist # 10463.11

minDist<-min(dist(cbind(df2$longitude, df2$latitude)))
minDist #  31.09662

dists <- dist(cbind(df2$longitude, df2$latitude))
dists

nclasses <- maxDist/400 # 26.15777
head(df2)

xy=cbind(df2$longitude, df2$latitude)
pgi.cor <- correlog(coords=xy, z=df2$broad.Sponges, method="Moran", nbclass=14)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 2. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","broad.Sponges")])


##    ##    ##    ##    ##    ##

#### 3. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/14,maxDist,l=14))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


### 4. Assemblage : PCA ----
head(df2)
df3 <- df2[,c(9:14)]
head(df3)

r.pca <- PCA(df3, scale.unit=TRUE, ncp=5, graph=TRUE)
print(r.pca)
head(r.pca)
r.pca$ind
r.pca$ind$dist
df2$pcadist <- r.pca$ind$dist


pgi.cor <- correlog(coords=xy, z=df2$pcadist, method="Moran", nbclass=45)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 5. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","pcadist")])


##    ##    ##    ##    ##    ##

#### 6. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/45,maxDist,l=45))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


#### FORWARD BRUV Style ####

# clear environment --
rm(list = ls())

# Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <- "Y:/boss-habitat-swcmp/tidy data"

# Load data ----

dfname <- "20201119_Multibeamed_BRUVstyle._broad.habitat.csv"

df <- read.csv(paste(dt.dir, dfname, sep='/'), head =T)
head(df)
str(df) # observations duplicated 5 times
# check for duplicates--
duplicated(df)
# extract unique --
df <- unique(df)
str(df) # 37 observations

df$latitude <- df$latitude*(-1)
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude
plot(dfs)


# Project sp points --
crs1 <- CRS("+init=epsg:4979") # latlon
crs2 <- CRS("+init=epsg:32750")

proj4string(dfs) <- crs1
dfs2 <- spTransform(dfs, crs2)
proj4string(dfs2)

head(dfs2)
df2 <- as.data.frame(dfs2)
head(df2)

#### 1. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(df2$longitude, df2$latitude)))
maxDist # 10463.11

minDist<-min(dist(cbind(df2$longitude, df2$latitude)))
minDist # 31.09662

nclasses <- maxDist/400 # 26.15777
head(df2)

xy=cbind(df2$longitude, df2$latitude)
pgi.cor <- correlog(coords=xy, z=df2$broad.Sponges, method="Moran", nbclass=26)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 2. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","broad.Sponges")])


##    ##    ##    ##    ##    ##

#### 3. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/26,maxDist,l=26))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


### 4. Assemblage : PCA ----
head(df2)
df3 <- df2[,c(10:16)]
head(df3)

r.pca <- PCA(df3, scale.unit=TRUE, ncp=5, graph=TRUE)
print(r.pca)
head(r.pca)
r.pca$ind
r.pca$ind$dist
df2$pcadist <- r.pca$ind$dist


pgi.cor <- correlog(coords=xy, z=df2$pcadist, method="Moran", nbclass=45)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 5. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","pcadist")])


##    ##    ##    ##    ##    ##

#### 6. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/45,maxDist,l=45))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)



#### FORWARD BOSS style ####


# clear environment --
rm(list = ls())

# Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <- "Y:/boss-habitat-swcmp/tidy data"

# Load data ----

dfname <- "20201119_Multibeamed_BOSSstyle._broad.habitat.csv"

df <- read.csv(paste(dt.dir, dfname, sep='/'), head =T)
head(df)
str(df) # observations duplicated 5 times
# check for duplicates--
duplicated(df)
# extract unique --
df <- unique(df)
str(df) # 37 observations

df$latitude <- df$latitude*(-1)
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude
plot(dfs)


# Project sp points --
crs1 <- CRS("+init=epsg:4979") # latlon
crs2 <- CRS("+init=epsg:32750")

proj4string(dfs) <- crs1
dfs2 <- spTransform(dfs, crs2)
proj4string(dfs2)

head(dfs2)
df2 <- as.data.frame(dfs2)
head(df2)

#### 1. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(df2$longitude, df2$latitude)))
maxDist # 10463.11

minDist<-min(dist(cbind(df2$longitude, df2$latitude)))
minDist # 31.09662

nclasses <- maxDist/400 # 26.15777
head(df2)

xy=cbind(df2$longitude, df2$latitude)
pgi.cor <- correlog(coords=xy, z=df2$broad.Seagrasses, method="Moran", nbclass=26)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 2. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","broad.Seagrasses")])


##    ##    ##    ##    ##    ##

#### 3. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/26,maxDist,l=26))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


### 4. Assemblage : PCA ----
head(df2)
df3 <- df2[,c(9:15)]
head(df3)

r.pca <- PCA(df3, scale.unit=TRUE, ncp=5, graph=TRUE)
print(r.pca)
head(r.pca)
r.pca$ind
r.pca$ind$dist
df2$pcadist <- r.pca$ind$dist


pgi.cor <- correlog(coords=xy, z=df2$pcadist, method="Moran", nbclass=26)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 5. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","pcadist")])


##    ##    ##    ##    ##    ##

#### 6. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/26,maxDist,l=26))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


#### BRUV DATA REAL ####

# clear environment --
rm(list = ls())

# Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <- "Y:/boss-habitat-swcmp/tidy data"

# Load data ----

dfname <- "202006_Multibeamed_BRUV._broad.habitat.csv"

df <- read.csv(paste(dt.dir, dfname, sep='/'), head =T)
head(df)
str(df) # observations duplicated 5 times
# check for duplicates--
duplicated(df)
# extract unique --
df <- unique(df)
str(df) # 37 observations

df$latitude <- df$latitude*(-1)
head(df)
str(df)

dfs <- df
coordinates(dfs) <- ~longitude+latitude
plot(dfs)


# Project sp points --
crs1 <- CRS("+init=epsg:4979") # latlon
crs2 <- CRS("+init=epsg:32750")

proj4string(dfs) <- crs1
dfs2 <- spTransform(dfs, crs2)
proj4string(dfs2)

head(dfs2)
df2 <- as.data.frame(dfs2)
head(df2)

#### 1. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(df2$longitude, df2$latitude)))
maxDist # 10463.11

minDist<-min(dist(cbind(df2$longitude, df2$latitude)))
minDist # 6.430533

distx <- dist(cbind(df2$longitude, df2$latitude))
which.min(distx)
distx[299]


nclasses <- maxDist/400 # 26.15777
head(df2)

xy=cbind(df2$longitude, df2$latitude)
pgi.cor <- correlog(coords=xy, z=df2$broad.Seagrasses, method="Moran", nbclass=26)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 2. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","broad.Seagrasses")])


##    ##    ##    ##    ##    ##

#### 3. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/26,maxDist,l=26))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


### 4. Assemblage : PCA ----
head(df2)
df3 <- df2[,c(9:15)]
head(df3)

r.pca <- PCA(df3, scale.unit=TRUE, ncp=5, graph=TRUE)
print(r.pca)
head(r.pca)
r.pca$ind
r.pca$ind$dist
df2$pcadist <- r.pca$ind$dist


pgi.cor <- correlog(coords=xy, z=df2$pcadist, method="Moran", nbclass=26)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red
pgi.cor

##    ##    ##    ##    ##    ##
#### 5. Spatial autocorrlelation p stats ----

dat_geo<-as.geodata(df2[,c("longitude","latitude","pcadist")])


##    ##    ##    ##    ##    ##

#### 6. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/26,maxDist,l=26))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)