### Map of the sampling sites ----

### MAP of BRUV deployments of the South West Corner ----

### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(sp)
library(spData)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(tmap)
library(mapdata)
library(leaflet)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(latticeExtra)
library(dplyr)
library(rasterVis)
library(zoo)
library(fields)
library(geoR)
library(gstat)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(patchwork)
#library(elsa)
#install.packages("corrplot")
#library(corrplot)
library(broman)
library(viridis)


# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "Plots", sep = '/')
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')


# http://oswaldosantos.github.io/ggsn/


# Read sw cmr polys ----
cmr <- readOGR(paste(s.dir, "SW_CMR.shp",sep='/'))
plot(cmr)
names(cmr)
head(cmr)
cmr$ZONENAME <- as.factor(cmr$ZONENAME)
crs1 <- proj4string(sw)
levels(cmr$ZONENAME)
# get poly for each zone --
NPZ <- cmr[cmr$ZONENAME=="National Park Zone",]
#HPZ <- cmr[cmr$ZoneName=="Habitat Protection Zone",]
#MUZ <- gb[gb$ZoneName=="Multiple Use Zones",]
SPZ <- cmr[cmr$ZONENAME=="Special Purpose Zone (Mining Exclusion)",]

# read Australia poly ----
wa <- readOGR(paste(s.dir, "WA_wgs84.shp",sep='/'))
plot(wa)

# read state reserves ----
wamp <- readOGR(paste(s.dir, "All_StateReserves.shp", sep='/'))
wamp

# set extent --
ext1 <- extent(114.464, 115.2, -34.29779, -33.57028)

# crop polys ----
cmr1 <- crop(cmr, ext1)
wa1 <- crop(wa, ext1)
wamp1 <- crop(wamp, ext1)
NPZ1 <- crop(NPZ, ext1)
SPZ1 <- crop(SPZ, ext1)

# Read BRUV deployment coordinates ----
df <- read.csv(paste(dt.dir, "deployed_BRUVS.csv", sep = '/'))%>%
  mutate_at(vars(sample, family, genus, species, dataset, unique.name, full.name, location, status, cluster, cluster.new, number, n, class), list(as.factor)) %>% # make these columns as factors
  # At some point filter for successful count
  glimpse()


dfs <- df
coordinates(dfs) <- ~longitude+latitude
plot(dfs)

# Read land locations ----
df <- read.csv(paste(s.dir, "Locations.csv", sep = '/'))
df
# remove some if needed
df <- df[-c(5:7),]
df
dfs <- df
coordinates(dfs) <- ~Lon+Lat
plot(dfs)
     
### raster data ----

# Read bathy data ----
bathy <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
plot(bathy)
proj4string(bathy) <- proj4string(cmr)
names(bathy) <- "Depth"
bathy@z

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html

# choose colours --

br1 <- brocolors("crayons")["Tumbleweed"]
pink <- brocolors("crayons")["Lavender"]
sg <- brocolors("crayons")["Spring Green"]
y1 <- brocolors("crayons")["Canary"]
g1 <- brocolors("crayons")["Asparagus"]
g2 <- brocolors("crayons")["Fern"]
g3 <- brocolors("crayons")["Tropical Rainforest"]
g4 <- brocolors("crayons")["Yellow Green"]
g5 <- brocolors("crayons")["Pine Green"]
g6 <- brocolors("crayons")["Electric Lime"]

## MAP----

map <- tm_shape(cmr1, bbox = ext1)  + tm_borders(col ='white', lwd = 1.5) +
  tm_compass(type = "arrow", position = c(0.1, 0.1), size = 3) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.7, position = c(0.08, 0.02)) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- map + tm_shape(bathy) + 
  #tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
  tm_raster(title = 'Depth (m)', palette=get_brewer_pal("Blues", plot=FALSE), style='cont', legend.reverse = T) +
  tm_layout(legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = 'right',
            #legend.position = c(1, 0.05),
            legend.title.size = 1.5,
            legend.title.color = 'black',
            legend.width = 1) 
map1

map2 <- map1 + tm_shape(wa) + tm_borders(col ='black', lwd = 1) + tm_fill(col = br1) 
map2

map3 <- map2 + tm_shape(dfs) + tm_symbols(col = 'black', size = 0.2) +
  tm_text("Location", size = 0.9, xmod = 1.4, ymod = -0.5)
map3

map4 <- map3 + tm_shape(SPZ1) + tm_borders(col = 'grey55', lwd = 1, lty = '33') +
  tm_add_legend(type = 'fill', labels = "Special Purpose Zone", col= 'white', border.col = 'grey55', border.lwd = 2, border.lty = '33', size = 2)

map4

map5 <- map4 + tm_shape(NPZ1) + tm_borders(col = "#006600", lwd = 2) + 
          tm_add_legend(type = 'fill', labels = "National Park Zone", col= 'white', border.col = "#006600", border.lwd = 2, size = 2)

map5


map3 <- map2 + tm_shape(wamp) + tm_borders(col ='black', lwd = 1) + tm_fill(col='pink', alpha = 0.4) +
        tm_add_legend(type = 'fill', col = 'pink', alpha = 0.4, labels = 'State Sanctuary Zones')
map3


map4 <- map3 + tm_shape(dfs) + tm_dots('optional', col = 'red', size = 0.2, shape = 21) +
        tm_add_legend(type = "symbol", shape = 21, col = 'red', size = 0.7, labels = 'Stereo BRUVs')
map4



tmap_save(
tm = map4,
filename = paste(p.dir, "Bruvs_survey_map.png", sep ='/'),
#width = NA,
#height = NA,
#units = NA,
dpi = 300)








### TESTING MULTIBEAM ----

b.dir <- "C:/Users/00093391/Dropbox/Field (1)"

b <- raster(paste(b.dir, "ga-4858_swc_ausseabed_cube_04m_EGM2008_epsg-32750_20210222.tiff", sep ='/'))
plot(b)

e <- drawExtent()
b2 <- crop(b2, e)
plot(b2)
