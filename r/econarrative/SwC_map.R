### MAP of different sea levels ---

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
library(tmaptools)
library(maptools)
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
library(smoothr)
library(units)
library(RColorBrewer)
library(grid)


# Clear memory ----
rm(list=ls())

### Set directories ----

w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
m.dir <- "/homevol/anitasgiraldo2021/mbh-d3-swc-fish"
p.dir <- paste(w.dir, "plots", sep = '/')
dt.dir <- paste(m.dir, "Data/Tidy", sep='/')
s.dir <- paste(m.dir, "shapefiles", sep='/')
r.dir <- paste(m.dir, "rasters", sep='/')

au <- readOGR(paste(s.dir, "AustraliaNetworkMarineParks.shp",sep='/'))
plot(au)
e <- drawExtent()
extent1 <- extent(106,122.2549,-38.80666,-33.06172)
au2 <- crop(au2,e)
plot(au2)
plot(wa, add=T)

### Read sw cmr polys ----
cmr <- readOGR(paste(s.dir, "GB-SwC-AMP.shp",sep='/'))
plot(cmr)
names(cmr)
head(cmr)
cmr$ZoneName <- as.factor(cmr$ZoneName)
cmr$ResName <- as.factor(cmr$ResName)
levels(cmr$ZoneName)
crs1 <- proj4string(sw)
levels(cmr$ZoneName)
dfcmr <- as.data.frame(cmr)

# get poly for each zone --
NPZ <- cmr[cmr$ZoneName=="National Park Zone",]
HPZ <- cmr[cmr$ZoneName=="Habitat Protection Zone",]
MUZ <- cmr[cmr$ZoneName=="Multiple Use Zone",]
SPZ <- cmr[cmr$ZoneName=="Special Purpose Zone",]
SPZME <- cmr[cmr$ZoneName=="Special Purpose Zone (Mining Exclusion)",]


# read Australia poly ----
wa <- readOGR(paste(s.dir, "WA_wgs84.shp",sep='/'))
plot(wa)

# read state reserves ----
wamp <- readOGR(paste(s.dir, "WA_MPA_2018.shp", sep='/'))
wamp

# read coastal waters ----
cw <- readOGR(paste(s.dir, "amb_coastal_waters_limit.shp", sep='/'))
cw

## Read Bathy ----
bathy <- raster(paste(r.dir, "GB-SW_250mBathy.tif", sep='/'))
plot(bathy)

# Colours ----
sg <- brocolors("crayons")["Spring Green"]
colspzme <- brocolors("crayons")["Violet Blue"]
colnpz <- brocolors("crayons")["Granny Smith Apple"]
colmuz <- brocolors("crayons")["Canary"]
colhpz <- brocolors("crayons")["Yellow Orange"]
colspz <- brocolors("crayons")["Sky Blue"]
colbg <- brocolors("crayons")["Blizzard Blue"]
colst <- brocolors("crayons")["Peach"]
y1 <- brocolors("crayons")["Canary"]
g1 <- brocolors("crayons")["Asparagus"]
g2 <- brocolors("crayons")["Fern"]
g3 <- brocolors("crayons")["Tropical Rainforest"]
g4 <- brocolors("crayons")["Yellow Green"]
g5 <- brocolors("crayons")["Pine Green"]
g6 <- brocolors("crayons")["Electric Lime"]


# MAP 1 ----
map <- tm_shape(cmr)  + tm_borders(col ='white', lwd = 1.5) +
  tm_compass(type = "arrow", position = c(0.087, 0.1), size = 2) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, position = c(0.08, 0.02)) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

# map0 <- map + tm_shape(bathy2) + tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
#   tm_layout(legend.text.size = 1.7,
#             legend.outside = TRUE,
#             legend.outside.position = 'right',
#             #legend.position = c(1, 0.05),
#             legend.title.size = 0.5,
#             legend.title.color = 'white',
#             legend.width = 1) 
# map0



# map1 <- map + tm_shape(cmr) + tm_borders(col = g2, lwd = 2) + tm_fill(col=g2) +
#   tm_add_legend(type = 'fill', labels = "21,000 years ago", col = g2, lwd = 2, size = 2)
# 
# map1
# 
# 
# map2 <- map1 + tm_shape(k70) + tm_borders(col = g5, lwd = 2) + tm_fill(col=g5) +
#   tm_add_legend(type = 'fill', labels = "70,000 years ago", col = g5, lwd = 2, size = 2)
# map2
# 
# 
# map3 <- map2 + tm_shape(k11) + tm_borders(col = g4, lwd = 2) + tm_fill(g4) +
#   tm_add_legend(type = 'fill', labels = "11,000 years ago", col = g4, lwd = 2, size = 2)
# map3

map4 <- map + tm_shape(wa) + tm_borders(col = 'black', lwd = 1) + tm_fill(col='lightgoldenrodyellow') +
  tm_add_legend(type = 'fill', labels = "Coastline today", col = 'lightgoldenrodyellow', lwd = 2, size = 2)
map4

# map5 <- map4 + tm_shape(dfs) + tm_symbols(col = 'black', size = 0.2) +
#   tm_text("Location", size = 0.8, xmod = 1.5, ymod = -0.5)
# map5

map55 <- map4 + tm_shape(cmr) + tm_borders(col = 'black', lwd = 1.5) 

map55

map6 <- map55 + tm_shape(NPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col=colnpz) +
  tm_add_legend(type = 'fill', labels = "National Park Zone", col = colnpz, lwd = 2, size = 2)
map6

map7 <- map6 + tm_shape(SPZME) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col = colspzme) +
  tm_add_legend(type = 'fill', labels = "Special Purpose Zone", col = colspzme, lwd = 2, size = 2)
map7

map8 <- map7 + tm_shape(MUZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col = colmuz) + 
  tm_add_legend(type = 'fill', labels = "Mutiple Use Zone", col = colmuz, lwd = 2, size = 2, lty ='33')
map8

map9 <- map8 + tm_shape(HPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col=colhpz) + 
  tm_add_legend(type = 'fill', labels = "Habitat Protection Zone", col = colhpz, lwd = 2, size = 2)
map9

map10 <- map9 + tm_shape(SPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col=colspz) +  
  tm_add_legend(type = 'fill', labels = "Special Purpose Zone", col = colspz, lwd = 2, size = 2)
map10

map11 <- map10 + tm_shape(cw) + tm_lines(col = 'red', lwd = 1) +
  tm_add_legend(type = 'line', labels = "Coastal waters limit", col = 'red', lwd = 2, size = 2)
map11

### MAP version2 ----

#### To create the inset map ----


sw_region <- st_bbox(e2,
                     crs = st_crs(wa)) %>%
  st_as_sfc()

map <- tm_shape(cmr, bbox=tmaptools::bb(extent1)) + tm_borders(col ='white', lwd = 1.5) +
  tm_compass(type = "arrow", position = c(0.045, 0.85), size = 2) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1, position = c(0.03, 0.7)) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 4, n.y = 4, labels.size = 1, lines = FALSE) +
  tm_layout(bg.color = "lightcyan1",
            main.title = "a",
            main.title.size = 2,
            main.title.fontface = "bold",
            main.title.position = "left")

map


map4 <- map + tm_shape(wa) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col='lightgoldenrodyellow') # +
  #tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 2)
map4

pal1 <- c("#FFFF99","#99FFFF", 'chartreuse3',   'steelblue2', "#324ab2")
levels(cmr$ZoneName)

map55 <- map4 + tm_shape(cmr) + tm_borders(col = 'grey35', lwd = 1.5) +
  tm_fill(col = 'ZoneName', palette = pal1, title = NA) +
  tm_shape(cw) + tm_lines(col = 'red', lwd = 1) +
  tm_layout(#legend.outside = TRUE,
            #legend.outside.size = 3,
            #legend.outside.position = 'right'
            legend.position = c(0.02, 0.04),
            legend.title.color = "lightcyan1",
            legend.title.size = 0.7,
            legend.text.size = 1,
            #legend.outside = TRUE,
            legend.width = 0.6,
            legend.height = 0.5,
            #legend.bg.color = 'white'
            #legend.just = 'right',
            ) +
  tm_add_legend(type = 'fill', labels = "State Marine Park", col = colst, lwd = 1, size = 1) +
  tm_add_legend(type = 'line', labels = "Coastal waters limit", col = 'red', lwd = 1, size = 1)


map55

map6 <- map55 + tm_shape(sw_region) + tm_borders(col = 'red', lwd = 3)
map6

# save ----
tmap_save(
  tm = map6,
  filename = paste(p.dir, "SwC3_Map.png", sep ='/'),
  #width = NA,
  #height = NA,
  #units = NA,
  #scale = 1,
  dpi = 300)


# Other additions if needed ----

map6 <- map55 + tm_shape(NPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col='chartreuse3') 
  
map6

map7 <- map6 + tm_shape(SPZME) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col = colspzme) 
  
map7

map8 <- map7 + tm_shape(MUZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col = "#99FFFF") 
  
map8

map9 <- map8 + tm_shape(HPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col="#FFFF99") 
  
map9

map10 <- map9 + tm_shape(SPZ) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col='steelblue2')  
  
map10

map11 <- map10 + tm_shape(cw) + tm_lines(col = 'red', lwd = 1) 
  
map11



# Legend

map.leg <- tm_shape(cmr) + tm_borders(col ='white', lwd = 1.5) +
  #tm_shape(cw1) + tm_lines(col = 'red', lwd = 1) +
  tm_layout(#outer.margins = 0.1,
    legend.text.size = 3,
    #legend.outside = TRUE,
    #   #legend.outside.position = 'right',
    #   #legend.outside.position = 'right',
    legend.only = TRUE) +
  #   legend.title.size = 1.5,
  #   legend.title.color = 'black',
  #legend.width = 1) +
  tm_add_legend(type = 'fill', labels = "National Park Zone", col = 'chartreuse3', lwd = 1, size = 1) + 
  tm_add_legend(type = 'fill', labels = "Special Purpose Zone (Mining exclusion)", col = colspzme, lwd = 1, size = 1) +
  tm_add_legend(type = 'fill', labels = "Special Purpose Zone", col = 'steelblue2', lwd = 1, size = 1) +
  tm_add_legend(type = 'fill', labels = "Mutiple Use Zone", col = "#99FFFF", lwd = 1, size = 1) +
  tm_add_legend(type = 'fill', labels = "Habitat Protection Zone", col = "#FFFF99", lwd = 1, size = 1) +
  #tm_add_legend(type = 'fill', labels = "Special Purpose Zone", col = 'steelblue2', lwd = 2, size = 1) +
  tm_add_legend(type = 'line', labels = "Coastal waters limit", col = 'red', lwd = 1, size = 1)


map.leg

current.mode <- tmap_mode("plot")
f.map <- tmap_arrange(map11, map.leg, ncol=2)
f.map <- tmap_arrange(map11, map.leg, ncol=2, widths = c(0.8, 0.2))
tmap_mode(current.mode)

# save ----
tmap_save(
  tm = f.map,
  filename = paste(p.dir, "SwC_Map.png", sep ='/'),
  #width = NA,
  #height = NA,
  #units = NA,
  scale = 1,
  dpi = 300)


# set extent --
#ext1 <- extent( 114.1401 ,  115.8426, -34.64244, -32.87244)
#e <- drawExtent()
ext1 <- extent(114.1463, 115.8601, -34.63865, -33.07346)


### CLOSE UP MAP ----

plot(au2)
#e2 <- drawExtent()
e2 <- extent(114, 115.9504, -34.69117, -33.34007)
au3 <- crop(au2, e2)
plot(au3)

#e3 <- drawExtent()
e3 <- extent(114.5697,114.938,-34.13724,-33.94978)
study.area <- st_bbox(e3,
                      crs = st_crs(wa)) %>%
  st_as_sfc()
# crop polys ----
cmr1 <- crop(cmr, e2)
head(cmr1)
wa1 <- crop(wa, e2)
cw1 <- crop(cw, e2)
wamp1 <- crop(wamp, e2)
head(cw1)
head(mps)
plot(mps)



# Read land locations ----
df <- read.csv(paste(s.dir, "Locations.csv", sep = '/'))
df
# remove some if needed
df <- df[-c(2,3),]
df
dfs <- df
coordinates(dfs) <- ~Lon+Lat
#plot(dfs)
head(dfs)


### Map ----


map <- tm_shape(cmr, bbox=tmaptools::bb(e2)) + tm_borders(col ='white', lwd = 1.5) +
  #tm_compass(type = "arrow", position = c(0.04, 0.85), size = 2) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  #tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.8, position = c(0.03, 0.7)) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 4, n.y = 4, labels.size = 1, lines = FALSE) +
  tm_layout(bg.color = "lightcyan1",
            main.title = "b",
            main.title.size = 2,
            main.title.fontface = "bold",
            main.title.position = "left")

map

map4 <- map + tm_shape(wa) + tm_borders(col = 'black', lwd = 1.5) + tm_fill(col='lightgoldenrodyellow') # +
#tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 2)
map4

#pal1 <- c("#FFFF99","#99FFFF", 'chartreuse3',   'steelblue2', "#324ab2")
#levels(cmr$ZoneName)

map55 <- map4 + tm_shape(cmr) + tm_borders(col = 'grey35', lwd = 1.5) +
  tm_fill(col = 'ZoneName', palette = pal1, title = NA) +
  tm_legend(show = FALSE)
  #tm_shape(cw) + tm_lines(col = 'red', lwd = 1) +
  #tm_shape(wamp) + tm_fill(col = colst) +
  # tm_layout(legend.outside = TRUE,
  #   #legend.outside.size = 3,
  #   legend.outside.position = 'bottom',
  #   #legend.position = c(0.02, 0.04),
  #   legend.title.color = "white",
  #   legend.title.size = 0.9,
  #   legend.text.size = 0.6,
  #   #legend.outside = TRUE,
  #   #legend.width = 0.6,
  #   #legend.height = 0.5,
  #   #legend.bg.color = 'white'
  #   #legend.just = 'right',
  # ) #+
  #tm_add_legend(type = 'fill', labels = "State Marine Park", col = colst, lwd = 1, size = 1) +
  #tm_add_legend(type = 'line', labels = "Limit of coastal waters", col = 'red', lwd = 1, size = 1)

map55


map6 <- map55 + tm_shape(wamp1) + tm_fill(col = colst) +
  tm_shape(cw1) + tm_lines(col = 'red', lwd = 1) +
  tm_compass(type = "arrow", position = c(0.04, 0.15), size = 3) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.8, position = c(0.03, 0.05)) 
  #tm_graticules(ticks = FALSE)
map6


map7 <- map6 + tm_shape(dfs[1,]) + tm_dots(size = 0.1, shape = 21) + tm_text("Location", size = 1.2, 
                                                                             just = 'left', xmod = 0.4)
map7

map8 <- map7 + tm_shape(study.area) + tm_borders(col = 'black', lwd = 3, lty = 33)
map8


# save ----
tmap_save(
  tm = map8,
  filename = paste(p.dir, "SwC_Map5.png", sep ='/'),
  #width = NA,
  #height = NA,
  #units = NA,
  scale = 1,
  dpi = 300)


# map0 <- map + tm_shape(bathy2) + tm_raster(palette=viridis(40, direction =-1), style = 'cont', legend.reverse = TRUE) +
#   tm_layout(legend.text.size = 1.7,
#             legend.outside = TRUE,
#             legend.outside.position = 'right',
#             #legend.position = c(1, 0.05),
#             legend.title.size = 0.5,
#             legend.title.color = 'white',
#             legend.width = 1) 
# map0
            
            

map1 <- map + tm_shape(cmr1) + tm_borders(col = g2, lwd = 2) + tm_fill(col=g2) +
  tm_add_legend(type = 'fill', labels = "21,000 years ago", col = g2, lwd = 2, size = 2)

map1


map2 <- map1 + tm_shape(k70) + tm_borders(col = g5, lwd = 2) + tm_fill(col=g5) +
  tm_add_legend(type = 'fill', labels = "70,000 years ago", col = g5, lwd = 2, size = 2)
map2


map3 <- map2 + tm_shape(k11) + tm_borders(col = g4, lwd = 2) + tm_fill(g4) +
  tm_add_legend(type = 'fill', labels = "11,000 years ago", col = g4, lwd = 2, size = 2)
map3

map4 <- map3 + tm_shape(wa1) + tm_borders(col = 'black', lwd = 2) + tm_fill(col=sg) +
  tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 2)
map4

map5 <- map4 + tm_shape(dfs) + tm_symbols(col = 'black', size = 0.2) +
  tm_text("Location", size = 0.8, xmod = 1.5, ymod = -0.5)
map5

map55 <- map5 + tm_shape(cmr1) + tm_borders(col = 'black', lwd = 1.5) 
  
map55

# Option 1 ----

map6 <- map55 + tm_shape(SPZ) + tm_borders(col = 'black', lwd = 1.5) + 
  tm_add_legend(type = 'line', labels = "Special Purpose Zone", col = 'black', lwd = 2, size = 2)
map6

map7 <- map6 + tm_shape(MUZ) + tm_borders(col = 'gray', lwd = 1.5) + 
  tm_add_legend(type = 'line', labels = "Mutiple Use Zone", col = 'gray', lwd = 2, size = 2, lty ='33')
map7

map8 <- map7 + tm_shape(HPZ) + tm_borders(col = 'red', lwd = 1.5, lty ='33') + 
  tm_add_legend(type = 'line', labels = "Habitat Protection Zone", col = 'red', lwd = 2, size = 2, lty ='33')
map8

map9 <- map8 + tm_shape(NPZ) + tm_borders(col = 'green', lwd = 1.5, lty ='33') + 
  tm_add_legend(type = 'line', labels = "National Park Zone", col = 'green', lwd = 2, size = 2, lty ='33')
map9

# Option 2 ----
map6 <- map55 + tm_shape(SPZ) + tm_borders(col = 'black', lwd = 1.5)  
  #tm_add_legend(type = 'line', labels = "Special Purpose Zone", col = 'black', lwd = 2, size = 2)
map6

map7 <- map6 + tm_shape(NPZ) + tm_borders(col = 'green', lwd = 1.5, lty ='33') + 
  tm_add_legend(type = 'line', labels = "National Park Zone", col = 'green', lwd = 2, size = 2, lty ='33')
map7

map8 <- map7 + tm_shape(wamp1) + tm_borders(col = 'black', lwd = 1.5, lty ='33') + tm_fill(col='pink', alpha = 0.4) +
  tm_add_legend(type = 'fill', col = 'pink', alpha = 0.4, labels = 'State Sanctuary Zones')
map8

####

# Bathy options

map0 <- map + tm_shape(bathy2) + tm_raster( style= "quantile", n=7, palette=get_brewer_pal("Blues", n = 7, plot=FALSE), legend.reverse = T) +
  tm_layout(legend.text.size = 1.7,
            legend.outside = TRUE,
            legend.outside.position = 'right',
            #legend.position = c(1, 0.05),
            legend.title.size = 0.5,
            legend.title.color = 'white',
            legend.width = 1) 
map0

map0 <- map + tm_shape(bathy2) + tm_raster(title = 'Depth (m)', palette=get_brewer_pal("Blues", plot=FALSE), style='cont', legend.reverse = T) +
  tm_layout(legend.text.size = 1,
            legend.outside = TRUE,
            #legend.outside.position = 'right',
            legend.outside.position = 'bottom',
            #legend.position = c(1, 0.05),
            legend.title.size = 1.5,
            legend.title.color = 'black',
            legend.width = 1) 
map0


# save ----
tmap_save(
  tm = map8,
  filename = paste(p.dir, "SW-Sea-level1.png", sep ='/'),
  #width = NA,
  #height = NA,
  #units = NA,
  dpi = 300)


### VERSION TWO -----

# smooth polygons ---

k11sm <- st_as_sf(k11)
k11sm <- drop_crumbs(k11sm, set_units(2, km^2))
plot(k11sm)
k11sm <- fill_holes(k11sm, set_units(201, km^2))
k11sm <- smoothr::smooth(k11sm, method = "ksmooth", smoothness = 15)


k21sm <- st_as_sf(k21)
k21sm <- drop_crumbs(k21sm, set_units(2, km^2))
k21sm <- fill_holes(k21sm, set_units(201, km^2))
k21sm <- smooth(k21sm, method = "ksmooth", smoothness = 10)

wa1sm <- st_as_sf(wa1)
wa1sm <- smoothr::drop_crumbs(wa1sm, set_units(2, km^2))
#wa1sm <- fill_holes(wa1sm, set_units(201, km^2))
wa1sm2 <- smoothr::smooth(wa1sm, method = 'ksmooth')
plot(wa1sm2) # THIS ISN'T WORKING



map <- tm_shape(cmr1, bbox=ext1)  + tm_borders(col ='white', lwd = 1.5) +
  tm_compass(type = "arrow", position = c(0.23, 0.1), size = 2) +
  #tm_fill(col ='ZONENAME', palette=c('yellow', 'red'), alpha = 0.1) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, position = c(0.2, 0.02))  
  #tm_graticules(ticks = FALSE) +
  #tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map0 <- map + tm_shape(bathy1) + tm_raster(title = 'Depth (m)', palette=get_brewer_pal("Blues", plot=FALSE), style='cont', legend.reverse = T) +
  #tm_add_legend(type = 'fill', labels = "xxx", col = g2, lwd = 2, size = 2) +
 tm_layout(outer.margins = 0.01,
           inner.margins = 0,
            legend.text.size = 1,
            #legend.outside = FALSE,
            #legend.outside.position = 'right',
            #legend.outside.position = 'right',
            #legend.stack = 'horizontal',
            #legend.is.protrait = TRUE,
            legend.position = c(0.01, 0.72),
            legend.title.size = 1.5,
            legend.title.color = 'black',
            legend.width = 1)
map0



map1 <- map0 + tm_shape(k21sm) + tm_borders(col = g2, lwd = 2) + tm_fill(col=g2) 
  #tm_add_legend(type = 'fill', labels = "21,000 years ago", col = g2, lwd = 2, size = 2)

map1


#map2 <- map1 + tm_shape(k70) + tm_borders(col = g5, lwd = 2) + tm_fill(col=g5) +
 # tm_add_legend(type = 'fill', labels = "70,000 years ago", col = g5, lwd = 2, size = 2)
#map2


map3 <- map1 + tm_shape(k11sm) + tm_borders(col = g4, lwd = 2) + tm_fill(g4) 
 # tm_add_legend(type = 'fill', labels = "11,000 years ago", col = g4, lwd = 2, size = 2)
map3

map4 <- map3 + tm_shape(wa1sm) + tm_borders(col = 'grey70', lwd = 2) + tm_fill(col=sg) 
  #tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 2)
map4

#map5 <- map4 + tm_shape(dfs) + tm_symbols(col = 'black', size = 0.2) +
#  tm_text("Location", size = 0.8, xmod = 1.5, ymod = -0.5)
#map5

map55 <- map4 + tm_shape(cmr1) + tm_borders(col = 'grey55', lwd = 0.5) 

map55


#map6 <- map55 + tm_shape(SPZ) + tm_borders(col = 'black', lwd = 1.5)  
#tm_add_legend(type = 'line', labels = "Special Purpose Zone", col = 'black', lwd = 2, size = 2)
#map6

map7 <- map55 + tm_shape(NPZ) + tm_borders(col = "#006600", lwd = 1.5) 
  #tm_add_legend(type = 'fill', labels = "National Park Zone and State Sanctuary Zones", col = 'white', border.col= "#006600", border.lwd = 3, size = 2)
                
map7

map71 <- map7 + tm_shape(dfs) + tm_symbols(col = 'black', size = 0.2) +
  tm_text("Location", size = 0.8, xmod = 1.4, ymod = -0.5)
map71

map8 <- map71 + tm_shape(wamp1) + tm_borders(col = "#006600", lwd = 1.5)  #+ tm_fill(col='pink', alpha = 0.4) +
  #tm_add_legend(type = 'fill', col = 'white', border.col = 'white', labels = 'State Sanctuary Zones')
map8

map9 <- map8 + tm_shape(cw1) + tm_lines(col = 'red', lwd = 1) 
  # #tm_add_legend(type = 'fill', col = 'white', border.col = 'red', border.lwd =3, labels = 'Commonwealth/State waters boundary') +
  # tm_layout(#outer.margins = 0.1,
  #   legend.text.size = 1,
  #   #legend.outside = TRUE,
  #   #legend.outside.position = 'right',
  #   #legend.outside.position = 'right',
  #   #legend.only = TRUE,
  #   legend.title.size = 1.5,
  #   legend.title.color = 'black',
  #   legend.width = 1) +
  # tm_add_legend(type = 'fill', labels = "21,000 years ago (-125 metres)", col = g2, lwd = 2, size = 2) + 
  # tm_add_legend(type = 'fill', labels = "11,000 years ago (-40 metres)", col = g4, lwd = 2, size = 2) +
  # tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 2) +
  # tm_add_legend(type = 'fill', col = 'white', border.col = 'red', border.lwd =3, labels = 'Commonwealth/State waters boundary', size = 3) +
  # tm_add_legend(type = 'fill', labels = "National Park Zone and State Sanctuary Zones", col = 'white', border.col= "#006600", border.lwd = 3, size = 2)
 
map9


map.leg <- tm_shape(cmr1) + tm_borders(col ='white', lwd = 1.5) +
  #tm_shape(cw1) + tm_lines(col = 'red', lwd = 1) +
   tm_layout(#outer.margins = 0.1,
  legend.text.size = 2,
    #legend.outside = TRUE,
  #   #legend.outside.position = 'right',
  #   #legend.outside.position = 'right',
  legend.only = TRUE) +
  #   legend.title.size = 1.5,
  #   legend.title.color = 'black',
   #legend.width = 1) +
  tm_add_legend(type = 'fill', labels = "21,000 years ago (-125 metres)", col = g2, lwd = 2, size = 4) + 
  tm_add_legend(type = 'fill', labels = "11,000 years ago (-40 metres)", col = g4, lwd = 2, size = 4) +
  tm_add_legend(type = 'fill', labels = "Coastline today", col = sg, lwd = 2, size = 4) +
  tm_add_legend(type = 'fill', col = 'white', border.col = 'red', border.lwd =3, labels = 'Commonwealth/State waters boundary', size = 4) +
  tm_add_legend(type = 'fill', labels = "National Park Zone and State Sanctuary Zones", col = 'white', border.col= "#006600", border.lwd = 3, size = 4)
 

map.leg

####


# save ----
tmap_save(
  tm = map9,
  filename = paste(p.dir, "SW-Sea-level5.png", sep ='/'),
  width = 2000,
  height = 2200,
  #units = NA,
  #scale = 1,
  dpi = 300)



#### To create the inset map ----

### 1. define area of interest: part of WA ####

sw_region <- st_bbox(ext1,
                     crs = st_crs(bathy1)) %>%
  st_as_sfc()

### 1.1 define area of interest: Rottnest Island ####
extent(ri)



ri_region <- st_bbox(c(xmin = 115.4242, xmax = 115.5675,
                       ymin = -32.06073 , ymax = -31.97739),
                     crs = st_crs(ri)) %>%
  st_as_sfc()

### 2.1 create a base map: Austalia ###

# read Australia shapefile ----
a <- readOGR(paste(s.dir, "STE11aAust.shp", sep='/'))
plot(a)
asm <- st_as_sf(a)
asm <- drop_crumbs(asm, set_units(10, km^2))
plot(asm)
asm2 <- fill_holes(asm, set_units(20, km^2))
asm3 <- smoothr::smooth(asm2, method = "ksmooth", smoothness = 20)
plot(asm3)

a_map <- tm_shape(wa, bbox = wa_region) + tm_polygons(border.col = "black")
wa2_map

