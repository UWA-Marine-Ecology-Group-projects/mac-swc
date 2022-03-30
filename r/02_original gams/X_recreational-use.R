
###
# Project: MAC HUB South-west Corner
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Kingsley Griffin
# date:    Mar 2022
##

rm(list=ls())

library(dplyr)
library(ggplot2)
library(scatterpie)
library(viridis)
library(sf)
library(raster)
library(ggnewscale)
library(metR)
library(cowplot)

working.dir <- getwd()
setwd(working.dir)

#define crs
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# bring in marine parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")%>%           # all aus mpas
  dplyr::filter(ResName%in%"South-west Corner",ZoneIUCN%in%"II")
#bring in state MP
wampa  <- st_read("data/spatial/shapefiles/test1.shp", 
                  crs = wgscrs)%>%
  dplyr::mutate(sanctuary = "Sanctuary Zone")

# get aus outline data
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == "mainland", ]
st_crs(aus)         <- st_crs(aumpa)
                                          
dat <- st_read("data/spatial/shapefiles/Capes_rec-use.shp")%>%
  dplyr::mutate(Perc_trips = ifelse(is.na(Perc_trips),0,Perc_trips))

#bring in bathy for contour lines
bath_r <- raster("data/spatial/rasters/archive/GB-SW_250mBathy.tif")            # bathymetry trimmed to project area
bathdf <- as.data.frame(bath_r, na.rm = TRUE, xy = TRUE)
colnames(bathdf)[3] <- "Depth"
bathdf <- bathdf %>% dplyr::rename("Longitude" = x, "Latitude" = y)                          # trim to reduce legend
# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1")) 

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

#class colours 
hab_cols <- scale_fill_manual(values = c("Rock" = "grey40",
                              "Biogenic reef" = "plum",
                              "Macroalgae" = "darkgoldenrod4",
                              "Seagrasses" = "forestgreen",
                              "Sand" = "wheat"))

#depth colours 
depth_cols <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", "#a3bbff"),guide = "none")
#shallow to deep

#make the plot
gg.recuse <- ggplot() + 
  geom_sf(data = dat%>%dplyr::filter(Perc_trips>0), aes(fill = Perc_trips), colour = "black", size = 0.1) + #
  scale_fill_gradient(low = "#d0d1f2", high = "#00058a")+ #
  labs(fill = "Percentage of trips")+
  new_scale_fill()+
  geom_sf(data = dat%>%dplyr::filter(Perc_trips%in%0), fill = "#ffffff", colour = "black", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wampa,color = "#bfd054", alpha = 2/5, fill = NA, size = 0.8)+
  geom_sf(data = aumpa, color = "#7bbc63",alpha = 2/5, fill = NA, size = 1) +
  coord_sf(xlim = c(113.8334, 115.8331), ylim = c(-34.6664, -33.1667))+
  scale_fill_gradient(low = "#ffffff", high = "#00058a")+
  labs(x = "Longitude", y = "Latitude")+
  theme_minimal()
gg.recuse

save_plot("plots/spatial/rec-use.png", gg.recuse,base_height = 6.5,base_width = 7)

