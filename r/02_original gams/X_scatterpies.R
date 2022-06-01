
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
                                        
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 123, ymin = -39, ymax = -30))      # crop down coastal waters line to general project area

dat <- readRDS("data/tidy/dat.full.habitat.rds")%>%
  dplyr::select(26, 25, 3:17)%>%
  dplyr::mutate(biogenic_reef = broad.ascidians + broad.bryozoa +
                  broad.crinoids + broad.hydroids +
                  broad.invertebrate.complex +
                  broad.octocoral.black + broad.sponges +
                  broad.stony.corals + broad.true.anemones)%>%
  dplyr::mutate(grouping = factor(1:727))%>%
  dplyr::rename("Sessile invertebrate communities" = biogenic_reef,
                "Rock" = broad.consolidated,
                "Macroalgae" = broad.macroalgae,
                "Seagrasses" = broad.seagrasses,
                "Sand" = broad.unconsolidated)%>%
  glimpse()

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
                              "Sessile invertebrate communities" = "plum",
                              "Macroalgae" = "darkgoldenrod4",
                              "Seagrasses" = "forestgreen",
                              "Sand" = "wheat"))

#depth colours 
depth_cols <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", "#a3bbff"),guide = "none")
#shallow to deep

#make the plot
gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(Longitude, Latitude, z = Depth, fill = after_stat(level)), color = "black",
               breaks = c(-30, -70, -200,-700,-10000), size = 0.1) +
  annotate("text", x = c(114.40,114.467,114.72,114.945), y = -33.85, label = c("700m","200m","70m","30m"), size = 2)+
  # scale_fill_grey(start = 0.5, end = 0.8) +
  depth_cols+
  new_scale_fill()+
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wampa,fill = "#bfd054", alpha = 2/5, color = NA)+
  wampa_cols+
  labs(fill = "State Marine Parks")+
  new_scale_fill()+
  geom_sf(data = aumpa, fill = "#7bbc63",alpha = 2/5, color = NA) +
  labs(fill = "Australian Marine Parks")+
  nmpa_cols+
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  new_scale_fill()+
  geom_scatterpie(aes(x=longitude, y=latitude, group=grouping), data=dat,
                  cols = c("Rock","Sessile invertebrate communities","Macroalgae",
                           "Seagrasses", "Sand"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude')+
  hab_cols+
  coord_sf(xlim = c(114.4, 115.1), ylim = c(-34.15, -33.65))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#b9d1d6"),
                                        panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank())
gg.scatterpie

save_plot("plots/spatial/scatterpies.png", gg.scatterpie,base_height = 6.5,base_width = 7)

