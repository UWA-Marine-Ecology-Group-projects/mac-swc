###
# Project: mac - South-west Corner
# Data:    BRUVS, BOSS Habitat & fish data
# Task:    Fish figures - predictions
# author:  Kingsley Griffin & Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

# library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)
library(cowplot)
library(purrr)

#Theme
Theme1 <-
  theme( # use theme_get() to see available options
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    #legend.title = element_blank(),
    #legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    #axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    #axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                             # just Abrolhos Aus MP
swc_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
swc_npz$parkid <- c(1:7)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes
# wanew <- wanew[wanew$Name != c("Cosy Corner Sanctuary Zone","Hamelin Island Sanctuary Zone","Hamelin Bay Recreation Zone"),]
st_crs(wanew) <- crs(wgscrs)
#remove state sanctuary zones outside of the prediction area
wanew <- st_crop(wanew, c(xmin = 114.8, xmax = 115.2, ymin = -34.2, ymax = -33.6))

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 123, ymin = -39, ymax = -30))      # crop down coastal waters line to general project area

#bring in bathy for contour lines
bath_r <- raster("data/spatial/rasters/multibeam_derivatives_depth.tif")        # bathymetry trimmed to project area
bath_agg <- aggregate(bath_r, fact = 10, fun = "mean")
bath_t <- projectRaster(bath_agg, crs = wgscrs)                                   # transform before convert to dataframe
bathdf <- as.data.frame(bath_t, na.rm = TRUE, xy = TRUE)
colnames(bathdf)[3] <- "Depth"

#bring in state MPs to mask out the indjidup sanctuary
#same file as above but can't be bothered rewriting code for now
# wampa  <- st_read("data/spatial/shapefiles/test1.shp", 
#                   crs = wgscrs)%>%
#   dplyr::filter(Name%in%c("Injidup Sanctuary Zone","Kilcarnup Sanctuary Zone",
#                           "Cape Freycinet Sanctuary Zone"))

# read in outputs from 'R/habitat_fish_model_predict.R'
#mask them by the state sanctuary zones
#who knows wtf this does
wanew <- st_zm(wanew)

file.names <- list.files(path="output/multibeam fish gamms",
                         pattern = '*.rds',full.names = T)   #split up as too big for git
spreddf <- file.names %>%
  map_dfr(readRDS)

#total abundance
p_totabund <- spreddf %>%
  dplyr::select(x,y,p_totabund)
#rasterise
p_totabund <- rasterFromXYZ(p_totabund, crs = sppcrs)
#reproject
p_totabund <- projectRaster(p_totabund,crs = wgscrs)
#mask
p_totabund <- raster::mask(p_totabund,wanew, inverse = T)
#convert to dataframe
p_totabund <- as.data.frame(p_totabund, xy = T, na.rm = T)

#species richness
p_richness <- spreddf %>%
  dplyr::select(x,y,p_richness)
#rasterise
p_richness <- rasterFromXYZ(p_richness, crs = sppcrs)
#reproject
p_richness <- projectRaster(p_richness,crs = wgscrs)
#mask
p_richness <- raster::mask(p_richness,wanew, inverse = T)
#convert to dataframe
p_richness <- as.data.frame(p_richness, xy = T, na.rm = T)

#legal
p_legal <- spreddf %>%
  dplyr::select(x,y,p_legal)
#rasterise
p_legal <- rasterFromXYZ(p_legal, crs = sppcrs)
#reproject
p_legal <- projectRaster(p_legal,crs = wgscrs)
#mask
p_legal <- raster::mask(p_legal,wanew, inverse = T)
#convert to dataframe
p_legal <- as.data.frame(p_legal, xy = T, na.rm = T)

#sublegal
p_sublegal <- spreddf %>%
  dplyr::select(x,y,p_sublegal)
#rasterise
p_sublegal <- rasterFromXYZ(p_sublegal, crs = sppcrs)
#reproject
p_sublegal <- projectRaster(p_sublegal,crs = wgscrs)
#mask
p_sublegal <- raster::mask(p_sublegal,wanew, inverse = T)
#convert to dataframe
p_sublegal <- as.data.frame(p_sublegal, xy = T, na.rm = T)                

# plotting
#total abundance
p11 <- ggplot() +
  geom_tile(data = p_totabund, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("text", x = 114.764, y = -34.08, label = "70m", 
           size = 1.8, colour = "grey54")+
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9))+
  labs(x = NULL, y = NULL, fill = "Total Abundance", title = "Whole assemblage")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  coord_sf(xlim = c(min(p_totabund$x), max(p_totabund$x)), ylim = c(min(p_totabund$y), max(p_totabund$y)))+
  Theme1
p11

#species richness
p21 <- ggplot() +
  geom_raster(data = p_richness, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("text", x = 114.764, y = -34.08, label = "70m", 
           size = 1.8, colour = "grey54")+
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9))+
  labs(x = NULL, y = NULL, fill = "Species Richness")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  coord_sf(xlim = c(min(p_richness$x), max(p_richness$x)), ylim = c(min(p_richness$y), max(p_richness$y)))+
  Theme1

p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = p_legal, aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("text", x = 114.764, y = -34.08, label = "70m", 
           size = 1.8, colour = "grey54")+
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9))+
  labs(x = NULL, y = NULL, fill = "Legal", title = "Targeted assemblage")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  coord_sf(xlim = c(min(p_legal$x), max(p_legal$x)), ylim = c(min(p_legal$y), max(p_legal$y)))+
  Theme1

p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = p_sublegal, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("text", x = 114.764, y = -34.08, label = "70m", 
           size = 1.8, colour = "grey54")+
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9))+
  labs(x = NULL, y = NULL, fill = "Sublegal")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  coord_sf(xlim = c(min(p_sublegal$x), max(p_sublegal$x)), ylim = c(min(p_sublegal$y), max(p_sublegal$y)))+
  Theme1

p41

gg.predictions.npz <- p11+p21+p31+p41 & theme(legend.justification = "left")   
png(file = "plots/multibeam gamms/site_fish_predictions.png",width = 9, height = 4, res = 300, units = "in")
gg.predictions.npz
dev.off()
