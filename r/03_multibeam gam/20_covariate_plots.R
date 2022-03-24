###
# Project: MAC Hub - SWC
# Data:    Covariate data and predictions
# Task:    Covariate figures
# author:  Kingsley Griffin
# date:    Mar 2022
##

rm(list=ls())
gc()

library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(ggnewscale)
library(sf)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
nb_nmp <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                    # regional nat parks networks
nb_npz <- nb_nmp[(nb_nmp$ZoneName == "National Park Zone" & 
                    nb_nmp$Area_KM2 < 1000), ]                                  # isolate local AMP
nb_npz <- nb_npz[2, ]

wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                    # all wa mpas
nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
nb_npz <- st_transform(nb_npz, sppcrs)

habi    <- readRDS("data/tidy/habitat_multibeam_merged.rds")
# habi$ns <- ifelse(habi$Latitude.1 > 6940000, 1, 0)
habi$method <- dplyr::recode(habi$method,
                             BOSS = "Drop Camera")

# read in outputs from 'R/03_multibeam gam/16_habitat_model.R'
# bring in multibeam derivatives and extract at sample locations
deriv_list <- list.files("data/spatial/rasters", "multibeam_derivatives",
                         full.names = TRUE)
mb_deriv   <- stack(deriv_list)
mb_deriv   <- aggregate(mb_deriv, 10, fun = mean)
names(mb_deriv) <- c("mb_depth", "mb_detrended", "mb_roughness", "mb_tpi")
spreddf <- as.data.frame(mb_deriv, xy = TRUE, na.rm = TRUE)

# adding spatial layers to the relief plot below, bit long as need separate scale + legend

# depth
pd <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = mb_depth)) +
  scale_fill_viridis(option = "A", direction = -1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "Depth (m)") +
  theme_minimal() +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))
pd

# tpi
pt <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = mb_tpi)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "TPI") +
  theme_minimal() +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))
pt

# roughness
pr <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = mb_roughness)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "Roughness") +
  theme_minimal()  +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))
pr

# detrended
pdt <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = mb_detrended)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL, fill = "Detrended (m)") +
  theme_minimal() +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))
pdt

# fig 4: predicted relief
# pcelldf <- readRDS('output/spatial/raster/predicted_relief_multibeam_site.rds')
prelief <- raster("output/spatial/raster/predicted_relief_multibeam_prelief.tif")
prelief <- aggregate(prelief, 10, fun = mean)
pcelldf <- as.data.frame(prelief, xy = TRUE, na.rm = TRUE)
names(pcelldf)[3] <- "prelief"

p4 <- ggplot() +
  geom_tile(data = pcelldf, aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL, fill = "Relief (predicted)") +
  theme_minimal() +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))

# relief only
p4


# combined spatial layers

(pd + pt ) /
  (pr + pdt) /
  (p4 + plot_spacer()) +
  theme(text = element_text(size = 8))

ggsave("plots/multibeam gamms/multibeam_site_spatial_layers.png", width = 12, height = 10, dpi = 160)


# fig 4.1.2: spatial random effect
pserelief <- raster("output/spatial/raster/predicted_relief_multibeam_p_sp.tif")
pserelief <- aggregate(pserelief, 10, fun = mean)
psecelldf <- as.data.frame(pserelief, xy = TRUE, na.rm = TRUE)
names(psecelldf)[3] <- "prelief_se"

p5 <- ggplot() +
  geom_tile(data = psecelldf, aes(x, y, fill = prelief_se)) +
  scale_fill_viridis(option = "B") +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  geom_point(data = habi, aes(longitude.1, latitude.1), 
             alpha = 0.7, colour = "grey70", size = 1, shape = 3) +
  labs(x= NULL, y = NULL, fill = "spatial\ndependence") +
  theme_minimal() +
  coord_sf(xlim = c(288666, 311266), ylim = c(6220394, 6234274))
p5

ggsave("plots/multibeam gamms/multibeam_site_relief_spatialeffect.png", 
       width = 10, height = 6, dpi = 160)
