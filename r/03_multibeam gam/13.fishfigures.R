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

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                             # just Abrolhos Aus MP
swc_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
swc_npz$parkid <- c(1:7)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
swc_npz <- st_transform(swc_npz, sppcrs)

# read in outputs from 'R/habitat_fish_model_predict.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/fish gamms/site_fish_predictions.rds")                       # site predictions only
#spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# plotting broad maps
#total abundance
p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.4,114.6,114.8,115.0))+
  labs(x = NULL, y = NULL, fill = "Total Abundance")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p11

#species richness
p21 <- ggplot() +
  geom_raster(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.4,114.6,114.8,115.0))+
  labs(x = NULL, y = NULL, fill = "Species Richness")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = spreddf%>%filter(p_legal<5), aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.4,114.6,114.8,115.0))+
  labs(x = NULL, y = NULL, fill = "Legal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.4,114.6,114.8,115.0))+
  labs(x = NULL, y = NULL, fill = "Sublegal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p41

gg.predictions.npz <- p11+p21+p31+p41 & theme(legend.justification = "left")    #, aspect.ratio=1
gg.predictions.npz

ggsave("plots/original gamms/site_fish_predictions.png", gg.predictions.npz,width = 10, height = 4, dpi = 160)
