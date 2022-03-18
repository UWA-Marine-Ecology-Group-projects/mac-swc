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

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                             # just Abrolhos Aus MP
swc_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
swc_npz$parkid <- c(1:7)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
swc_npz <- st_transform(swc_npz, sppcrs)

# read in outputs from 'R/habitat_fish_model_predict.R'
file.names <- list.files(path="output/multibeam fish gamms",
                         pattern = '*.rds',full.names = T)   #split up as too big for git
spreddf <- file.names %>%
  map_dfr(readRDS)
                
# plotting broad maps
#total abundance
p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9),limits = c(min(spreddf$x),max(spreddf$x)))+
  scale_y_continuous(limits = c(min(spreddf$y),max(spreddf$y)))+
  labs(x = NULL, y = NULL, fill = "Total Abundance")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# p11

#species richness
p21 <- ggplot() +
  geom_raster(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9),limits = c(min(spreddf$x),max(spreddf$x)))+
  scale_y_continuous(limits = c(min(spreddf$y),max(spreddf$y)))+
  labs(x = NULL, y = NULL, fill = "Species Richness")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = spreddf%>%filter(p_legal<5), aes(x, y, fill = p_legal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9),limits = c(min(spreddf$x),max(spreddf$x)))+
  scale_y_continuous(limits = c(min(spreddf$y),max(spreddf$y)))+
  labs(x = NULL, y = NULL, fill = "Legal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_sublegal)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = swc_npz[swc_npz$parkid == 4, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(114.7,114.8,114.9),limits = c(min(spreddf$x),max(spreddf$x)))+
  scale_y_continuous(limits = c(min(spreddf$y),max(spreddf$y)))+
  labs(x = NULL, y = NULL, fill = "Sublegal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# p41

gg.predictions.npz <- p11+p21+p31+p41 & theme(legend.justification = "left")    #, aspect.ratio=1
gg.predictions.npz

#clear out the memory
# rm(list= ls()[!(ls() %in% c('gg.predictions.npz'))])
# gc()

ggsave("plots/multibeam gamms/site_fish_predictions.png", gg.predictions.npz,width = 9, height = 4, dpi = 300)
