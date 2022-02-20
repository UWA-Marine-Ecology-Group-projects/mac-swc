###
# Project: Parks - Abrolhos Post-Survey
# Data:    BOSS Fish data
# Task:    Fish figures - predictions
# author:  Kingsley Griffin
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
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("Abrolhos"), ]                             # just Abrolhos Aus MP
ab_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
ab_npz$parkid <- c(1:3)
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
ab_npz <- st_transform(ab_npz, sppcrs)

# read in outputs from 'R/habitat_fish_model_predict.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/site_fish_predictions.rds")                       # site predictions only
spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# plotting broad maps
#npz6
#total abundance
p11 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_totabund6)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Total Abundance")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p11

#species richness
p21 <- ggplot() +
  geom_raster(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_richness6)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Species Richness")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_legal6)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Legal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = p_sublegal6)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113.2,113.4,113.6))+
  labs(x = NULL, y = NULL, fill = "Sublegal")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p41

#npz9
#total abundance
p1 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_totabund9)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+
  labs(x = NULL, y = NULL, fill = "Total Abundance")+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
p1

#species richness
p2 <- ggplot() +
  geom_raster(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_richness9)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Species Richness") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
p2

# greater than legal size
p3 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_legal9)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Legal") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p3

#smaller than legal size
p4 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = p_sublegal9)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Sublegal") +
  scale_x_continuous(breaks = c(113,113.10,113.2,113.3))+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p4

# gg.predictions.npz6 <- plot_grid(NULL,NULL,p11,p21,NULL,NULL,p31,p41,NULL,NULL,
#                                  ncol = 2, align = "vh",rel_heights = c(-0.3,1,-0.5,1,0))

gg.predictions.npz6 <- p11+p21+p31+p41 & theme(legend.justification = "left")    #, aspect.ratio=1
gg.predictions.npz6

gg.predictions.npz9 <- p1+p2+p3+p4 & theme(legend.justification = "left")       #, aspect.ratio=1
gg.predictions.npz9

ggsave("plots/site_fish_predictions-npz6.png", gg.predictions.npz6,width = 10, height = 4, dpi = 160)
ggsave("plots/site_fish_predictions-npz9.png", gg.predictions.npz9,width = 10, height = 4, dpi = 160)
