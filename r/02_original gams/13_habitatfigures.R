###
# Project: MAC Hub - SWC
# Data:    Habitat predictions
# Task:    Habitat figures
# author:  Kingsley Griffin
# date:    Mar 2022
##

rm(list=ls())

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

habi    <- readRDS('data/tidy/habitat_merged.rds')
# habi$ns <- ifelse(habi$Latitude.1 > 6940000, 1, 0)
habi$method <- dplyr::recode(habi$method,
                             BOSS = "Drop Camera")

# read in outputs from 'R/4_habitat_model.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/habitat_fssgam/site_habitat_predictions.rds")                       # site predictions only
head(spreddf)

spreddf$dom_tag <- as.factor(spreddf$dom_tag)
unique(spreddf$dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                          macroalgae = "Macroalgae",
                          sand = "Sand",
                          biogenic = "Biogenic Reef",
                          rock = "Rock",
                          sponge = "Sponge")
  
# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Macroalgae" = "darkgoldenrod4",
                                         "Sponge" = "darkorange1",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Biogenic Reef" = "plum"))

p4 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  geom_point(data = habi,
  aes(longitude.1, latitude.1, colour = method),
  shape = 10, size = 1, alpha = 1/5) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "navyblue")) +
  labs(fill = "Habitat", colour = "Sample", x = NULL, y = NULL) +
  coord_sf() +
  theme_minimal()
p4
a
ggsave("plots/original gamms/fullarea_dominant_habitat.png", 
       width = 12, height = 8, dpi = 160)

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- melt(spreddf, measure.vars = c(8:12))
widehabit$variable <- dplyr::recode(widehabit$variable,
                                    psponge = "Sponge",
                                    pmacroalgae = "Macroalgae",
                                    prock = "Rock",
                                    psand = "Sand",
                                    preef = "Biogenic Reef",
                                    pseagrass = "Seagrass")

p2 <- ggplot() +
  geom_tile(data = widehabit, 
            aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL, fill = "Occurrence (p)") +
  theme_minimal() +
  facet_wrap(~variable)
p2

ggsave("plots/original gamms/fullarea_habitat_predicted.png", 
       width = 12, height = 14, dpi = 160)

# # fig 3: biogenic reef
# p3 <- ggplot(spreddf[widehabit$sitens == 1, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL) +
#   coord_equal() +
#   guides(fill = "none") +
#   theme_minimal()
# 
# p32 <- ggplot(spreddf[widehabit$sitens == 0, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL, fill = "Biogenic\nReef (p)") +
#   coord_equal() +
#   theme_minimal()
# 
# p3 + p32 + plot_layout(widths = c(0.46, 0.54))
# ggsave("plots/site_biogenicreef_p.png", width = 10, height = 6, dpi = 160)

# adding spatial layers to the relief plot below, bit long as need separate scale + legend
colnames(spreddf)[3] <- "depth"
# pred_df <- melt(spreddf, id.vars = c(1:2, 14))
# pred_df <- pred_df[pred_df$variable %in% c("depth", "tpi", 
#                                            "roughness","detrended"), ]
# pred_df$value <- as.numeric(pred_df$value)

# depth
pd <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = depth)) +
  scale_fill_viridis(option = "A", direction = -1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "Depth (m)") +
  theme_minimal() 
pd

# tpi
pt <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = tpi)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "TPI") +
  theme_minimal()
pt

# roughness
pr <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "Roughness") +
  theme_minimal() 
pr

# detrended
pdt <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "D", direction = 1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = "Detrended (m)") +
  theme_minimal()
pdt

# fig 4: predicted relief
pcelldf <- readRDS('output/spatial/raster/predicted_relief_site.rds')

p4 <- ggplot() +
  geom_tile(data = pcelldf, aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "Relief (predicted)") +
  theme_minimal()

# relief only
p4


# combined spatial layers

(pd + pt ) /
  (pr + pdt) /
  (p4 + plot_spacer()) +
  theme(text = element_text(size = 8))

ggsave("plots/original gamms/site_spatial_layers.png", width = 12, height = 15, dpi = 160)


# fig 4.1.2: spatial random effect

p5 <- ggplot() +
  geom_tile(data = pcelldf, aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  geom_point(data = habi, aes(longitude.1, latitude.1), 
             alpha = 0.7, colour = "grey70", size = 1, shape = 3) +
  labs(x= NULL, y = NULL, fill = "spatial\ndependence") +
  theme_minimal()
p5

ggsave("plots/original gamms/site_relief_spatialeffect.png", 
       width = 10, height = 6, dpi = 160)

