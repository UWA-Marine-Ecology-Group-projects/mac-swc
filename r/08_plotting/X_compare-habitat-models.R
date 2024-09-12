###
# Project: MAC Hub - SWC
# Data:    Habitat predictions
# Task:    Habitat figures
# author:  Kingsley Griffin & Claude
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
library(dplyr)
# library(rgdal)

sf_use_s2(F)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
nb_nmp <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                    # regional nat parks networks
nb_npz <- nb_nmp[(nb_nmp$ZoneName == "National Park Zone" & 
                    nb_nmp$Area_KM2 < 1000), ]                                  # isolate local AMP
nb_npz <- nb_npz[2, ]

wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                    # all wa mpas
nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes
# wanew <- wanew[wanew$Name != c("Cosy Corner Sanctuary Zone","Hamelin Island Sanctuary Zone","Hamelin Bay Recreation Zone"),]
st_crs(wanew) <- crs(wampa)
#remove state sanctuary zones outside of the prediction area
wanew <- st_crop(wanew, c(xmin = 114.8, xmax = 115.2, ymin = -34.2, ymax = -33.6)) 

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 123, ymin = -39, ymax = -30))      # crop down coastal waters line to general project area

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
nb_npz <- st_transform(nb_npz, sppcrs)

#bring in bathy for contour lines
bath_r <- raster("data/spatial/rasters/archive/GB-SW_250mBathy.tif")            # bathymetry trimmed to project area
bath_t <- projectRaster(bath_r, crs = sppcrs)                                   # transform before convert to dataframe
bathdf <- as.data.frame(bath_t, na.rm = TRUE, xy = TRUE)
colnames(bathdf)[3] <- "Depth"

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
                                 biogenic = "Sessile invertebrates",
                                 rock = "Rock",
                                 sponge = "Sessile invertebrates")  # recoding to make sponge biogenic, as biogenic includes sponge

# Export this as a raster and shapefile for IMAS
# As a raster
unique(spreddf$dom_tag)

dom.hab <- spreddf %>%
  dplyr::select(x, y, dom_tag) %>%
  dplyr::mutate(dom_tag = recode(dom_tag,
                                 "Macroalgae" = 0,
                                 "Sand" = 1,
                                 "Sessile invertebrates" = 2,
                                 "Rock" = 3)) %>%
  glimpse()

# dom.habr <- rasterFromXYZ(dom.hab)
# plot(dom.habr)
# 
# writeRaster(dom.habr, "data/tidy/2020-2021_south-west_BOSS-BRUV_domhab-broad.tif")

# As a shapefile
# dom.habs <- rasterToPolygons(dom.habr, dissolve = T)
# 
# writeOGR(dom.habs, "data/tidy/2020-2021_south-west_BOSS-BRUV_domhab-broad.shp", 
#          layer = "dom_tag", driver = "ESRI Shapefile")

# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Macroalgae" = "darkgoldenrod4",
                                         # "Sponge" = "darkorange1",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Sessile invertebrates" = "plum"),
                              name = "Habitat")

rastnames  <- list.files("output/multibeam_habitat_fssgam", '*.tif', 
                         full.names = TRUE)                                     #split up as too big for git
phab_rasts <- stack(rastnames[-3])                                              # not reef
mbhab <- as.data.frame(phab_rasts, xy = TRUE, na.rm = TRUE)
mbhab$dom_tag <- apply(mbhab[c(3:8)], 1,
                         FUN = function(x){names(which.max(x))})
mbhab$dom_tag <- gsub("layer_p", "", mbhab$dom_tag)                          # tidy tag labels
mbhab$dom_tag <- as.factor(mbhab$dom_tag)
unique(mbhab$dom_tag)
mbhab$dom_tag <- dplyr::recode(mbhab$dom_tag,
                                 macroalgae = "Macroalgae",
                                 sand = "Sand",
                                 biogenic = "Sessile invertebrates",
                                 # seagrass = "Seagrass",
                                 rock = "Rock")

# convert to data frame
mbhab <- as.data.frame(phab_rasts, xy = TRUE, na.rm = TRUE)

ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
  hab_cols +
  new_scale_fill() +
  geom_tile(data = mbhab, aes(x, y, fill = dom_tag), show.legend = F) +
  hab_cols +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  # geom_sf(data = wampa, fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  # geom_point(data = habi,aes(longitude.1, latitude.1, colour = method),shape = 10, size = 1, alpha = 1/5) +
  # scale_colour_manual(values = c("BRUV" = "indianred4",
  #                                "Drop Camera" = "navyblue")) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("rect", xmin = 288666, xmax = 311266, ymin = 6220394, ymax = 6234274,
           colour = "goldenrod1", fill = NA, alpha = 0.1, linewidth = 1) +
  annotate("text", x = c(265000,290000,310000), y = 6240000, label = c("200m","70m","30m"), size = 2, colour = "grey54")+
  labs(fill = "Habitat", colour = "Sample", x = NULL, y = NULL) +
  coord_sf(xlim = c(262908.3, 318579.3), ylim = c(6208685, 6282921)) +
  theme_minimal()
ggsave("plots/original gamms/amsa2024_withMB.png",
    width = 8, height = 6, units = "in", dpi = 300, bg = "white")

ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
  hab_cols +
  new_scale_fill() +
  # geom_tile(data = mbhab, aes(x, y, fill = dom_tag), show.legend = F) +
  # hab_cols +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  # geom_sf(data = wampa, fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  # geom_point(data = habi,aes(longitude.1, latitude.1, colour = method),shape = 10, size = 1, alpha = 1/5) +
  # scale_colour_manual(values = c("BRUV" = "indianred4",
  #                                "Drop Camera" = "navyblue")) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("rect", xmin = 288666, xmax = 311266, ymin = 6220394, ymax = 6234274,
           colour = "goldenrod1", fill = NA, alpha = 0.1, linewidth = 1) +
  annotate("text", x = c(265000,290000,310000), y = 6240000, label = c("200m","70m","30m"), size = 2, colour = "grey54")+
  labs(fill = "Habitat", colour = "Sample", x = NULL, y = NULL) +
  coord_sf(xlim = c(262908.3, 318579.3), ylim = c(6208685, 6282921)) +
  theme_minimal()
ggsave("plots/original gamms/amsa2024_withoutMB.png",
       width = 8, height = 6, units = "in", dpi = 300, bg = "white")

