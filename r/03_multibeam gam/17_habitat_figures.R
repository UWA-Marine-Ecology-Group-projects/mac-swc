###
# Project: MAC Hub - SWC
# Data:    Habitat predictions
# Task:    Habitat figures
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
library(dplyr)
library(rgdal)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
nb_nmp <- aumpa[aumpa$ResName %in% c("South-west Corner"), ]                    # regional nat parks networks
nb_npz <- nb_nmp[(nb_nmp$ZoneName == "National Park Zone" & 
                    nb_nmp$Area_KM2 < 1000), ]                                  # isolate local AMP
nb_npz <- nb_npz[2, ]

wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                    # all wa mpas
nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes
st_crs(wanew) <- crs(wampa)

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
nb_npz <- st_transform(nb_npz, sppcrs)

#bring in bathy for contour lines
bath_r <- raster("data/spatial/rasters/multibeam_derivatives_depth.tif")            # bathymetry trimmed to project area
# bath_t <- projectRaster(bath_r, crs = sppcrs)                                   # transform before convert to dataframe
bath_r <- aggregate(bath_r, fact = 10, fun = mean)
bathdf <- as.data.frame(bath_r, na.rm = TRUE, xy = TRUE)
colnames(bathdf)[3] <- "Depth"

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 123, ymin = -39, ymax = -30))      # crop down coastal waters line to general project area

habi    <- readRDS("data/tidy/habitat_multibeam_merged.rds")
# habi$ns <- ifelse(habi$Latitude.1 > 6940000, 1, 0)
habi$method <- dplyr::recode(habi$method,
                             BOSS = "Drop Camera")

# read in outputs from 'R/03_multibeam gam/16_habitat_model.R'
rastnames  <- list.files("output/multibeam_habitat_fssgam", '*.tif', 
                         full.names = TRUE)                                     #split up as too big for git
phab_rasts <- stack(rastnames[-3])                                              # not reef

# convert to data frame
spreddf <- as.data.frame(phab_rasts, xy = TRUE, na.rm = TRUE)

# categorise by dominant tag
spreddf$dom_tag <- apply(spreddf[c(3:8)], 1,
                        FUN = function(x){names(which.max(x))})
spreddf$dom_tag <- gsub("layer_p", "", spreddf$dom_tag)                          # tidy tag labels
spreddf$dom_tag <- as.factor(spreddf$dom_tag)
unique(spreddf$dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                                 macroalgae = "Macroalgae",
                                 sand = "Sand",
                                 biogenic = "Sessile invertebrates",
                                 # seagrass = "Seagrass",
                                 rock = "Rock")

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
# writeRaster(dom.habr, "data/tidy/2020-2021_south-west_BOSS-BRUV_domhab-multibeam.tif")

# As a shapefile
# dom.habs <- rasterToPolygons(dom.habr, dissolve = T)
# 
# writeOGR(dom.habs, "data/tidy/2020-2021_south-west_BOSS-BRUV_domhab-multibeam.shp", 
#          layer = "dom_tag", driver = "ESRI Shapefile")

# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Macroalgae" = "darkgoldenrod4",
                                         # "Seagrass" = "forestgreen",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Sessile invertebrates" = "plum"))

p4 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  # geom_point(data = habi,
  #            aes(longitude.1, latitude.1, colour = method),
  #            shape = 10, size = 1, alpha = 2/5) +
  # scale_colour_manual(values = c("BRUV" = "indianred4",
  #                                "Drop Camera" = "navyblue")) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.7) +
  annotate("text", x = 294000, y = 6227000, label = "70m", size = 2.5, colour = "grey54")+
  labs(fill = "Habitat", colour = "Sample", x = NULL, y = NULL) +
  coord_sf(xlim = c(min(spreddf$x),max(spreddf$x)), ylim = c(min(spreddf$y),max(spreddf$y))) +
  theme_minimal()

png(file="plots/multibeam gamms/multibeam_dominant_habitat.png",
    width=9, height=6, units = "in", res = 160)
p4

dev.off()

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- reshape2::melt(spreddf, measure.vars = c(3:8))
widehabit$variable <- gsub("layer_p", "", widehabit$variable)
widehabit$variable <- dplyr::recode(widehabit$variable,
                                    sponges = "Sponge",
                                    macroalgae = "Macroalgae",
                                    rock = "Rock",
                                    sand = "Sand",
                                    biogenic = "Sessile invertebrates",
                                    seagrass = "Seagrass")

p2 <- ggplot() +
  geom_tile(data = widehabit%>%dplyr::filter(!variable%in%c("Sponge")), 
            aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = nb_npz, fill = NA, colour = "#7bbc63") +
  geom_sf(data = wanew, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.3) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200), colour = "grey54",
               alpha = 1, size = 0.5) +
  annotate("text", x = 293700, y = 6227000, label = "70m", size = 2, colour = "grey54")+
  labs(x = NULL, y = NULL, fill = "Occurrence (p)") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.25))+
  scale_x_continuous(breaks = c(114.7,114.8,114.9))+
  coord_sf(xlim = c(min(spreddf$x),max(spreddf$x)), ylim = c(min(spreddf$y),max(spreddf$y)))+
  facet_wrap(~variable)

png(file="plots/multibeam gamms/multibeam_habitat_predicted.png",
    width=9, height=5, units = "in", res = 160)
p2

dev.off()

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


