rm(list = ls())

library(raster)
library(sf)
# library(stars)
# library(starsExtra)
library(dplyr)
library(ggplot2)
library(patchwork)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters/tiles", "*tile", full.names = TRUE) # Hidden in gitignore
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

# Too big so will crop
e <- extent(110, 116, -36,-31)
bath_crop <- crop(bath_r, e)
plot(bath_crop)

wgscrs <- CRS("+proj=longlat +datum=WGS84")
crs(bath_crop) <- wgscrs

sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
bath_t <- projectRaster(bath_crop, crs = sppcrs)
plot(bath_t)

# To data frame
bath_df <- as.data.frame(bath_t, xy = T)

# Find the closest value to the middle of the NPZ
bath_df[which.min(abs(6222000-bath_df$y)),] #6221918 

bath_cross <- bath_df %>%
  # dplyr::filter(y %in% -34.05875) %>%
  dplyr::mutate(y = round(y, digits = 0)) %>%
  # dplyr::mutate(y = as.character(y)) %>%
  dplyr::filter(y %in% 6221918)

bath_cross[which.min(abs(0-bath_cross$Z)),] # Closest to 0m sea level - ie coastline

bath_700 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -700, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

bath_255 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -255, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

multi <- raster("data/spatial/rasters/multibeam_derivatives_depth.tif")
plot(multi)
multi_df <- as.data.frame(multi, xy = T, na.rm = T)

multi_df[which.min(abs(6222000-multi_df$y)),] #6222000

multi_cross <- multi_df %>%
  dplyr::mutate(y = round(y, digits = 0)) %>%
  dplyr::filter(y %in% 6222000)

multi_plot <- multi_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline - from broad dataset, multibeam doesnt go to coastline
  dplyr::filter(distance.from.coast < 10) %>% # 10km from coastline
  glimpse()


p1 <- ggplot() +
  geom_rect(aes(xmin = min(bath_700$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_700, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_700, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") + 
  annotate("text", x = 3, y = 200, label = "Boranup")+
  geom_segment(aes(x = -5.556, xend = -5.556, y =-44.03002, yend = 0), color = "red")
p1

p2 <- ggplot() +
  geom_rect(aes(xmin = min(bath_255$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) + # Add ocean
  geom_line(data = bath_255, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_255, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(100, 0, -100 , -200),expand = c(0,0), limits = c(-255, 240)) + # Set depth/topo limits
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  annotate("text", x = 3, y = 175, label = "Boranup") + # Boranup label
  geom_segment(aes(x = -23.431987, xend = -16, y = -118, yend = -118), linetype = 2, alpha = 0.5) + # 20-30 Ka BP - segment length 7.5
  geom_segment(aes(x = -21.111987, xend = -16, y = -94, yend = -94), linetype = 2, alpha = 0.5) + # 15-17 Ka BP
  geom_segment(aes(x = -19.255987, xend = -16, y = -63, yend = -63), linetype = 2, alpha = 0.5) + # 12-13 Ka BP
  geom_segment(aes(x = -10, xend = 1, y = -41, yend = -41), linetype = 2, alpha = 0.5) + # 9-10 Ka BP
  annotate("text", x = c(-13.5,-13.5, -13.5, 3.5), y = c(-118,-94,-63,-35.93282), # 3.5 from end segment to start text
           label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"),  # Bold text
           size = 3.5, fontface = "bold") +
  geom_segment(aes(x = -5.556, xend = -5.556, y =-44.03002, yend = 0), color = "red") +
  annotate("text", x = c(-33, -6.5), y = c(-170, -77), label = c("Mid-shelf sand plains", "Submerged
  wetlands"), size = 3) + # Standard text
  annotate("rect", xmin = -40, xmax = -25, ymin = -155, ymax = -110,  fill = "white", color = "gray33", alpha = 0.3) + # Sand plains highlight box
  annotate("rect", xmin = -8, xmax = -5, ymin = -50, ymax = -25, fill = "white", color = "gray33", alpha = 0.3) # Wetlands highlight box
p2

p3 <- ggplot() +
  geom_rect(aes(xmin = min(bath_255$distance.from.coast), xmax = 4, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_ribbon(data = bath_255%>%mutate(Z = ifelse((between(distance.from.coast, -60, min(multi_plot$distance.from.coast)+0.1)|between(distance.from.coast, max(multi_plot$distance.from.coast)-0.1, 10)), Z, -255)), 
              aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "#f0e3d3") +
  geom_line(data = multi_plot, aes(y = multibeam_derivatives_depth, x = distance.from.coast))+
  geom_ribbon(data = multi_plot, aes(ymin = -Inf, ymax = multibeam_derivatives_depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(100, 0, -100 , -200),expand = c(0,0), limits = c(-255, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)")+
  annotate("text", x = 3, y = 175, label = "Boranup")+
  geom_segment(aes(x = -5.556, xend = -5.556, y =-44.03002, yend = 0), color = "red")
p3

plots <- p1 / p2 / p3 + plot_annotation(tag_levels = "a")
plots

ggsave("plots/original gamms/bathy-cross-section.png", plots, height = 29, width = 21, units = "cm")
