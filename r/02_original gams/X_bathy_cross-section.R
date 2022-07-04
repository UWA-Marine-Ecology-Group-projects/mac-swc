library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)
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
bath_df[which.min(abs(6223483.39-bath_df$y)),]

bath_cross <- bath_df %>%
  # dplyr::filter(y %in% -34.05875) %>%
  dplyr::mutate(y = round(y, digits = 0)) %>%
  # dplyr::mutate(y = as.character(y)) %>%
  dplyr::filter(y %in% 6223580)

bath_cross[which.min(abs(0-bath_cross$Z)),] # Closest to 0m sea level - ie coastline

bath_700 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -700, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

bath_200 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -200, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

multi <- raster("data/spatial/rasters/multibeam_derivatives_depth.tif")
plot(multi)
multi_df <- as.data.frame(multi, xy = T, na.rm = T)

multi_df[which.min(abs(6223483.39-multi_df$y)),] #6223484

multi_cross <- multi_df %>%
  dplyr::mutate(y = round(y, digits = 0)) %>%
  dplyr::filter(y %in% 6223484)

multi_plot <- multi_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline - from broad dataset, multibeam doesnt go to coastline
  dplyr::filter(distance.from.coast < 10) %>% # 10km from coastline
  glimpse()


p1 <- ggplot() +
  geom_rect(aes(xmin = min(bath_700$distance.from.coast), xmax = 10, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_700, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_700, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast", y = "Elevation (m)") + 
  annotate("text", x = 3, y = 200, label = "Boranup")
p1

p2 <- ggplot() +
  geom_rect(aes(xmin = min(bath_200$distance.from.coast), xmax = 10, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_200, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_200, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(100, 0, -100 , -200),expand = c(0,0), limits = c(-200, 240)) +
  labs(x = "Distance from coast", y = "Elevation (m)") +
  annotate("text", x = 3, y = 175, label = "Boranup")
p2

p3 <- ggplot() +
  geom_rect(aes(xmin = min(multi_plot$distance.from.coast), xmax = 10, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  # geom_line(data = bath_200%>%dplyr::filter(distance.from.coast > -5.57), aes(y = Z, x = distance.from.coast), color = "#faeede")+
  geom_ribbon(data = bath_200%>%dplyr::filter(distance.from.coast > -5.57), aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "#f0e3d3") +
  geom_line(data = multi_plot, aes(y = multibeam_derivatives_depth, x = distance.from.coast))+
  geom_ribbon(data = multi_plot, aes(ymin = -Inf, ymax = multibeam_derivatives_depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(min(multi_plot$distance.from.coast), 10)) +
  scale_y_continuous(breaks = c(100, 0, -100 , -200),expand = c(0,0), limits = c(-200, 240)) +
  labs(x = "Distance from coast", y = "Elevation (m)")+
  annotate("text", x = 3, y = 175, label = "Boranup")
p3

plots <- p1 / p2 / p3 + plot_annotation(tag_levels = "a")
plots

ggsave("plots/original gamms/bathy-cross-section.png", plots, height = 7, width = 8)
