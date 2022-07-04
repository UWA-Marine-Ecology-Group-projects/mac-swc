library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)
library(dplyr)
library(ggplot2)

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

bath_cross[which.min(abs(0-bath_cross$Z)),] # Closest to 0 sea level - ie coastline

bath_700 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -700, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

bath_200 <- bath_cross %>%
  dplyr::mutate(distance.from.coast = (x - 315484)/1000) %>% # 0 sea level is coastline
  dplyr::filter(Z > -200, distance.from.coast < 10) %>% # down to 800m depth and 10km from coastline
  glimpse()

p1 <- ggplot() +
  geom_rect(aes(xmin = min(bath_700$distance.from.coast), xmax = 10, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_700, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_700, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  labs(x = "Distance from coast", y = "Depth")
p1

p2 <- ggplot() +
  geom_rect(aes(xmin = min(bath_200$distance.from.coast), xmax = 10, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_200%>%dplyr::filter(Z >-200), aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bath_200%>%dplyr::filter(Z >-200), aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  labs(x = "Distance from coast", y = "Depth")
p2

