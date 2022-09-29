rm(list = ls())

library(sf)
library(ggplot2)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(dplyr)
library(tidyr)
library(cartography)
library(RColorBrewer)
library(scales)
library(colorspace)

# lang <- st_read("data/spatial/shapefiles/language-groups-final.shp")
lang <- read.csv("data/tidy/aus-language-groups.csv") %>%
  dplyr::rename(x = "approximate_longitude_of_language_variety",
                y = "approximate_latitude_of_language_variety") %>%
  glimpse()

langspat <- st_read("data/spatial/shapefiles/language-groups-clipped.shp") %>%
  dplyr::mutate(id = as.factor(id))

aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
# dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                      # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
aumpa <- st_crop(aumpa, xmin = 100, xmax = 170, ymin = -50, ymax = -5) %>%
  dplyr::mutate(ZoneName = dplyr::recode(ZoneName,
                                         "Special Purpose Zone (Norfolk)"  = "Special Purpose Zone",
                                         "Special Purpose Zone (Trawl)" = "Special Purpose Zone",
                                         "Habitat Protection Zone (Reefs)" = "Habitat Protection Zone",
                                         "Marine National Park Zone" = "National Park Zone",
                                         "Habitat Protection Zone (Lord Howe)" = "Habitat Protection Zone"))
unique(aumpa$ZoneName)
aumpa_c <- st_crop(aumpa, xmin = 114, xmax = 120, ymin = -40, ymax = -32)
st_crs(aus) <- st_crs(aumpa)

stateres <- st_read("data/spatial/shapefiles/All_StateReserves.shp")

world <- st_read("data/spatial/shapefiles/ne_10m_admin_0_countries.shp")

wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- "+proj=longlat +datum=WGS84"
wa_sanc <- wampa %>%
  dplyr::filter(ZONE_TYPE %in% "Sanctuary Zone (IUCN VI)")

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters/tiles", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0, ]
bath_r <- rast(cbathy)
plot(bath_r)
e <- ext(113, 121, -37, -31)
bath_c <- crop(bath_r, e)
plot(bath_c)
bathdf <- as.data.frame(bath_c, xy = T, na.rm = T)

bathl <- rast("data/spatial/rasters/tiles/bath_250_good.tif")
aus_v <- vect(aus)
bathl <- mask(bathl, aus_v, inverse = T)
aumpa_v <- vect(aumpa)
bathl_c <- crop(bathl, buffer(aumpa_v, 100000))
bathl_c <- clamp(bathl_c, lower = -Inf, upper = 0, values = F)
bathl_c <- terra::aggregate(bathl_c, fact = 10, fun = "mean")
plot(bathl_c)
bathldf <- as.data.frame(bathl_c, na.rm = T, xy = T)

mb <- rast("data/spatial/rasters/multibeam_derivatives_depth.tif")
mb <- project(mb, "+proj=longlat +datum=WGS84")
plot(mb)
ext(mb)

terrnp <- st_read(
  "data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 110, xmax = 123, ymin = -39, ymax = -31)       # just swc

austerr <- st_read("data/spatial/shapefiles/CAPAD2020_terrestrial.shp")
austerr <- austerr[austerr$TYPE %in% c("Nature Reserve", "National Park"), ]    # exclude state forests etc
# Set colours
# state terrestrial parks colours
terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#c4cea6"), #e4d0bb
                                name = "Terrestrial Managed Areas")

# assign commonwealth zone colours
nmpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"),
                                name = "Commonwealth Marine Parks")

p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x = x, y = y, fill = Z),show.legend = F) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa_c, aes(fill = ZoneName), alpha = 0.4, color = NA) +
  nmpa_fills +
  new_scale_fill() +
  geom_sf(data = wa_sanc, fill = "#bfd054", alpha = 0.4, color = NA) +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  # geom_sf(data = are, aes(fill = IAR_NAME21)) + 
  coord_sf(xlim = c(114, 119.9), ylim = c(-35.5, -32.1)) +
  geom_text(data = lang%>%dplyr::filter(language_name %in% c("Wadandi ", "Pibelmen")), 
            aes(x = x, y = y, label = language_name), 
            fontface = "bold.italic", size = 7, alpha = 0.65, colour = "darkorange3") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(axis.line = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),legend.position = "none",
        panel.background = element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.background = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))
png(filename = "plots/Indigenous-language-groups.png", units = "in", res = 300,
    width = 10, height = 6.7)
p1
dev.off()


p2 <- ggplot() +
  geom_raster(data = bathldf, aes(x = x, y = y, fill = bath_250_good), show.legend = F) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = world, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 0.4, color = NA) +
  nmpa_fills +
  new_scale_fill() +
  geom_sf(data = stateres, fill = "#bfd054", alpha = 0.4, color = NA) +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  geom_sf(data = austerr, aes(fill = TYPE), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  geom_point(data = lang, aes(x = x, y = y, colour = language_name),
            size = 5, alpha = 0.15) + #, colour = "darkorange3"
  scale_colour_discrete_sequential(palette = "YlOrBr") +
  coord_sf(xlim = c(111, 160), ylim = c(-44, -11)) +
  theme_minimal() +
  theme(axis.line = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),legend.position = "none",
        panel.background = element_rect(fill = "#9dc9e1"),
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.background = element_blank())

png(filename = "plots/Aus-with-parks.png", units = "in", res = 200,
    width = 10, height = 7.5)
p2
dev.off()

p3 <- ggplot() +
  geom_raster(data = bathdf, aes(x = x, y = y, fill = Z),show.legend = F) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa_c, aes(fill = ZoneName), alpha = 0.4, color = NA) +
  nmpa_fills +
  new_scale_fill() +
  geom_sf(data = wa_sanc, fill = "#bfd054", alpha = 0.4, color = NA) +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  # geom_sf(data = are, aes(fill = IAR_NAME21)) + 
  coord_sf(xlim = c(114, 116.2), ylim = c(-34.6, -33.3)) +
  geom_text(data = lang, aes(x = x, y = y, label = language_name), 
            fontface = "bold.italic", size = 10, alpha = 0.65, colour = "darkorange3") +
  labs(x = "Longitude", y = "Latitude") +
  annotate("rect", xmin = 114.708118647453, xmax = 114.956063687516,
           ymin = -34.1400803710618, ymax = -34.0108992689951,
           colour = "darkgoldenrod1", fill = "white", alpha = 0.2, size = 1.5) +
  theme_minimal() +
  theme(axis.line = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),legend.position = "none",
        panel.background = element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.background = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))
png(filename = "plots/Indigenous-language-groups-zoomed-multibeam.png", units = "in", res = 300,
    width = 10, height = 6.7)
p3
dev.off()


