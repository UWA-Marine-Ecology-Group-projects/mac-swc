library(tidyverse)
library(terra)
library(tidyterra)
library(sf)

rm(list = ls())

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
# dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                      # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                    # all wa mpas
nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
rg_nmp <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # regional nat parks networks
nb_nmp <- rg_nmp[rg_nmp$ResName %in% c("South-west Corner", "Geographe"), ]     # just nat parks nearby
nb_npz <- nb_nmp[nb_nmp$ZoneName == "National Park Zone", ]
wanew  <- st_read("data/spatial/shapefiles/test1.shp") %>%
  st_make_valid()# zones in ngari capes
terrnp <- st_read(
  "data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 123, ymin = -39, ymax = -30))      # crop down coastal waters line to general project area

kef <- st_read("data/spatial/shapefiles/AU_DOEE_KEF_2015.shp")
sf_use_s2(FALSE)                                                                #errors otherwise, not sure what it does...
st_crs(aus)         <- st_crs(aumpa)
st_crs(wanew)       <- st_crs(nb_mp)

habitat <- readRDS('data/tidy/habitat_merged_allcols.rds')                              # get sampling data
habitat$method <- dplyr::recode(habitat$method,
                                "BOSS" = "Drop Camera")

# simplify zone names
nb_nmp$ZoneName <- dplyr::recode(nb_nmp$ZoneName,
                                 "Special Purpose Zone (Mining Exclusion)" =
                                   "Special Purpose Zone\n(Mining Exclusion)")

nb_mp$waname <- gsub("( \\().+(\\))", "", nb_mp$ZONE_TYPE)
nb_mp$waname <- gsub(" [1-4]", "", nb_mp$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <- 
#   c("Special Purpose Zone\n(Habitat Protection)")
nb_mp$waname[nb_mp$NAME == "Ngari Capes"]     <- "General Use"
nb_mp$waname <- dplyr::recode(nb_mp$waname, 
                              "General Use" = "General Use Zone",
                              # "MMA" = "Marine Management Area",
                              # "Recreation Area" = "Recreation Zone",
                              # "Conservation Area" = "Sanctuary Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)")

# fix up new zones within Ngari Capes
wanew$waname <- word(wanew$Name, start = -2, end = -1)

# reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 110, xmax = 123, ymin = -39, ymax = -33.3)       # just swc
# plot(terrnp["leg_catego"])

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"))

# assign commonwealth zone colours
s_nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                            "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1"))

# state colours
s_wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                             "General Use Zone" = "#bddde1"))

bathy <- rast("data/spatial/rasters/GA_Bathymetry_past-shelf.tif")
plot(bathy)

ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = nb_mp, aes(fill = waname), alpha = 3/5, colour = NA) +
  geom_sf(data = wanew, aes(fill = waname), alpha = 3/5, colour = NA) +
  s_wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 3/5, colour = NA) +
  waterr_cols +
  labs(fill = "Terrestrial Managed Areas") +
  new_scale_fill() +
  geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  s_nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_spatraster_contour(data = bathy,
               binwidth = 50, colour = "white", alpha = 4/5, size = 0.1) +
  # geom_text_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
  #                   binwidth = 50, size = 2.5, label.placer = label_placer_n(1)) +
  geom_point(data = habitat, aes(longitude, latitude), 
             alpha = 1, shape = 21, colour = "black", fill = "white") +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", colour = "Sample") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 114.7, xmax = 114.95, ymin = -34.14, ymax = -34.01,
           colour = "goldenrod1", fill = "white", alpha = 0.1, linewidth = 0.8) +
  coord_sf(xlim = c(114.47, 115.1), ylim = c(-34.15, -33.65)) +
  # geom_segment(aes(x = 114.4, xend = 115.08, y = -34.127327939, yend = -34.127327939), linetype = 2, alpha = 0.3) +
  theme_minimal()

ggsave("plots/spatial/amsa_site_overview_map.png", dpi = 300, width = 10, height = 6,
       bg = "white")
