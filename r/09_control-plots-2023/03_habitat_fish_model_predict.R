###
# Project: mac - South-west Corner
# Data:    BRUVS, BOSS Habitat & fish data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin & Claude
# date:    Feb 2022
##

rm(list=ls())
gc()

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)
library(sp)
library(janitor)
library(rgdal)
library(GlobalArchive)
library(rgdal)
library(terra)
library(sf)
library(dismo)

# read in
dat <- readRDS("data/tidy/2023-03_dat.length-maturity.rds") %>%
  dplyr::mutate(method="BRUV")

coords <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  dplyr::select(campaignid,sample,latitude,longitude)%>%
  glimpse()

fabund <- dat %>%                      # merged fish data used for fssgam script
  left_join(coords,by = c("campaignid","sample"))%>%
  dplyr::mutate(latitude = as.numeric(latitude),longitude=as.numeric(longitude))%>%
  dplyr::mutate(method = as.factor(method),site = as.factor(site))%>%
  glimpse()

preds  <- readRDS("output/habitat_fssgam/broad_habitat_predictions.rds") # spatial and habitat covs

prel   <- readRDS("output/spatial/raster/predicted_relief_raster.rds")                         # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
predsp$relief <- raster::extract(prel, predsp)
preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(broad.reef = preef,
                mean.relief = relief) %>%
  dplyr::select(-c(na.rm, xy, coords.x1, coords.x2)) %>%
  glimpse()

predsf <- st_as_sf(preddf, coords = c("x", "y"), crs = "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

ramps <- read.csv("data/2020_SWC_boat_ramps.csv") %>%
  # dplyr::filter(Boat.ramp %in% "Gnarabup") %>%
  glimpse()

rampsv <- vect(ramps, geom = c("Longitude", "Latitude"),
               crs = "EPSG:4326") %>%
  project("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")
rampst <- as.data.frame(rampsv, geom = "XY")
rampsf <- st_as_sf(rampst, coords = c("x", "y"), crs = "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

distances <- data.frame(st_distance(predsf, rampsf)) %>%
  dplyr::mutate_all(as.numeric) %>%
  rowwise() %>%
  dplyr::mutate(distance.to.ramp = min(X1, X2, X3, X4))

preddf <- cbind(preddf, distances$distance.to.ramp) %>%
  dplyr::rename(distance.to.ramp = "distances$distance.to.ramp") %>%
  dplyr::mutate(distance.to.ramp = distance.to.ramp / 1000)

# Join status onto the predictors ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)
commonwealth.marineparks <- spTransform(commonwealth.marineparks, CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"))

preddf.raw <- preddf

coordinates(preddf) <- c('x', 'y')
proj4string(preddf) <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

metadata.commonwealth.marineparks <- over(preddf, commonwealth.marineparks) %>%
  dplyr::select(ZoneName)

unique(metadata.commonwealth.marineparks$ZoneName)

names(metadata.commonwealth.marineparks)

preddf <- bind_cols(preddf.raw,metadata.commonwealth.marineparks) %>%
  dplyr::rename(Commonwealth.zone=ZoneName) %>%
  mutate(Status = if_else((Commonwealth.zone %in% c("National Park Zone")),"No-take","Fished"))%>%
  dplyr::filter(!is.na(mean.relief)) %>%
  ga.clean.names()

#change CRS to UTM
coordinates(fabund) <- c("longitude","latitude")
proj4string(fabund) <- CRS("+proj=longlat +datum=WGS84") 
fabund <- spTransform(fabund, CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"))
fabund <- as.data.frame(fabund)

# reduce predictor space to fit survey area
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude, 
                                                fabund$latitude), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)
unique(fabund$scientific)

# use formula from top model from FSSGam model selection
# Over size of first maturity
m_mature <- gam(number ~ s(depth, k = 3, bs = "cr") + s(roughness, k = 3, bs = "cr") + 
                  s(tpi, k = 3, bs = "cr") + s(site,bs = "re"), 
               data = fabund%>%dplyr::filter(scientific%in%"greater than size of maturity"), 
               method = "REML", family = tw())
summary(m_mature)

# Under size of first maturity
# NOT ENOUGH DATA
# m_immature <- gam(number ~ s(broad.reef, k = 3, bs = "cr")+s(detrended, k = 3, bs = "cr")+
#                     s(tpi, k = 3, bs = "cr")+s(site,bs = "re"),
#                      data = fabund%>%dplyr::filter(scientific%in%"smaller than size of maturity"), 
#                      method = "REML", family = tw())
# summary(m_immature)

#greater than legal size
m_legal <- gam(number ~ s(mean.relief, k = 3, bs = "cr")+
                 s(roughness, k = 3, bs = "cr") +s(site,bs = "re"),
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

#smaller than legal size
m_sublegal <- gam(number ~ s(broad.reef, k = 3, bs = "cr")+
                    s(roughness, k = 3, bs = "cr") + s(site,bs = "re"), 
               data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)

# Now same response vars subset for only west coast demersal indicator species
# Over size of first maturity
m_matureind <- gam(number ~ s(mean.relief, k = 3, bs = "cr") + 
                     s(roughness, k = 3, bs = "cr") + s(site,bs = "re"), 
                data = fabund%>%dplyr::filter(scientific%in%"greater than size of maturity indicator"), 
                method = "REML", family = tw())
summary(m_matureind)

# Under size of first maturity
# NOT ENOUGH DATA
# m_immature <- gam(number ~ s(broad.reef, k = 3, bs = "cr")+s(detrended, k = 3, bs = "cr")+
#                     s(tpi, k = 3, bs = "cr")+s(site,bs = "re"),
#                      data = fabund%>%dplyr::filter(scientific%in%"smaller than size of maturity"), 
#                      method = "REML", family = tw())
# summary(m_immature)

#greater than legal size
m_legalind <- gam(number ~ s(distance.to.ramp, k = 3, bs = "cr")+
                 s(roughness, k = 3, bs = "cr") + s(site,bs = "re"),
               data = fabund%>%dplyr::filter(scientific%in%"greater than legal size indicator"), 
               method = "REML", family = tw())
summary(m_legalind)

#smaller than legal size
# NOT ENOUGH DATA
# m_sublegal <- gam(number ~ s(broad.reef, k = 3, bs = "cr")+s(roughness, k = 3, bs = "cr") + s(site,bs = "re"), 
#                   data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
#                   method = "REML", family = tw())
# summary(m_sublegal)

preddf <- cbind(preddf, 
                "p_mature" = predict(m_mature, preddf, type = "response",
                                       exclude = "s(site)", newdata.guaranteed=TRUE, se.fit = T),
                "p_legal" = predict(m_legal, preddf, type = "response",
                                    exclude = "s(site)", newdata.guaranteed=TRUE, se.fit = T),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response",
                                       exclude = "s(site)", newdata.guaranteed=TRUE, se.fit = T),
                "p_legalind" = predict(m_legalind, preddf, type = "response",
                                       exclude = "s(site)", newdata.guaranteed=TRUE, se.fit = T),
                "p_matureind" = predict(m_matureind, preddf, type = "response",
                                       exclude = "s(site)", newdata.guaranteed=TRUE, se.fit = T)) %>%
  dplyr::mutate(depth.zone = case_when(depth < 30 ~ "shallow",
                                       between(depth, 30, 70) ~ "mesophotic",
                                       depth > 70 ~ "rariphotic"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 19:28)]) 
plot(prasts)

# MESS 
# Extract sampling locations
predr <- rasterFromXYZ(preddf[,c(1, 2, 4:7, 15:16)])
plot(predr)

xy <- fabund %>%
  dplyr::select(longitude , latitude) %>%
  glimpse()

dat <- raster::extract(predr, xy)
messrast <- mess(predr, dat) %>%
  clamp(lower = -0.01, useValues = F)
plot(messrast)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff) %>%
  mask(messrast) # Exclude areas of prediction out of range of predictors
plot(sprast)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE) %>%
  dplyr::left_join(preddf %>% dplyr::select(x, y, status, depth.zone))

control.plots <- spreddf %>%
  dplyr::group_by(status, depth.zone) %>%
  dplyr::summarise(mature.mean    = mean(p_mature.fit),
                   mature.se      = mean(p_mature.se.fit),
                   matureind.mean = mean(p_matureind.fit),
                   matureind.se   = mean(p_matureind.se.fit),
                   legal.mean     = mean(p_legal.fit),
                   legal.se       = mean(p_legal.se.fit),
                   sublegal.mean  = mean(p_sublegal.fit),
                   sublegal.se    = mean(p_sublegal.se.fit),
                   legalind.mean  = mean(p_legalind.fit),
                   legalind.se    = mean(p_legalind.se.fit)) %>%
  glimpse()

saveRDS(control.plots, file = "data/tidy/2023-03_control-plot-data.rds")
