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
library(purrr)

# read in
dat1 <- readRDS("data/tidy/dat.maxn.multibeam.rds")%>%
  dplyr::rename(number=maxn)%>%
  glimpse()
dat2 <- readRDS("data/tidy/dat.length.multibeam.rds")%>%
  dplyr::mutate(method="BRUV")%>%
  glimpse()
janitor::compare_df_cols(dat1,dat2)

coords <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  dplyr::select(campaignid,sample,latitude,longitude)%>%
  glimpse()

fabund <- bind_rows(dat1,dat2) %>%                       # merged fish data used for fssgam script
  left_join(coords,by = c("campaignid","sample"))%>%
  dplyr::mutate(latitude = as.numeric(latitude),longitude=as.numeric(longitude))%>%
  dplyr::mutate(method = as.factor(method),site = as.factor(site))%>%
  glimpse()

file.names <- list.files("output/multibeam_habitat_fssgam", '*.tif', 
                         full.names = TRUE)                                     #split up as too big for git
preds <- raster::stack(file.names) 
preds <- as.data.frame(preds, xy = T, na.rm = T)

#read in multibeam data to join to predicted habitat
file.names.multi <- list.files(path="data/spatial/rasters",
                               pattern = 'multibeam_*',full.names = T)
multi <- stack(file.names.multi)

#preds as a spatial dataframe
coordinates(preds) <- ~x+y

preds <- raster::extract(multi, preds, sp = T)
preds <- as.data.frame(preds)
preds$multibeam_derivatives_depth <- abs(preds$multibeam_derivatives_depth)

#read in relief data
prel   <- raster("output/spatial/raster/predicted_relief_multibeam_prelief.tif")      # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
predsp$relief <- raster::extract(prel, predsp)
preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(broad.reef=layer_preef,mean.relief=relief,broad.macroalgae=layer_pmacroalgae)%>%
  glimpse()

#change CRS of fish predictions to UTM
coordinates(fabund) <- c("longitude","latitude")
proj4string(fabund) <- CRS("+proj=longlat +datum=WGS84") 
fabund <- spTransform(fabund, CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"))
fabund <- as.data.frame(fabund)

# reduce predictor space to fit survey area
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude, 
                                                fabund$latitude), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000) #buffer 10k around samples
unique(fabund$scientific)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(mean.relief, k = 3, bs = "cr"), 
                  data = fabund%>%dplyr::filter(scientific%in%"total.abundance"), 
                  method = "REML", family = tw())
summary(m_totabund)

#species richness
m_richness <- gam(number ~ s(multibeam_derivatives_depth, k = 3, bs = "cr"),
                  data = fabund%>%dplyr::filter(scientific%in%"species.richness"), 
                  method = "REML", family = tw())
summary(m_richness)

#greater than legal size
m_legal <- gam(number ~ s(mean.relief, k = 3, bs = "cr")+
                 s(multibeam_derivatives_detrended, k = 3, bs = "cr")+
                 s(multibeam_derivatives_roughness, k = 3, bs = "cr"),
               data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
               method = "REML", family = tw())
summary(m_legal)

#smaller than legal size
m_sublegal <- gam(number ~ s(broad.macroalgae, k = 3, bs = "cr"),
                  data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
                  method = "REML", family = tw())
summary(m_sublegal)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response"))

p_totabund <- rasterFromXYZ(preddf[, c(1, 2, 19)], res = c(40, 40)) 
p_richness <- rasterFromXYZ(preddf[, c(1, 2, 20)], res = c(40, 40))
p_legal <- rasterFromXYZ(preddf[, c(1, 2, 21)], res = c(40, 40))
p_sublegal <- rasterFromXYZ(preddf[, c(1, 2, 22)], res = c(40, 40))
prasts <- stack(p_totabund,p_richness,p_legal,p_sublegal)

###
# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

plot(sprast$p_totabund)
plot(sprast$p_richness)
plot(sprast$p_legal)
plot(sprast$p_sublegal)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

summary(spreddf) #not any funky looking outliers

# split and output data whole data are too large for git :'(
nspreddf <- nrow(spreddf)/10

spreddfa <- spreddf[1:nspreddf, ]
spreddfb <- spreddf[(1 + nspreddf):(nspreddf * 2), ]
spreddfc <- spreddf[(1 + (nspreddf * 2)):(nspreddf * 3), ]
spreddfd <- spreddf[(1 + (nspreddf * 3)):(nspreddf * 4), ]
spreddfe <- spreddf[(1 + (nspreddf * 4)):(nspreddf * 5), ]
spreddff <- spreddf[(1 + (nspreddf * 5)):(nspreddf * 6), ]
spreddfg <- spreddf[(1 + (nspreddf * 6)):(nspreddf * 7), ]
spreddfh <- spreddf[(1 + (nspreddf * 7)):(nspreddf * 8), ]
spreddfi <- spreddf[(1 + (nspreddf * 8)):(nspreddf * 9), ]
spreddfj <- spreddf[(1 + (nspreddf * 9)):(nspreddf * 10), ]

saveRDS(spreddfa, "output/multibeam fish gamms/site_fish_predictions_a.rds")
saveRDS(spreddfb, "output/multibeam fish gamms/site_fish_predictions_b.rds")
saveRDS(spreddfc, "output/multibeam fish gamms/site_fish_predictions_c.rds")
saveRDS(spreddfd, "output/multibeam fish gamms/site_fish_predictions_d.rds")
saveRDS(spreddfe, "output/multibeam fish gamms/site_fish_predictions_e.rds")
saveRDS(spreddff, "output/multibeam fish gamms/site_fish_predictions_f.rds")
saveRDS(spreddfg, "output/multibeam fish gamms/site_fish_predictions_g.rds")
saveRDS(spreddfh, "output/multibeam fish gamms/site_fish_predictions_h.rds")
saveRDS(spreddfi, "output/multibeam fish gamms/site_fish_predictions_i.rds")
saveRDS(spreddfj, "output/multibeam fish gamms/site_fish_predictions_j.rds")
