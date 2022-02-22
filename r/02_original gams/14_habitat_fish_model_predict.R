###
# Project: mac - South-west Corner
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin & Claude
# date:    Feb 2022
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)
library(sp)
library(janitor)

# read in
dat1 <- readRDS("data/tidy/dat.maxn.full.rds")%>%
  dplyr::rename(number=maxn)%>%
  glimpse()
dat2 <- readRDS("data/tidy/dat.length.full.rds")%>%
  dplyr::mutate(method="BRUV")
janitor::compare_df_cols(dat1,dat2)

coords <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.Metadata.csv")%>%
  dplyr::select(campaignid,sample,latitude,longitude)%>%
  glimpse()

fabund <- bind_rows(dat1,dat2) %>%                       # merged fish data used for fssgam script
  left_join(coords,by = c("campaignid","sample"))%>%
  glimpse()

preds  <- readRDS("output/habitat_fssgam/broad_habitat_predictions.rds") # spatial and habitat covs

prel   <- readRDS("output/spatial/raster/predicted_relief_raster.rds")                         # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
predsp$relief <- extract(prel, predsp)
preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE) #
preddf$reef   <- preddf$preef
preddf$sand   <- preddf$psand
preddf$macroalgae   <- preddf$pmacroalg
head(preddf)

# reduce predictor space to fit survey area
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude, 
                                                fabund$latitude), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)
unique(fabund$scientific)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(relief, k = 3, bs = "cr")+method+s(site,k=3,bs="re"), 
               data = fabund%>%dplyr::filter(scientific%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

m_richness <- gam(number ~ s(broad.macroalgae, k = 3, bs = "cr")+
                    s(detrended, k = 3, bs = "cr")+status+method+s(site,k = 3, bs = "re"),
                     data = fabund%>%dplyr::filter(scientific%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_legal <- gam(number ~ s(detrended, k = 3, bs = "cr")+status,
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

m_sublegal <- gam(number ~ s(tpi, k = 3, bs = "cr"),
               data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 28:35)], res = c(247, 277)) 
plot(prasts)

###
# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

plot(sprast$p_totabund9)
plot(sprast$p_richness9)
plot(sprast$p_legal9)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

summary(spreddf)

saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/site_fish_predictions.rds")


