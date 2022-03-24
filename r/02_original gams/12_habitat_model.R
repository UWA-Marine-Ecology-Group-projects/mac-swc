###
# Project: MAC Hub - SWC
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Kingsley Griffin
# date:    Feb 2022
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
habi   <- readRDS("data/tidy/habitat_merged.rds")                               # merged data from 'R/11_habitat_modelselect.R'
preds  <- readRDS("data/spatial/250m_spatialcovariates_utm.rds")                # spatial covs from 'R/XX_krige_relief.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth <- preddf$GA_Bathymetry_past.shelf

# reduce predictor space to fit survey area
# preddf <- preddf[preddf$Depth > min(habi$Depth), ]
# preddf <- preddf[preddf$Depth < 200, ]
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude.1, 
                                                habi$latitude.1), data = habi)
sbuff  <- buffer(habisp, 10000)


# use formula from top model from '2_modelselect.R'
m_macro <- gam(cbind(broad.macroalgae, broad.total.points.annotated - broad.macroalgae) ~ 
                 s(depth, k = 5, bs = "cr") + 
                 s(roughness, k = 3, bs = "cr") +
                 s(tpi, k = 3, bs = "cr") +
                 s(detrended, k = 3, bs = "cr") 
               ,
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

m_reef <- gam(cbind(broad.reef, broad.total.points.annotated - broad.reef) ~ 
                s(depth, k = 3, bs = "cr") +
                 s(detrended, k = 3, bs = "cr")  + 
                s(roughness, k = 5, bs = "cr") +
                s(tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)
gam.check(m_reef)
vis.gam(m_reef)

m_biogenic <- gam(cbind(biogenic_reef, broad.total.points.annotated - biogenic_reef) ~ 
                s(depth, k = 3, bs = "cr") +
                s(detrended, k = 3, bs = "cr")  + 
                s(roughness, k = 5, bs = "cr")+
                  s(tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)
gam.check(m_reef)
vis.gam(m_reef)

m_sand <- gam(cbind(broad.unconsolidated, broad.total.points.annotated - broad.unconsolidated) ~ 
                s(depth,   k = 3, bs = "cr") +
                s(detrended, k = 5, bs = "cr") +
                s(roughness, k = 5, bs = "cr") +
                s(tpi,       k = 3, bs = "cr") , 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)

m_rock <- gam(cbind(broad.consolidated, broad.total.points.annotated - broad.consolidated) ~ 
                s(depth,     k = 5, bs = "cr") +
                s(roughness, k = 3, bs = "cr") +
                s(detrended, k = 3, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
vis.gam(m_rock)

m_grass <- gam(cbind(broad.seagrasses, broad.total.points.annotated - broad.seagrasses) ~ 
                 
                 s(depth,   k = 5, bs = "cr") +
                 s(detrended, k = 3, bs = "cr") +
                 s(tpi,       k = 3, bs = "cr"),
                # s(roughness, k = 3, bs = "cr") +,
              data = habi, method = "REML", family = binomial("logit"))
summary(m_grass)
gam.check(m_grass)
vis.gam(m_grass)

m_sponge <- gam(cbind(broad.sponges, broad.total.points.annotated - broad.sponges) ~ 
                  s(depth,   k = 5, bs = "cr") + 
                  s(roughness, k = 5, bs = "cr") +
                  # s(tpi,       k = 5, bs = "cr")+
                  s(detrended, k = 5, bs = "cr"),
                data = habi, method = "REML", family = binomial("logit"))
summary(m_sponge)
gam.check(m_sponge)
vis.gam(m_sponge)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pmacroalgae" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "pbiogenic" = predict(m_biogenic,preddf, type = "response"),
                "preef" = predict(m_reef, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "pseagrass" = predict(m_grass, preddf, type = "response"),
                "psponge" = predict(m_sponge, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[,c(1,2,8:14)])
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# tidy, categorise by dominant tag and output data
spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)
spreddf$dom_tag <- apply(spreddf[c(3:5,7:9)], 1,
                        FUN = function(x){names(which.max(x))})
spreddf$dom_tag <- sub('p', '', spreddf$dom_tag)
unique(spreddf$dom_tag)
head(spreddf)

saveRDS(preddf,  "output/habitat_fssgam/broad_habitat_predictions.rds")
saveRDS(spreddf, "output/habitat_fssgam/site_habitat_predictions.rds")
