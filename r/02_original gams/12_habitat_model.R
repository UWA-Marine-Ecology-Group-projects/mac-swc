###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Kingsley Griffin
# date:    Feb 2022
##

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
habi   <- readRDS("data/tidy/habitat_merged.rds")                               # merged data from 'R/1_mergedata.R'
preds  <- readRDS("data/spatial/250m_spatialcovariates_utm.rds")                        # spatial covs from 'R/1_mergedata.R'
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
preddf$depth.y <- preddf$GA_Bathymetry_past.shelf

# reduce predictor space to fit survey area
# preddf <- preddf[preddf$Depth > min(habi$Depth), ]
# preddf <- preddf[preddf$Depth < 200, ]
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude.1, 
                                                habi$latitude.1), data = habi)
sbuff  <- buffer(habisp, 10000)

# use formula from top model from '2_modelselect.R'
m_macro <- gam(cbind(broad.macroalgae, broad.total.points.annotated - broad.macroalgae) ~ 
                 s(depth.y, k = 3, bs = "cr")  + 
                 s(tpi, k = 5, bs = "cr") +
                 s(detrended, k = 5, bs = "cr") + 
                 s(roughness, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

m_reef <- gam(cbind(broad.reef, broad.total.points.annotated - broad.reef) ~ 
                    s(depth.y, k = 3, bs = "cr")  + 
                    s(tpi, k = 5, bs = "cr") +
                    s(detrended, k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr"), 
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)
gam.check(m_reef)
vis.gam(m_reef)

m_sand <- gam(cbind(broad.unconsolidated, broad.total.points.annotated - broad.unconsolidated) ~ 
                s(depth.y,     k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") +
                s(tpi,       k = 5, bs = "cr")+
                s(detrended, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)
# 
# m_rock <- gam(cbind(rock, totalpts - rock) ~ 
#                 s(Depth, k = 5, bs = "cr") + 
#                 s(detrended,  k = 5, bs = "cr") + 
#                 s(tpi,    k = 5, bs = "cr"), 
#               data = habi, method = "REML", family = binomial("logit"))
# summary(m_rock)
# gam.check(m_rock)
# vis.gam(m_rock)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pmacroalgae" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "preef" = predict(m_reef, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf)
prasts$dom_tag <- which.max(prasts[[6:8]])
plot(prasts)

# categorise by dominant tag
preddf$dom_tag <- apply(preddf[8:10], 1,
                        FUN = function(x){names(which.max(x))})
preddf$dom_tag <- sub('.', '', preddf$dom_tag)
head(preddf)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

# tidy and output data
spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)
spreddf$dom_tag <- (names(spreddf)[8:10])[spreddf$dom_tag]

saveRDS(preddf, "output/habitat_fssgam/broad_habitat_predictions.rds")
saveRDS(spreddf, "output/habitat_fssgam/site_habitat_predictions.rds")
