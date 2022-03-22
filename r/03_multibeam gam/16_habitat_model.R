###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Multibeam Habitat modelling
# author:  Kingsley Griffin & Claude
# date:    Feb 2022
##
rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
habi   <- readRDS("data/tidy/habitat_multibeam_merged.rds")                               # merged data from 'R/1_mergedata.R'

# bring in multibeam derivatives and extract at sample locations
deriv_list <- list.files("data/spatial/rasters", "multibeam_derivatives",
                         full.names = TRUE)
mb_deriv   <- stack(deriv_list)
names(mb_deriv) <- c("mb_depth", "mb_detrended", "mb_roughness", "mb_tpi")
plot(mb_deriv)

preddf <- as.data.frame(mb_deriv, xy = TRUE, na.rm = TRUE)
preddf$mb_depth <- abs(preddf$mb_depth)

# reduce predictor space to fit survey area (removed because this is a fairly small patch)

# use formula from top model from '2_modelselect.R'
#macroalgae
m_macro <- gam(cbind(broad.macroalgae, broad.total.points.annotated - broad.macroalgae) ~ 
                 s(mb_depth, k = 3, bs = "cr")  + 
                 s(mb_detrended, k = 5, bs = "cr") + 
                 s(mb_roughness, k = 5, bs = "cr") +
                 s(mb_tpi, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro) # not a great fit..?
vis.gam(m_macro)

#biogenic reef (inverts)
m_biogenic <- gam(cbind(biogenic_reef, broad.total.points.annotated - biogenic_reef) ~ 
                s(mb_depth, k = 5, bs = "cr")  + 
               s(mb_detrended, k = 5, bs = "cr") +
                s(mb_roughness, k = 3, bs = "cr") +
                s(mb_tpi, k = 5, bs = "cr") , 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_biogenic)
gam.check(m_biogenic)
vis.gam(m_biogenic)

#reef (for fish modelling)
m_reef <- gam(cbind(broad.reef, broad.total.points.annotated - broad.reef) ~ 
                s(mb_depth, k = 5, bs = "cr")  + 
                s(mb_detrended, k = 5, bs = "cr") +
                s(mb_roughness, k = 3, bs = "cr") +
                s(mb_tpi, k = 5, bs = "cr") , 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)
gam.check(m_reef)
vis.gam(m_reef)

#sand
m_sand <- gam(cbind(broad.unconsolidated, 
                    broad.total.points.annotated - broad.unconsolidated) ~ 
                s(mb_depth,   k = 3, bs = "cr") + 
                s(mb_detrended, k = 5, bs = "cr") +
                s(mb_roughness, k = 3, bs = "cr") +
                s(mb_tpi,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)

#rock
m_rock <- gam(cbind(broad.consolidated, 
                    broad.total.points.annotated - broad.consolidated) ~
                s(mb_depth,   k = 3, bs = "cr") + 
                s(mb_detrended, k = 3, bs = "cr") +
                s(mb_roughness, k = 3, bs = "cr") +
                s(mb_tpi,       k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
vis.gam(m_rock)

#seagrass
m_seagrass <- gam(cbind(broad.seagrasses, 
                        broad.total.points.annotated - broad.seagrasses) ~
                    s(mb_depth,   k = 5, bs = "cr") + 
                    s(mb_detrended, k = 5, bs = "cr") +
                    s(mb_roughness, k = 3, bs = "cr") +
                    s(mb_tpi,       k = 5, bs = "cr"),
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrass)
gam.check(m_seagrass)
vis.gam(m_seagrass)

#sponges
m_sponges <- gam(cbind(broad.sponges, 
                       broad.total.points.annotated - broad.sponges) ~
                   s(mb_depth,   k = 5, bs = "cr") +
                   # s(mb_detrended, k = 3, bs = "cr") +
                   s(mb_roughness, k = 3, bs = "cr") + 
                   s(mb_tpi,       k = 3, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))
summary(m_sponges)
gam.check(m_sponges)
vis.gam(m_sponges)

# predict, rasterise and plot
p_habitat <- cbind(preddf[1:2], 
                   "pmacroalgae" = predict(m_macro, preddf, type = "response"),
                   "psand" = predict(m_sand, preddf, type = "response"),
                   "preef" = predict(m_reef, preddf, type = "response"),
                   "pseagrass" = predict(m_seagrass, preddf, type = "response"),
                   "psponges" = predict(m_sponges, preddf, type = "response"),
                   "prock" = predict(m_rock, preddf, type = "response"),
                   "pbiogenic" = predict(m_biogenic, preddf, type = "response"))

pmacroalgae <- rasterFromXYZ(p_habitat[, c(1, 2, 3)], res = c(4, 4))
psand <- rasterFromXYZ(p_habitat[, c(1, 2, 4)], res = c(4, 4))
preef <- rasterFromXYZ(p_habitat[, c(1, 2, 5)], res = c(4, 4))
pseagrass <- rasterFromXYZ(p_habitat[, c(1, 2, 6)], res = c(4, 4))
psponges <- rasterFromXYZ(p_habitat[, c(1, 2, 7)], res = c(4, 4))
prock <- rasterFromXYZ(p_habitat[, c(1, 2, 8)], res = c(4, 4))
pbiogenic <- rasterFromXYZ(p_habitat[, c(1, 2, 9)], res = c(4, 4))
phab_rasts <- stack(pmacroalgae,psand,preef,pseagrass,psponges,prock,pbiogenic)
phab_rasts <- raster::aggregate(phab_rasts, fact = 10, fun = mean) #scale to 40 x 40 res
phab_rasts

plot(phab_rasts)
phab_rasts <- as.data.frame(phab_rasts, xy = TRUE, na.rm = TRUE)

# categorise by dominant tag
phab_rasts$dom_tag <- apply(phab_rasts[c(3,4,6,7,8,9)], 1,
                        FUN = function(x){names(which.max(x))})
phab_rasts$dom_tag <- sub('p', '', phab_rasts$dom_tag)
head(phab_rasts)

# split and output data (whole data are too large for git agh)
nphab <- nrow(phab_rasts)/10

phaba <- phab_rasts[1:nphab, ]
phabb <- phab_rasts[(1 + nphab):(nphab * 2), ]
phabc <- phab_rasts[(1 + (nphab * 2)):(nphab * 3), ]
phabd <- phab_rasts[(1 + (nphab * 3)):(nphab * 4), ]
phabe <- phab_rasts[(1 + (nphab * 4)):(nphab * 5), ]
phabf <- phab_rasts[(1 + (nphab * 5)):(nphab * 6), ]
phabg <- phab_rasts[(1 + (nphab * 6)):(nphab * 7), ]
phabh <- phab_rasts[(1 + (nphab * 7)):(nphab * 8), ]
phabi <- phab_rasts[(1 + (nphab * 8)):(nphab * 9), ]
phabj <- phab_rasts[(1 + (nphab * 9)):(nphab * 10), ]

saveRDS(phaba, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_a.rds")
saveRDS(phabb, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_b.rds")
saveRDS(phabc, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_c.rds")
saveRDS(phabd, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_d.rds")
saveRDS(phabe, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_e.rds")
saveRDS(phabf, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_f.rds")
saveRDS(phabg, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_g.rds")
saveRDS(phabh, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_h.rds")
saveRDS(phabi, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_i.rds")
saveRDS(phabj, "output/multibeam_habitat_fssgam/multibeam_habitat_pred_j.rds")

