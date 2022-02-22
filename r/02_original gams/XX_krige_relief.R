###
# Project: Parks - Abrolhos Post-Survey
# Data:    Relief data (scored during fish counts)
# Task:    Kriging relief
# author:  Kingsley Griffin
# date:    Feb 2022
##

library(INLA)
library(sp)
library(raster)
library(ggnewscale)
library(ggplot2)
library(viridis)

# bring in data
habi  <- readRDS("data/tidy/dat.full.habitat.rds")                                # merged data from 'R/1_mergedata.R'
preds <- readRDS("data/spatial/250m_spatialcovariates_utm.rds")                   # spatial covs from 'R/1_mergedata.R'

# spatial setup
wgscrs  <- CRS("+proj=longlat +datum=WGS84")
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
plot(preds)
names(preds)[1] <- "depth"

# trim predictor data cols
habi <- habi[ , c(1, 2, 21, 23, 25:26)]
head(habi)

# transform habitat coordinates, extract from covariates
habisp <- SpatialPointsDataFrame(coords = habi[6:5], data = habi, 
                                 proj4string = wgscrs)
habisp <- spTransform(habisp, sppcrs)
habisp <- cbind(habisp, data.frame(raster::extract(preds, habisp)))
habi <- as.data.frame(habisp, xy = T)
saveRDS(habi, "data/tidy/habitat_spatialcovs.rds")

# plot relationships quickly
ggplot(habi, aes(depth, mean.relief)) +     geom_point(alpha = 0.2) + geom_smooth()
ggplot(habi, aes(detrended, mean.relief)) + geom_point(alpha = 0.2) + geom_smooth()
ggplot(habi, aes(roughness, mean.relief)) + geom_point(alpha = 0.2) + geom_smooth()

# Build with all data, or set aside test/train data
traind <- habi
# # OR set aside train/test data
# set.seed(42)
# testd  <- habi[sample(nrow(habi), nrow(habi)/5), ]
# traind <- habi[!habi$sample %in% testd$sample , ]

# build inla mesh from spatial layout of sites - the constants need some tuning
sitecoords     <- traind[11:12]
sitelocs       <- as.matrix(sitecoords)
max.edgelength <- c(1000, 12000)
mesha          <- inla.mesh.2d(loc = sitelocs, max.edge = max.edgelength,
                               offset = c(500, 2000), cutoff = 300)
plot(mesha)
plot(habisp, add = T, col = "red")

# prep for modelling
meshadata      <- inla.spde.make.A(mesha, sitelocs)
spde           <- inla.spde2.matern(mesha, alpha = 2)
datn           <- nrow(traind)
preddf         <- traind[, colnames(traind) %in% c("depth", "roughness", 
                                                   "tpi", "detrended")]

relief_stack   <- inla.stack(data = list(y = traind$mean.relief),
                             A = list(meshadata, 1),
                             effects = list(c(sp = list(1:mesha$n)),
                                            list(depth = preddf$depth,
                                                 rough = preddf$roughness,
                                                 # tpi   = preddf$tpi,
                                                 dtren = preddf$detrended)),
                             remove.unused = TRUE)

modform <- y ~ 1 + depth + rough + dtren  + f(sp, model = spde)

# fit model
m1 <- inla(modform, 
           # family = "poisson",
           data = inla.stack.data(relief_stack),
           control.predictor = list(A = inla.stack.A(relief_stack)))

summary(m1)

# plot effect sizes
avgs <- rbind(m1$summary.fixed)
avgs$covariate <- gsub("[0-9]", "", rownames(avgs)) 
colnames(avgs)[3:5] <- c("lwrq", "midq", "uprq")
avgs$sig <-  ifelse(avgs$lwrq < 0 & avgs$uprq < 0, 1, 
                    ifelse(avgs$lwrq > 0 & avgs$uprq > 0, 1, 0))
avgs$covariate <- dplyr::recode(
  avgs$covariate,
  dtren = "detrended",
  rough = "roughness"
)

p1 <- ggplot(avgs[avgs$covariate != "(Intercept)", ], 
             aes(covariate, mean, colour = as.factor(sig))) +  
  geom_point(shape = 20) + 
  geom_errorbar(aes(ymin = lwrq, ymax = uprq), width = 0.1, size = 0.2) + 
  geom_hline(aes(yintercept = 0), lty = 3) +
  scale_colour_viridis(option = "B", discrete = TRUE, begin = 0, end = 0.6) +
  labs(x = NULL, y = NULL) + guides(colour = "none") +
  theme_bw()
p1

ggsave("plots/relief_model/reliefmodel_ci.png", width = 5, height = 4, dpi = 160)

# evaluate fit and tuning
rf <- inla.spde.result(inla = m1, name = "sp", spde = spde, do.transf = TRUE)

par(mfrow=c(3,3))
plot(rf$marginals.variance.nominal[[1]], type="l",xlab=expression(sigma[x]^2), ylab="Density", main = "SPDE local var")
plot(rf$marginals.kap[[1]],type="l", xlab=expression(kappa), ylab="Density")
plot(m1$marginals.hy[[1]], type="l", ylab="Density",xlab=expression(phi))
# plot(m1$marginals.fix$Intercept, type="l", xlab="Intercept",ylab="Density")
# plot(mod$marginals.fixed$nmoorid, type="l", ylab="Density",xlab= "nmoorid")
# abline(v = m1$summary.fixed$mean, col = 'red')
plot(m1$summary.random$s[,1:2], type="l",xlab="Spatial", ylab="random effect")
plot.default(rf$marginals.range.nominal[[1]], type="l",xlab="Practical range", ylab="Density")
abline(v = max.edgelength[1], col = 'red')
abline(v = max.edgelength[2], col = 'red')

#as well as the posterior precision of the random effect.
plot(m1$marginals.hy[[2]], type="l", ylab="Density",xlab=names(m1$marginals.hy)[2])

dev.off()

# predict spatial random effect back onto mesh from spde model fit
ypred <- m1$summary.random$s$mean
# xlim  <- c(extent(preds)[1], extent(preds)[2])
# ylim  <- c(extent(preds)[3], extent(preds)[4])
# xlim  <- c(-102500 , 258000)
# ylim  <- c(6775500, 7123250)
# xdims <- (xlim[2] - xlim[1]) / 250
# ydims <- (ylim[2] - ylim[1]) / 250

xlim  <- c(extent(preds)[1], extent(preds)[2])
ylim  <- c(extent(preds)[3], extent(preds)[4])
xdims <- (xlim[2] - xlim[1]) / res(preds)[1]
ydims <- (ylim[2] - ylim[1]) / res(preds)[2]


proj       <- inla.mesh.projector(mesha, xlim = xlim, ylim = ylim, 
                                  dims = c(xdims, ydims))
field.proj <- inla.mesh.project(proj, ypred)

datpred <- data.frame(x = rep(proj$x, ydims), 
                      y = rep(proj$y, each = xdims), 
                      pred = as.numeric(field.proj))

datpred <- na.omit(datpred)

predrast <- rasterize(x = cbind(datpred$x, datpred$y), 
                      y = preds, field = datpred$pred)

# predict relief score across mesh using model formula
modout  <- m1$summary.fixed
hypout  <- m1$summary.hyperpar
pmask   <- predrast / predrast
pcells  <- preds[[c(1, 3, 4)]] * pmask
pcells  <- stack(pcells, predrast)
names(pcells) <- c("depth", "rough", "dtren", "p_sp")
pcelldf <- as.data.frame(pcells, na.rm = TRUE, xy = TRUE)
head(pcelldf)

# recall formula: y ~ 1 + depth + rough + dtren + f(sp, model = spde)
pcelldf$prelief <- 1 + modout$mean[1] + 
  (pcelldf$depth * modout$mean[2]) + 
  (pcelldf$rough * modout$mean[3]) + 
  (pcelldf$dtren * modout$mean[4]) + 
  pcelldf$p_sp

pcelldf$prelief[pcelldf$prelief < 0] <- 0                                       # rm p out of sample range (-ve relief)

prelief <- rasterFromXYZ(cbind(pcelldf[c(1:2, 6:7)]))
plot(prelief[[2]])
saveRDS(prelief[[2]], "output/spatial/raster/predicted_relief_raster.rds")

sitebuf <- buffer(habisp, 10000)
prelief <- mask(prelief, sitebuf)
prelief <- crop(prelief, extent(sitebuf))
plot(prelief)
# plot(habisp, add = TRUE, col = "red")
pcelldf <- as.data.frame(prelief, xy = TRUE, na.rm = TRUE)

saveRDS(pcelldf, 'output/spatial/raster/predicted_relief_site.rds')

ggplot(pcelldf, aes(x, y)) +
  geom_tile(aes(fill = prelief)) +
  scale_fill_viridis() +
  geom_point(data = habi_t, aes(longitude.1, latitude.1, colour = mean.relief),
             alpha = 4/5, size = 1) +
  scale_colour_viridis() +
  coord_equal() +
  labs(x= NULL, y = NULL, 
       fill = "p. relief (map)", 
       colour = "obs. relief (points)") +
  theme_minimal()

# spatial random effect
ggplot(pcelldf, aes(x, y)) +
  geom_tile(aes(fill = p_sp)) +
  scale_fill_viridis() +
  geom_point(data = habi_t, aes(longitude.1, latitude.1), 
             alpha = 1/5, size = 1, shape = 3) +
  coord_equal() +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  theme_minimal()

## if running on a k-fold subset (see lines 50-51)
# perform quick cross-validation (single fold with 20% of data)
testsp <- SpatialPointsDataFrame(coords = testd[11:12], data = testd)
testd$predicted <- extract(prelief[[2]], testsp)
testd$pdiff     <- testd$predicted - testd$mean.relief
testsp$pdiff    <- testd$pdiff
testd <- na.omit(testd) # there is an NA - there are some gaps in the rasters, that may be why

# calculate r2 as per Gelman et al 2017 and plot prediction accuracy 
# variance of predicted values divided by variances of predicted values plus variance of the errors

r2    <- var(testd$predicted) / (var(testd$predicted) + var(testd$pdiff))
r2lab <- paste("r^2 == ", round(r2, 3))

ggplot(testd, aes(mean.relief, predicted)) + 
  geom_abline(intercept = 0, lty = 3) +
  geom_point(alpha = 4/5, size = 1) + 
  geom_smooth(method = "gam", colour = "grey60", size = 0.2, fill = "grey80") +
  annotate("text", x = 0.3, y = 2.4, label = r2lab, parse = TRUE) + 
  coord_equal() +
  theme_minimal() + 
  labs(x = "observed")

ggsave("plots/relief_model/relief_prediction_accuracy.png", width = 5, height = 4, dpi = 160)

# # plot difference across sites? i.e. way to view spatial prediction success?
# 
# ggplot(pcelldf, aes(x, y)) +
#   geom_tile(aes(fill = prelief)) +
#   scale_fill_viridis() +
#   geom_point(data = testsp@data, aes(Longitude.1, Latitude.1, colour = pdiff/relief)) +
#   scale_colour_viridis(direction = -1) +
#   coord_equal()

