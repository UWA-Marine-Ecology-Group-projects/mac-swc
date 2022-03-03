###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Model selection 
# author:  Kingsley Griffin from @beckyfisher/FSSgam
# date:    Feb 2022

# Part 1-FSS modeling----

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(raster)
library(tidyr)
library(dplyr)
library(mgcv)
# library(MuMIn)
# library(car)
# library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
# library(gamm4)
# library(RCurl) #needed to download data from GitHub
library(reshape2)
library(FSSgam)

rm(list=ls())

# install fssgam package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once

# Bring in and format the data----
habi  <- readRDS("data/tidy/dat.full.habitat.rds")%>%                              # merged data from ??
  dplyr::mutate(id=paste(campaignid,sample, sep = "."))%>%
  distinct() #there is one sample with a duplicate

spcov <- readRDS("data/tidy/habitat_spatialcovs.rds")%>%
  dplyr::mutate(id=paste(campaignid,sample, sep = "."))%>%
  distinct() #there is one sample with a duplicate

habi <- habi %>%
  dplyr::left_join(spcov, by = "id")%>%
  glimpse()

# bring in multibeam derivatives and extract at sample locations
deriv_list <- list.files("data/spatial/rasters", "multibeam_derivatives",
                         full.names = TRUE)
mb_deriv   <- stack(deriv_list)
names(mb_deriv) <- c("mb_depth", "mb_detrended", "mb_roughness", "mb_tpi")
plot(mb_deriv)

habisp <- SpatialPointsDataFrame(coords = habi[48:49], data = habi)
habi   <- cbind(habi, extract( mb_deriv, habisp))
habi   <- habi[!is.na(habi$mb_depth), ]
habi$mb_depth <- abs(habi$mb_depth)
summary(habi)

saveRDS(habi, "data/tidy/habitat_multibeam_merged.rds")

# Set predictor variables---
pred.vars <- c("mb_depth", "mb_detrended", "mb_tpi", "mb_roughness") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(habi[ , pred.vars]), 2)

# habi <- habi[ , -c(9:49)]
colnames(habi)
habi <- melt(habi, measure.vars = c(5, 9, 11:13, 17))                               # collect all taxa tags for univariate stats
head(habi)

# rename taxa and response for the loop
habi <- habi %>%
  rename(Taxa = variable) %>%
  rename(response = value)

# Check to make sure Response vector has not more than 80% zeros----
unique.vars     <- unique(as.character(habi$Taxa))
unique.vars.use <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- habi[which(habi$Taxa == unique.vars[i]),]
  if(length(which(temp.dat$response == 0)) / nrow(temp.dat) < 0.8){
    unique.vars.use <- c(unique.vars.use, unique.vars[i])}
}
unique.vars.use
# unique.vars.use <- unique.vars[unique.vars != "turf"]
# unique.vars.use <- unique.vars.use[11:12] # only macroalgae and sponge of interest. remove unknown, open water
# unique.vars.use     

# Run the full subset model selection----
outdir    <- ("output/multibeam_habitat_fssgam/") #Set wd for example outputs
resp.vars <- unique.vars.use
use.dat   <- habi[habi$Taxa %in% c(unique.vars.use), ]
# factor.vars <- c("Status")# Status as a Factor with two levels
out.all <- list()
var.imp <- list()
name <- "eg"

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- habi[habi$Taxa == resp.vars[i],]
  use.dat <- use.dat[!(use.dat$broad.total.points.annotated - 
                         use.dat$response < 0), ] # added to fix weird point
  # use.dat$Site <- as.factor(use.dat$Site)
  Model1  <- gam(cbind(response, (broad.total.points.annotated - response)) ~ 
                   s(depth.y, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  # pred.vars.fact=factor.vars,
                                  # linear.vars="Distance",
                                  # cyclic.vars = c("aspect"),
                                  k = 5,
                                  cov.cutoff = 0.7
                                  # null.terms = "s(Site, bs='re')"
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T, n.cores = 8)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 10), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(outdir, name, "all.mod.fits.csv", sep = ""))
write.csv(all.var.imp,         file = paste(outdir, name, "all.var.imp.csv", sep = ""))
out.all
# Generic importance plots- - unsure why we're not getting any value for the other preds. internal m.cor exclusion?
heatmap.2(all.var.imp, notecex = 0.4,  dendrogram = "none",
          col = colorRampPalette(c("white", "yellow", "red"))(10),
          trace = "none", key.title = "", keysize = 2,
          notecol = "black", key = T,
          sepcolor = "black", margins = c(20, 20), lhei = c(2, 6), Rowv = FALSE, Colv = FALSE)
