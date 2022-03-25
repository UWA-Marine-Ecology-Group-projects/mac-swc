###
# Project: mac swc
# Data:    BOSS & BRUV fish, habitat
# Task:    Habitat GAM plots
# author:  Claude
# date:    March 2022
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)
library(reshape2)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Set the study name
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

#### Set working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Load the dataset ----
#habitat
dat <- readRDS("data/tidy/habitat_merged.rds")%>%
  dplyr::select(campaignid,sample,broad.consolidated,broad.macroalgae,
                biogenic_reef,broad.seagrasses,broad.sponges,broad.unconsolidated,
                broad.total.points.annotated, depth, tpi, roughness,
                detrended)%>%
  reshape2::melt(measure.vars = c(3:8))%>% # collect all taxa tags for univariate stats   
  rename(taxa = variable,response = value) %>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos habitat ####
unique(dat$taxa)
names(dat)

# MODEL consolidated rock (depth + detrended + roughness) ----
dat.rock <- dat %>% filter(taxa%in%"broad.consolidated")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
           s(roughness, bs = 'cr', k = 5),
         family = binomial("logit"), method = "REML", data=dat.rock)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for consolidated rock ----
# depth ----
ggmod.rock.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.rock,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Consolidated (rock)") +
  theme(plot.title = element_text(hjust = 0))
ggmod.rock.depth

# detrended ----
ggmod.rock.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.rock,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.detrended

# roughness ----
ggmod.rock.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.rock,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.roughness

# MODEL macroalgae (depth + detrended + tpi) ----
dat.weed <- dat %>% filter(taxa%in%"broad.macroalgae")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
          s(tpi, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.weed)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.weed.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.weed.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.weed.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for macroalgae ----
# depth ----
ggmod.weed.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.weed,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.weed.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.weed.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.weed.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Macroalgae") +
  theme(plot.title = element_text(hjust = 0))
ggmod.weed.depth

# detrended ----
ggmod.weed.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.weed,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.weed.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.weed.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.weed.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.weed.detrended

# tpi ----
ggmod.weed.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.weed,aes(x=tpi,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.weed.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.weed.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.weed.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.weed.tpi

# MODEL biogenic reef (depth + detrended + roughness) ----
dat.reef <- dat %>% filter(taxa%in%"biogenic_reef")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
          s(roughness, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.reef)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.reef.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.reef.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.reef.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for biogenic reef ----
# depth ----
ggmod.reef.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.reef,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.reef.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.reef.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.reef.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Biogenic reef") +
  theme(plot.title = element_text(hjust = 0))
ggmod.reef.depth

# detrended ----
ggmod.reef.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.reef,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.reef.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.reef.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.reef.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.reef.detrended

# roughness ----
ggmod.reef.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.reef,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.reef.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.reef.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.reef.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.reef.roughness

# MODEL Seagrasses (depth + detrended + tpi) ----
dat.grass <- dat %>% filter(taxa%in%"broad.seagrasses")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
          s(tpi, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.grass)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.grass.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.grass.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.grass.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for seagrass ----
# depth ----
ggmod.grass.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.grass,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.grass.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.grass.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.grass.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Seagrass") +
  theme(plot.title = element_text(hjust = 0))
ggmod.grass.depth

# detrended ----
ggmod.grass.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.grass,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.grass.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.grass.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.grass.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.grass.detrended

# tpi ----
ggmod.grass.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.grass,aes(x=tpi,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.grass.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.grass.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.grass.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.grass.tpi

# MODEL Sponges (depth + detrended + roughness) ----
dat.sponge <- dat %>% filter(taxa%in%"broad.sponges")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
          s(roughness, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.sponge)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sponge.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sponge.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sponge.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for sponges ----
# depth ----
ggmod.sponge.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sponge,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sponge.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.sponge.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sponge.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sponges") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sponge.depth

# detrended ----
ggmod.sponge.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.sponge,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sponge.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.sponge.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sponge.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sponge.detrended

# roughness ----
ggmod.sponge.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sponge,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sponge.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.sponge.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sponge.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sponge.roughness

# MODEL sand (depth + detrended + roughness) ----
dat.sand <- dat %>% filter(taxa%in%"broad.unconsolidated")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+
          s(roughness, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.sand)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# PLOTS for sand ----
# depth ----
ggmod.sand.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sand,aes(x=depth,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Unconsolidated (sand)") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sand.depth

# detrended ----
ggmod.sand.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.sand,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.detrended

# roughness ----
ggmod.sand.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sand,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.roughness

# Combine with cowplot
library(patchwork)
library(cowplot)

plot.grid <- ggmod.reef.depth+ggmod.reef.detrended+ggmod.reef.roughness+
  ggmod.rock.depth+ggmod.rock.detrended+ggmod.rock.roughness+
  ggmod.weed.depth+ggmod.weed.detrended+ggmod.weed.tpi+
  ggmod.grass.depth+ggmod.grass.detrended+ggmod.grass.tpi+
  ggmod.sponge.depth+ggmod.sponge.detrended+ggmod.sponge.roughness+
  ggmod.sand.depth+ggmod.sand.detrended+ggmod.sand.roughness+
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 6)
plot.grid

save_plot("plots/original gamms/swc.habitat.gam.plots.png", plot.grid,base_height = 9,base_width = 8.5)
