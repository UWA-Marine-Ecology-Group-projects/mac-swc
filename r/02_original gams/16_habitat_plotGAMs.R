###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Habitat GAM plots
# author:  Claude
# date:    Nov-Dec 2021
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
name <- "2021-05_Abrolhos_Habitat" # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Load the dataset -
#habitat
dat <- readRDS("data/tidy/merged_habitat.rds")                                 # merged data from 'R/1_mergedata.R'

dat <- dat[ , -c(9:49)]

colnames(dat)
dat <- melt(dat, measure.vars = c(21:26))%>%                                  # collect all taxa tags for univariate stats   
  glimpse() 

dat <- dat %>%
  mutate(logdepth = log(Depth)) %>%
  # mutate(sqrttri = sqrt(tri)) %>%
  mutate(sqrtrough = sqrt(roughness)) %>%
  # mutate(sqrtslope = sqrt(slope)) %>%
  rename(Taxa = variable) %>%
  rename(response = value) %>%
  ga.clean.names()

# Manually make the most parsimonious GAM models for each taxa ----
#### Abrolhos habitat ####
unique(dat$taxa)
names(dat)

# MODEL Kelps (depth + roughness + tpi) ----
dat.kelps <- dat %>% filter(taxa%in%"kelps")

gamm=gam(cbind(response, (totalpts - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
         family = binomial("logit"), method = "REML", data=dat.kelps)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.kelps.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.kelps.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
mod<-gamm
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.kelps.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for kelp ----
# depth ----
ggmod.kelp.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.kelps,aes(x=depth,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.kelps.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.kelps.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.kelps.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Kelp") +
  theme(plot.title = element_text(hjust = 0))
ggmod.kelp.depth

# PLOTS for kelp ----
# roughness ----
ggmod.kelp.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.kelps,aes(x=roughness,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.kelps.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.kelps.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.kelps.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.kelp.roughness

# PLOTS for kelp ----
# tpi ----
ggmod.kelp.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.kelps,aes(x=tpi,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.kelps.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.kelps.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.kelps.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.kelp.tpi

# MODEL Macroalgae (depth + detrended + roughness) ----
dat.macro <- dat %>% filter(taxa%in%"macroalgae")

gamm=gam(cbind(response, (totalpts - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5),
         family = binomial("logit"),method = 'REML',data=dat.macro)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macro.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macro.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macro.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for macroalgae ----
# depth ----
ggmod.macroalgae.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.kelps,aes(x=depth,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macro.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.macro.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macro.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Macroalgae") +
  theme(plot.title = element_text(hjust = 0))
ggmod.macroalgae.depth

# PLOTS for macroalgae ----
# roughness ----
ggmod.macro.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.macro,aes(x=roughness,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macro.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.macro.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macro.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.macro.roughness

# PLOTS for macroalgae ----
# detrended ----
ggmod.macro.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.kelps,aes(x=detrended,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macro.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.macro.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macro.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.macro.detrended

# MODEL biogenic reef (depth + detrended + roughness) ----
dat.biog <- dat %>% filter(taxa%in%"biog")

gamm=gam(cbind(response, (totalpts - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5),
         family = binomial("logit"),method = 'REML',data=dat.biog)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.biog.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.biog.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.biog.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for biogenic reef ----
# depth ----
ggmod.biog.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.biog,aes(x=depth,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.biog.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.biog.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.biog.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Biogenic reef") +
  theme(plot.title = element_text(hjust = 0))
ggmod.biog.depth

# PLOTS for biogenic reef ----
# roughness ----
ggmod.biog.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.biog,aes(x=roughness,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.biog.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.biog.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.biog.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.biog.roughness

# PLOTS for biogenic reef ----
# detrended ----
ggmod.biog.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.biog,aes(x=detrended,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.biog.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.biog.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.biog.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.biog.detrended

# MODEL sand  (depth + roughness + tpi) ----
dat.sand <- dat %>% filter(taxa%in%"sand")

gamm=gam(cbind(response, (totalpts - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
         family = binomial("logit"),method = 'REML',data=dat.sand)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
mod<-gamm
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
mod<-gamm
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for sand ----
# depth ----
ggmod.sand.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sand,aes(x=depth,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sand") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sand.depth

# PLOTS for sand ----
# roughness ----
ggmod.sand.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sand,aes(x=roughness,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.roughness

# PLOTS for sand ----
# tpi ----
ggmod.sand.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.sand,aes(x=tpi,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.tpi

# MODEL rock  (depth + detrended + tpi) ----
dat.rock <- dat %>% filter(taxa%in%"rock")

gamm=gam(cbind(response, (totalpts - response)) ~ 
           s(depth, bs = 'cr', k = 5)+s(detrended, bs = 'cr', k = 5)+s(tpi, bs = 'cr', k = 5),
         family = binomial("logit"),method = 'REML',data=dat.rock)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
mod<-gamm
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
mod<-gamm
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 100),
                        depth=mean(mod$model$depth),
                        detrended=mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for rock ----
# depth ----
ggmod.rock.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.rock,aes(x=depth,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Rock") +
  theme(plot.title = element_text(hjust = 0))
ggmod.rock.depth

# PLOTS for rock ----
# detrended ----
ggmod.rock.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.rock,aes(x=detrended,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.detrended

# PLOTS for rock ----
# tpi ----
ggmod.rock.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.rock,aes(x=tpi,y=response/totalpts),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.tpi,aes(x=tpi,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.tpi,aes(x=tpi,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.tpi,aes(x=tpi,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.tpi

# Combine with cowplot
library(cowplot)

# view plots
plot.grid.habitat <- plot_grid(ggmod.kelp.depth, ggmod.kelp.roughness,ggmod.kelp.tpi,
                       ggmod.macroalgae.depth, ggmod.macro.detrended,ggmod.macro.roughness,
                       ggmod.biog.depth, ggmod.biog.detrended, ggmod.biog.roughness,
                       ggmod.sand.depth,ggmod.sand.roughness,ggmod.sand.tpi,
                       ggmod.rock.depth,ggmod.rock.detrended,ggmod.rock.tpi,
                       ncol = 3, labels = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o'),align = "vh")
plot.grid.habitat

#save plots
save_plot("plots/abrolhos.habitat.gam.png", plot.grid.habitat,base_height = 9,base_width = 8.5)