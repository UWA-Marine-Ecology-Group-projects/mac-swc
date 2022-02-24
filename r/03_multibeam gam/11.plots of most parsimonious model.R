###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Plots of the most parsimonious model - multibeam patch
# author:  Claude
# date:    February 2022
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
library(patchwork)
library(cowplot) #for save_plot

# Set the study name
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

# plots of the most parsimonious models----
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

# Bring in and format the data----
dat <- readRDS('data/tidy/dat.maxn.multibeam.rds')%>%
  glimpse()

dat.length <- readRDS('data/tidy/dat.length.multibeam.rds')%>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----
unique(dat$scientific)

# MODEL Total abundance (mean.relief) ----
dat.tot <- dat %>% filter(scientific=="total.abundance")

mod=gam(maxn~s(mean.relief,k=3,bs='cr') + s(site,bs="re")+method, family=tw,data=dat.tot)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        site=(mod$model$site),
                        method=(mod$model$method))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.tot.mean = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for Total abundance ----
# mean relief ----
ggmod.total.mean <- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.tot,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.tot.mean,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.tot.mean,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.tot.mean,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.mean

# MODEL Species richness (mean.relief) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(site,bs="re")+method, family=tw,data=dat.sr)

# predict - relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        site=(mod$model$site),
                        method=(mod$model$method))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# mean relief ----
ggmod.sr.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.sr,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sr.relief

# MODEL greater than legal size (broad.reef + detrended + tpi) ----
dat.length$sample <- as.factor(dat.length$sample)
dat.length$status <- as.factor(dat.length$status)
dat.length$scientific <- as.factor(dat.length$scientific)
dat.length$site <- as.factor(dat.length$site)

dat.leg <- dat.length %>% filter(scientific=="greater than legal size")

mod=gam(number~s(broad.reef,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(tpi,k=3,bs='cr') + s(site,bs="re"), family=tw,data=dat.leg)

# predict - reef ----
testdata <- expand.grid(broad.reef=seq(min(dat.length$broad.reef),max(dat.length$broad.reef),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.reef = testdata%>%data.frame(fits)%>%
  group_by(broad.reef)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat.length$detrended),max(dat.length$detrended),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat.length$tpi),max(dat.length$tpi),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        detrended=mean(mod$model$detrended),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# broad.reef ----
ggmod.leg.reef<- ggplot() +
  ylab("")+
  xlab("Reef")+
  geom_point(data=dat.leg,aes(x=broad.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.reef,aes(x=broad.reef,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.reef,aes(x=broad.reef,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.reef,aes(x=broad.reef,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.leg.reef

# detrended ----
ggmod.leg.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.leg,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.detrended,aes(x=detrended,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.detrended,aes(x=detrended,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.detrended,aes(x=detrended,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.detrended

# tpi ----
ggmod.leg.tpi<- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.leg,aes(x=tpi,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.tpi,aes(x=tpi,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.tpi,aes(x=tpi,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.tpi,aes(x=tpi,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.tpi

# MODEL smaller than legal size (roughness) ----
dat.sub <- dat.length %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(roughness,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.sub)

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat.length$roughness),max(dat.length$roughness),length.out = 20),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sub.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Smaller than legal size ----
# roughness ----
ggmod.sub.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sub,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sub.roughness

# Combine wth patchwork
# view plots
plot.grid <- ggmod.total.mean+plot_spacer()+plot_spacer()+
  ggmod.sr.relief+plot_spacer()+plot_spacer()+
  ggmod.leg.reef+ggmod.leg.detrended+ggmod.leg.tpi+
  ggmod.sub.roughness+plot_spacer()+plot_spacer()+
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid

#save output
save_plot("plots/multibeam gamms/swc.gam.plots.png", plot.grid,base_height = 9,base_width = 8.5)
