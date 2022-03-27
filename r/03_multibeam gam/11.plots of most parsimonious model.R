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
  dplyr::rename(depth='multibeam_derivatives_depth', detrended = 'multibeam_derivatives_detrended',
                roughness = 'multibeam_derivatives_roughness')%>%
  glimpse()

dat.length <- readRDS('data/tidy/dat.length.multibeam.rds')%>%
  dplyr::rename(depth='multibeam_derivatives_depth', detrended = 'multibeam_derivatives_detrended',
                roughness = 'multibeam_derivatives_roughness')%>%
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

# MODEL Species richness (depth) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

mod=gam(maxn~s(depth,k=3,bs='cr')+ s(site,bs="re")+method, family=tw,data=dat.sr)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        site=(mod$model$site),
                        method=(mod$model$method))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# depth ----
ggmod.sr.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sr,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sr.depth

# MODEL greater than legal size (mean.relief + detrended + roughness) ----
dat.length$sample <- as.factor(dat.length$sample)
dat.length$status <- as.factor(dat.length$status)
dat.length$scientific <- as.factor(dat.length$scientific)
dat.length$site <- as.factor(dat.length$site)

dat.leg <- dat.length %>% filter(scientific=="greater than legal size")

mod=gam(number~s(mean.relief,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(roughness,k=3,bs='cr') + 
          s(site,bs="re"), family=tw,data=dat.leg)

# predict - mean.relief ----
testdata <- expand.grid(mean.relief=seq(min(dat.length$mean.relief),max(dat.length$mean.relief),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat.length$detrended),max(dat.length$detrended),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat.length$roughness),max(dat.length$roughness),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        detrended=mean(mod$model$detrended),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# mean.relief ----
ggmod.leg.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.leg,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.relief,aes(x=mean.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.relief,aes(x=mean.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.relief,aes(x=mean.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.leg.relief

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

# roughness ----
ggmod.leg.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.leg,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.roughness,aes(x=roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.roughness,aes(x=roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.roughness,aes(x=roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.roughness

# MODEL smaller than legal size (broad.macroalgae) ----
dat.sub <- dat.length %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(broad.macroalgae,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.sub)

# predict - macroalgae ----
testdata <- expand.grid(broad.macroalgae=seq(min(dat.length$broad.macroalgae),max(dat.length$broad.macroalgae),length.out = 20),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sub.macro = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Smaller than legal size ----
# broad.macroalgae ----
ggmod.sub.macro<- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.sub,aes(x=broad.macroalgae,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sub.macro,aes(x=broad.macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.sub.macro,aes(x=broad.macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.macro,aes(x=broad.macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sub.macro

# Combine wth patchwork
# view plots
plot.grid <- ggmod.total.mean+plot_spacer()+plot_spacer()+
  ggmod.sr.depth+plot_spacer()+plot_spacer()+
  ggmod.leg.relief+ggmod.leg.detrended+ggmod.leg.roughness+
  ggmod.sub.macro+plot_spacer()+plot_spacer()+
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid

#save output
save_plot("plots/multibeam gamms/swc.gam.plots.png", plot.grid,base_height = 9,base_width = 8.5)
