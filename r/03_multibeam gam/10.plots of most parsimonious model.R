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

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

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

# MODEL Total abundance (mean.relief + tpi) ----
dat.tot <- dat %>% filter(scientific=="total.abundance")

mod=gam(maxn~s(mean.relief,k=3,bs='cr') + s(tpi,k=3,bs='cr') + s(site,bs="re"), family=tw,data=dat.tot)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.tot.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - tpi ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.tot.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# mean relief ----
ggmod.total.relief <- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.tot,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.tot.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.tot.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.tot.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.relief

# tpi ----
ggmod.total.tpi <- ggplot() +
  ylab("")+
  xlab("TPI")+
  geom_point(data=dat.tot,aes(x=tpi,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.tot.tpi,aes(x=tpi,y=maxn),alpha=0.5)+
  geom_line(data=predicts.tot.tpi,aes(x=tpi,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.tot.tpi,aes(x=tpi,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.tpi

# MODEL Species richness (mean.relief + roughness) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(roughness,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.sr)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.mean = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.rough = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# mean relief ----
ggmod.sr.mean<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.sr,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.mean,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.mean,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.mean,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sr.mean

#roughness
ggmod.sr.rough<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sr,aes(x=roughness,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.rough,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.rough,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.rough,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sr.rough

# MODEL greater than legal size (broad.reef + detrended + roughness) ----
dat.leg <- dat.length %>% filter(scientific=="greater than legal size")

mod=gam(number~s(broad.reef,k=3,bs='cr') + s(detrended,k=3,bs='cr')+ s(roughness,k=3,bs='cr') + s(site,bs="re"), family=tw,data=dat.leg)

# predict - reef ----
testdata <- expand.grid(broad.reef=seq(min(dat$broad.reef),max(dat$broad.reef),length.out = 20),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.reef = testdata%>%data.frame(fits)%>%
  group_by(broad.reef)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.detrend = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        detrended=mean(mod$model$detrended),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.rough = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# reef ----
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

# sd relief ----
ggmod.leg.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.leg,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.detrend,aes(x=detrended,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.detrend,aes(x=detrended,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.detrend,aes(x=detrended,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.detrended

# TPI ----
ggmod.leg.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.leg,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.rough,aes(x=roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.rough,aes(x=roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.rough,aes(x=roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.roughness

# MODEL smaller than legal size (roughness) ----
dat.sub <- dat.length %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(roughness,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.sub)

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
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

#just testing to see if this ribbon errorbar thing looks better
# ggmod.sub.roughness <- ggplot(aes(x=roughness,y=number), data=predicts.sub.roughness) +
#   ylab(" ")+
#   xlab('Roughness')+
#   geom_point(data=dat.sub,aes(x=roughness,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
#   geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number),colour="#3d5a80", alpha=0.5)+
#   geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
#   theme_classic()+
#   #ylim(0,200)+
#   Theme1
# #annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
# ggmod.sub.roughness

# Combine wth patchwork
library(patchwork)
library(cowplot) #for save_plot

# view plots
plot.grid <- ggmod.total.relief+ggmod.total.tpi+plot_spacer()+
  ggmod.sr.mean+ggmod.sr.rough+plot_spacer()+
  ggmod.leg.reef+ggmod.leg.detrended+ggmod.leg.roughness+
  ggmod.sub.roughness+plot_spacer()+plot_spacer()+
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid

save_plot("plots/multibeam gamms/swc.gam.plots.png", plot.grid,base_height = 9,base_width = 8.5)
