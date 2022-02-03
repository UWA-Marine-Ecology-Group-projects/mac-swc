###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Plots of the most parsimonious model - full BRUV sample extent (MaxN)
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
dat <- readRDS('data/tidy/dat.maxn.full.rds')%>%
  glimpse()

dat.length <- readRDS('data/tidy/dat.length.full.rds')%>%
  dplyr::filter(scientific%in%c("greater than legal size","smaller than legal size"))%>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----
unique(dat$scientific)

# MODEL Total abundance (mean.relief) ----
dat.tot <- dat %>% filter(scientific=="total.abundance")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.tot)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.tot.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# mean relief ----
ggmod.total.relief<- ggplot() +
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

# MODEL Species richness (mean.relief + sd relief) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(sd.relief,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.sr)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.mean = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd relief ----
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.sd = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
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

#sd relief
ggmod.sr.sd<- ggplot() +
  ylab("")+
  xlab("SD relief")+
  geom_point(data=dat.sr,aes(x=sd.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.sd,aes(x=sd.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.sd,aes(x=sd.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.sd,aes(x=sd.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sr.sd

# MODEL greater than legal size (broad.macroalgae + sd relief + tpi) ----
dat.leg <- dat.length %>% filter(scientific=="greater than legal size")

mod=gam(number~s(broad.macroalgae,k=3,bs='cr') + s(sd.relief,k=3,bs='cr')+ s(tpi,k=3,bs='cr') + s(site,bs="re"), family=tw,data=dat.leg)

# predict - macroalgae ----
testdata <- expand.grid(broad.macroalgae=seq(min(dat$broad.macroalgae),max(dat$broad.macroalgae),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.macro = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd relief ----
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.relief = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - TPI ----
testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.leg.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# macroalgae ----
ggmod.leg.macro<- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.leg,aes(x=broad.macroalgae,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.macro,aes(x=broad.macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.macro,aes(x=broad.macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.macro,aes(x=broad.macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.leg.macro

# sd relief ----
ggmod.leg.relief<- ggplot() +
  ylab("")+
  xlab("SD relief")+
  geom_point(data=dat.leg,aes(x=sd.relief,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.leg.relief,aes(x=sd.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.leg.relief,aes(x=sd.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.leg.relief,aes(x=sd.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.leg.relief

# TPI ----
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

# MODEL smaller than legal size (broad.sponges+depth+roughness) ----
dat.sub <- dat.length %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(broad.reef,k=3,bs='cr')+ s(roughness,k=3,bs='cr')+ status + s(site,bs="re"), family=tw,data=dat.sub)

# predict - broad.reef ----
testdata <- expand.grid(broad.reef=seq(min(dat$broad.reef),max(dat$broad.reef),length.out = 20),
                        roughness=mean(mod$model$roughness),
                        status=c('No-take','Fished'),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sub.reef = testdata%>%data.frame(fits)%>%
  group_by(broad.reef)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        status=c('No-take','Fished'),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sub.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - status ----
testdata <- expand.grid(roughness=mean(mod$model$roughness),
                        broad.reef=mean(mod$model$broad.reef),
                        status=c('No-take','Fished'),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sub.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Smaller than legal size ----
# reef ----
ggmod.sub.reef<- ggplot() +
  ylab("")+
  xlab("Reef")+
  geom_point(data=dat.sub,aes(x=broad.reef,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sub.reef,aes(x=broad.reef,y=number),alpha=0.5)+
  geom_line(data=predicts.sub.reef,aes(x=broad.reef,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.reef,aes(x=broad.reef,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sub.reef

# roughness ----
ggmod.sub.rough<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sub,aes(x=roughness,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sub.roughness,aes(x=roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sub.rough

#status
ggmod.sub.status<- ggplot(aes(x=status,y=number,fill=status,colour=status), data=predicts.sub.status) +
  ylab("")+
  xlab('Status')+
  scale_fill_manual(labels = c("No-take","Fished"),values=c("#7bbc63","#b9e6fb"))+
  scale_colour_manual(labels = c("No-take","Fished"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.sub.status$status)))+
  geom_bar(stat = "identity", alpha=0.8)+
  geom_errorbar(aes(ymin = number-se.fit,ymax = number+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "C. auratus",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  ylim(-0.2,4)+
  theme(legend.position = "none")
ggmod.sub.status

# Combine wth patchwork
library(patchwork)
library(cowplot) #for save_plot

# view plots
plot.grid <- ggmod.total.relief+plot_spacer()+plot_spacer()+
             ggmod.sr.mean+ggmod.sr.sd+plot_spacer()+
             ggmod.leg.macro+ggmod.leg.relief+ggmod.leg.tpi+
             ggmod.sub.reef+ggmod.sub.rough+ggmod.sub.status+
             plot_annotation(tag_levels = 'a') + plot_layout(ncol = 3,nrow = 4)
plot.grid

save_plot("plots/original gamms/swc.gam.plots.png", plot.grid,base_height = 9,base_width = 8.5)
