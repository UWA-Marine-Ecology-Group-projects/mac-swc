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

# Manually make the most parsimonious GAM models for each taxa ----
unique(dat$scientific)

# MODEL Targeted abundance (mean.relief+roughness) ----
dat.target <- dat %>% filter(scientific=="targeted.abundance")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+s(roughness,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.target)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.target.relief = testdata%>%data.frame(fits)%>%
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

predicts.target.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Targeted abundance abundance ----
# mean relief ----
ggmod.target.relief<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.target,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Targeted abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.target.relief

# roughness ----
ggmod.target.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.target,aes(x=roughness,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.target.roughness

# MODEL Coris auricularis (depth) ----
dat.coris <- dat %>% filter(scientific=="Labridae Coris auricularis")

mod=gam(maxn~s(depth,k=3,bs='cr')+s(site,bs="re"), family=tw,data=dat.coris)

# predict - mean relief ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.coris.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Coris auricularis ----
# depth ----
ggmod.coris.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.target,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.target.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("*Coris auricularis*") +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(plot.title = ggtext::element_markdown())
ggmod.coris.depth

# MODEL Chromis klunzingeri (broad.sponges+depth+status) ----
dat.chromis <- dat %>% filter(scientific=="Pomacentridae Chromis klunzingeri")

mod=gam(maxn~s(broad.sponges,k=3,bs='cr')+s(depth,k=3,bs='cr')+ status + s(site,bs="re"), family=tw,data=dat.chromis)

# predict - sponges ----
testdata <- expand.grid(broad.sponges=seq(min(dat$broad.sponges),max(dat$broad.sponges),length.out = 20),
                        depth=mean(mod$model$depth),
                        status=c("Fished","No-take"),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.chromis.sponge = testdata%>%data.frame(fits)%>%
  group_by(broad.sponges)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        broad.sponges=mean(mod$model$broad.sponges),
                        status=c("Fished","No-take"),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.chromis.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - status ----
testdata <- expand.grid(depth=mean(mod$model$depth),
                        broad.sponges=mean(mod$model$broad.sponges),
                        status=c("Fished","No-take"),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.chromis.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Chromis klunzingeri ----
# sponge ----
ggmod.chromis.sponge<- ggplot() +
  ylab("")+
  xlab("Sponges")+
  geom_point(data=dat.chromis,aes(x=broad.sponges,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.chromis.sponge,aes(x=broad.sponges,y=maxn),alpha=0.5)+
  geom_line(data=predicts.chromis.sponge,aes(x=broad.sponges,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.chromis.sponge,aes(x=broad.sponges,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("*Chromis klunzingeri*") +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(plot.title = ggtext::element_markdown())
ggmod.chromis.sponge

# depth ----
ggmod.chromis.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.chromis,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.chromis.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.chromis.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.chromis.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.chromis.depth

# status ----
ggmod.chromis.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.chromis.status,show.legend=FALSE) +
  ylab("")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.chromis.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  Theme1+
  theme(legend.position = "none")
ggmod.chromis.status

# MODEL Neatypus obliquus (mean.relief+sd.relief) ----
dat.obliquus <- dat %>% filter(scientific=="Scorpididae Neatypus obliquus")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+s(sd.relief,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.obliquus)

# predict - mean relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.obliquus.mean = testdata%>%data.frame(fits)%>%
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

predicts.obliquus.sd = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Neatypus obliquus ----
# mean relief ----
ggmod.obliquus.mean<- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  geom_point(data=dat.obliquus,aes(x=mean.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.obliquus.mean,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.obliquus.mean,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.obliquus.mean,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("*Neatypus obliquus*") +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(plot.title = ggtext::element_markdown())
ggmod.obliquus.mean

# sd relief ----
ggmod.obliquus.sd<- ggplot() +
  ylab("")+
  xlab("SD relief")+
  geom_point(data=dat.obliquus,aes(x=sd.relief,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.obliquus.sd,aes(x=sd.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.obliquus.sd,aes(x=sd.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.obliquus.sd,aes(x=sd.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.obliquus.sd

# MODEL Chrysophrys auratus (aspect+depth+roughness) ----
dat.snap <- dat %>% filter(scientific=="Sparidae Chrysophrys auratus")

mod=gam(maxn~s(aspect,k=3,bs='cr')+s(depth,k=3,bs='cr')+s(roughness,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.snap)

# predict - aspect ----
testdata <- expand.grid(aspect=seq(min(dat$aspect),max(dat$aspect),length.out = 20),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.snap.aspect = testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.snap.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.snap.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Chrysophrys auratus ----
# aspect ----
ggmod.snap.aspect<- ggplot() +
  ylab("")+
  xlab("Aspect")+
  geom_point(data=dat.snap,aes(x=aspect,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.snap.aspect,aes(x=aspect,y=maxn),alpha=0.5)+
  geom_line(data=predicts.snap.aspect,aes(x=aspect,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.snap.aspect,aes(x=aspect,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("*Chrysophrys auratus*") +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(plot.title = ggtext::element_markdown())
ggmod.snap.aspect

# depth ----
ggmod.snap.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.snap,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.snap.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.snap.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.snap.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.snap.depth

# roughness ----
ggmod.snap.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.snap,aes(x=roughness,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.snap.roughness,aes(x=roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.snap.roughness,aes(x=roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.snap.roughness,aes(x=roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.snap.roughness

# MODEL Total abundance (mean.relief) ----
dat.tot <- dat %>% filter(scientific=="total.abundance")

mod=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(site,bs="re"), family=tw,data=dat.tot)

# predict - aspect ----
testdata <- expand.grid(aspect=seq(min(dat$aspect),max(dat$aspect),length.out = 20),
                        depth=mean(mod$model$depth),
                        roughness=mean(mod$model$roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.snap.aspect = testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# Combine with cowplot
library(cowplot)

# view plots
io.plot<- plot_grid(ggmod.ta.mean.relief.io, NULL, NULL,
                    ggmod.sr.mean.relief.io, ggmod.sr.sd.relief.io, NULL,
                    ggmod.ps.status.io, ggmod.ps.mean.relief.io, ggmod.ps.log.slope.x.status,
                    ggmod.wkw.status.io,ggmod.wkw.distance.to.ramp.x.status.io, ggmod.wkw.log.sponges.x.status.io,
                    ggmod.pj.depth.io,ggmod.pj.log.roughness.io,NULL,
                    ggmod.ol.status.io,ggmod.ol.depth.x.status,ggmod.ol.sd.relief.io,
                    ncol = 3,
                    labels = c('a',' ',' ','b','c',' ','d','e','f','g','h','i','j','k',' ','l','m','n'),align = "vh")
io.plot

save_plot("fishing.hwy.abundance.gam.plots.png", fhwy.plot,base_height = 9,base_width = 8.5)
save_plot("inside.outside.abundance.gam.plots.png", io.plot,base_height = 10.8,base_width = 8.5)
