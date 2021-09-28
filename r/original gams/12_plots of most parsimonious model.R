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
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

## Set sub directories----
d.dir <- paste(working.dir,"Data/Tidy",sep="/") 
h.dir <- paste(working.dir, "Data/Habitat/BRUV Style annotation/tidy data",sep="/") 
s.dir <- paste(working.dir,"shapefiles",sep="/")
p.dir <- paste(working.dir,"Plots",sep="/")
m.dir <- paste(working.dir,"Model Out GAM", sep="/")

# Bring in and format the data----
setwd(d.dir)
dir()

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


# Bring in and format the raw data----

# Load the dataset -
# MaxN ----
maxn <-read.csv(paste(name, 'complete.maxn.csv',sep=".")) %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv(paste(name, 'bathymetry.derivatives.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

test<-left_join(metadata.io,ramps)

ggplot() + geom_point(data=maxn.fh,aes(x=longitude,y=latitude, col=distance.to.ramp),  alpha=0.75, size=1,show.legend=TRUE)

# Habitat ----
habitat.2020.10 <- read.csv("2020-10_south-west_stereo-BRUVs_BRUV_style.broad.habitat.csv") %>%
  dplyr::select(-c(rowid.x,rowid.y)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.10)

habitat.2020.06 <- read.csv("2020-06._broad.habitat_BRUV_Style.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.06) # 0-100

habitat <-bind_rows(habitat.2020.06, habitat.2020.10) %>%
  tidyr::replace_na(list(broad.Consolidated=0,
                         broad.Macroalgae=0,
                         broad.Seagrasses=0,
                         broad.Sponges=0,
                         broad.Unconsolidated=0,
                         broad.Bryozoa=0,
                         broad.Hydroids=0,
                         broad.Octocoral.Black=0,
                         broad.Stony.corals=0,
                         fov.Facing.Up=0)) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + broad.seagrasses + broad.sponges + broad.stony.corals) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(campaignid,sample,everything()) %>% # re-ordering hab columns 
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample,total.abundance,species.richness) %>%
  tidyr::gather(.,"scientific","maxn",2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
setwd(d.dir)
dir()

# Select species of interest to model ----
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",
                                  "Labridae Coris auricularis",
                                  "Heterodontidae Heterodontus portusjacksoni",
                                  "Monacanthidae Nelusetta ayraud"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()

# Split metadata into Fishing HWY and In/Out dataframes
summary(metadata)

metadata.fh <- metadata %>%
  dplyr::filter(depth<50)

metadata.io <- metadata %>%
  dplyr::filter(latitude<=(-33.96))

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(species.maxn, 
                           ta.sr)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  distinct()

maxn.fh <- combined.maxn %>%
  semi_join(., metadata.fh) %>%
  filter(!scientific%in%c("Monacanthidae Nelusetta ayraud"))

maxn.io <- combined.maxn %>%
  semi_join(., metadata.io)

maxn.fh <- maxn.fh %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

maxn.io <- maxn.io %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

# Manually make the most parsimonious GAM models for each taxa ----
#### FISHING HWY ####
dat <- maxn.fh

unique(dat$scientific)

# MODEL Total abundance (log.tpi.by.status+mean.relief.by.status+status) ----
dat.ta <- dat %>% filter(scientific=="total.abundance")

gamm=gam(maxn~s(log.tpi,k=3,bs='cr', by=status)+s(mean.relief,k=3,bs='cr', by=status)+ s(site,bs="re") + status, family=poisson,data=dat.ta)

# predict - status ----
mod<-gamm
testdata <- expand.grid(mean.relief=mean(mod$model$mean.relief),
                        log.tpi=mean(mod$model$log.tpi),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.tpi.by.status ----
mod<-gamm
testdata <- expand.grid(log.tpi=seq(min(dat$log.tpi),max(dat$log.tpi),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.log.tpi.x.status = testdata%>%data.frame(fits)%>%
  group_by(log.tpi,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean.relief.by.status ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        log.tpi=mean(mod$model$log.tpi),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.mean.relief.x.status = testdata%>%data.frame(fits)%>%
  group_by(mean.relief,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for Total abundance ----
# status ----
ggmod.ta.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.ta.status,show.legend=FALSE) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.ta.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  Theme1+
  theme(legend.position = "none")+
  annotate("text", x = -Inf, y=Inf, label = "Total abundance",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.ta.status

# "Fished" = red, "No-take" = "#1470ad"

# log.tpi.by.status ----
ggmod.ta.log.tpi.x.status<- ggplot(aes(x=log.tpi,y=maxn,colour=status), data=dat.ta) +
  ylab(" ")+
  xlab('Log TPI')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.ta.log.tpi.x.status,show.legend=TRUE)+
  geom_line(data=predicts.ta.log.tpi.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.ta.log.tpi.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  theme(legend.position = c(0.85, 0.85))
  #annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ta.log.tpi.x.status

# mean.relief.by.status ----
ggmod.ta.mean.relief.x.status<- ggplot(aes(x=mean.relief,y=maxn,colour=status), data=dat.ta) +
  ylab(" ")+
  xlab('Mean relief')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ta.mean.relief.x.status,show.legend=FALSE)+
  geom_line(data=predicts.ta.mean.relief.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.ta.mean.relief.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ta.mean.relief.x.status

# MODEL Species richness (mean.relief+sd.relief) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

gamm=gam(maxn~s(mean.relief,k=3,bs='cr')+s(sd.relief,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.sr)

# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd.relief ----
mod<-gamm
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.sd.relief = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# mean relief ----
ggmod.sr.mean.relief<- ggplot() +
  ylab("Number of \nspecies")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sr,aes(x=mean.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
  #geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.mean.relief

# sd.relief ----
ggmod.sr.sd.relief<- ggplot() +
  ylab(" ")+
  xlab("SD relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sr,aes(x=sd.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.sd.relief

# MODEL Pink snapper (broad.macroalgae.by.status+log.slope+status) ----
dat.ps <- dat %>% filter(scientific=="Sparidae Chrysophrys auratus")

gamm=gam(maxn~s(broad.macroalgae,k=3,bs='cr', by=status)+s(log.slope,k=3,bs='cr')+ s(site,bs="re") + status, family=poisson,data=dat.ps)

# predict - status ----
mod<-gamm
testdata <- expand.grid(broad.macroalgae=mean(mod$model$broad.macroalgae),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - broad.macroalgae.by.status ----
mod<-gamm
testdata <- expand.grid(broad.macroalgae=seq(min(dat$broad.macroalgae),max(dat$broad.macroalgae),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.broad.macroalgae.x.status = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.log.slope = testdata%>%data.frame(fits)%>%
  group_by(log.slope)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Pink snapper ----
# status ----
ggmod.ps.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.ps.status) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.ps.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "C. auratus",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.position = "none")
ggmod.ps.status

# broad.macroalgae.by.status ----
ggmod.ps.broad.macroalgae.x.status<- ggplot(aes(x=broad.macroalgae,y=maxn,colour=status), data=dat.ps) +
  ylab(" ")+
  xlab('% Macroalgae')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.ps.broad.macroalgae.x.status,show.legend=TRUE)+
  geom_line(data=predicts.ps.broad.macroalgae.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.ps.broad.macroalgae.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  theme(legend.position = c(0.85, 0.85))
ggmod.ps.broad.macroalgae.x.status

# log.slope ----
ggmod.ps.log.slope<- ggplot() +
  ylab("")+
  xlab("Log Slope")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.ps,aes(x=log.slope,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ps.log.slope,aes(x=log.slope,y=maxn),alpha=0.5)+
  geom_line(data=predicts.ps.log.slope,aes(x=log.slope,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.ps.log.slope,aes(x=log.slope,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  #annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.ps.log.slope


# MODEL Western King Wrasse (depth.by.status+sd.relief.by.status+status) ----
dat.wkw <- dat %>% filter(scientific=="Labridae Coris auricularis")

gamm=gam(maxn~s(depth,k=3,bs='cr', by=status)+s(sd.relief,k=3,bs='cr', by=status)+ s(site,bs="re") + status, family=poisson,data=dat.wkw)

# predict - status ----
mod<-gamm
testdata <- expand.grid(sd.relief=mean(mod$model$sd.relief),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth.by.status ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.depth.by.status = testdata%>%data.frame(fits)%>%
  group_by(depth,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd.relief.by.status ----
mod<-gamm
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.sd.relief.x.status = testdata%>%data.frame(fits)%>%
  group_by(sd.relief,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTs for Western King Wrasse ----
# status ----
ggmod.wkw.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.wkw.status) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.wkw.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "C. auricularis",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.position = "none")
ggmod.wkw.status

# depth.by.status ----
ggmod.wkw.depth.x.status<- ggplot(aes(x=depth,y=maxn,colour=status), data=dat.wkw) +
  ylab(" ")+
  xlab('Depth (m)')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.wkw.depth.by.status,show.legend=FALSE)+
  geom_line(data=predicts.wkw.depth.by.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.wkw.depth.by.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.wkw.depth.x.status

# sd.relief.by.status ----
ggmod.wkw.sd.relief.x.status<- ggplot(aes(x=sd.relief,y=maxn,colour=status), data=dat.wkw) +
  ylab(" ")+
  xlab('SD relief')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.wkw.sd.relief.x.status,show.legend=TRUE)+
  geom_line(data=predicts.wkw.sd.relief.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.wkw.sd.relief.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  theme(legend.position = c(0.85, 0.85))
ggmod.wkw.sd.relief.x.status

# MODEL Port Jackson Shark (log.slope+mean.relief) ----
dat.pj <- dat %>% filter(scientific=="Heterodontidae Heterodontus portusjacksoni")

gamm=gam(maxn~s(log.slope,k=3,bs='cr')+s(mean.relief,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.pj)

# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.log.slope = testdata%>%data.frame(fits)%>%
  group_by(log.slope)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# Plots for Port Jackson Shark ----
# log.slope ----
ggmod.pj.log.slope<- ggplot() +
  ylab("Abundance")+
  xlab("Log Slope")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.pj,aes(x=log.slope,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.log.slope,aes(x=log.slope,y=maxn),alpha=0.5)+
  geom_line(data=predicts.pj.log.slope,aes(x=log.slope,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.pj.log.slope,aes(x=log.slope,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "H. portusjacksoni",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+

#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.pj.log.slope

# mean.relief ---- 
ggmod.pj.mean.relief <- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.pj,aes(x=mean.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.pj.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.pj.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.pj.mean.relief


# INSIDE/OUTSIDE -----
dat <- maxn.io

unique(dat$scientific)

# MODEL Total abundance (mean.relief) ----
dat.ta <- dat %>% filter(scientific=="total.abundance")

gamm=gam(maxn~s(mean.relief,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.ta)

# predict - mean.relief  ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for Total abundance ----
ggmod.ta.mean.relief.io <- ggplot() +
  ylab("Abundance")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.ta,aes(x=mean.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ta.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.ta.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.ta.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "Total abundance",vjust = 1, hjust = -.1,size=4,fontface="italic")#+#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.ta.mean.relief.io

# MODEL Species richness (mean.relief + sd.relief) ----
dat.sr <- dat %>% filter(scientific=="species.richness")

gamm=gam(maxn~s(mean.relief,k=3,bs='cr')+s(sd.relief,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.sr)

# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd.relief ----
mod<-gamm
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.sd.relief = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# mean relief ----
ggmod.sr.mean.relief.io<- ggplot() +
  ylab("Number of \nspecies")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sr,aes(x=mean.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.mean.relief.io

# sd.relief ----
ggmod.sr.sd.relief.io<- ggplot() +
  ylab(" ")+
  xlab("SD relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sr,aes(x=sd.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sr.sd.relief,aes(x=sd.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.sd.relief.io

# MODEL Pink snapper (log.slope.by.status+mean.relief+status) ----
dat.ps <- dat %>% filter(scientific=="Sparidae Chrysophrys auratus")

gamm=gam(maxn~s(log.slope,k=3,bs='cr', by=status)+s(mean.relief,k=3,bs='cr')+ s(site,bs="re") + status, family=poisson,data=dat.ps)

# predict - status ----
mod<-gamm
testdata <- expand.grid(mean.relief=mean(mod$model$mean.relief),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope.by.status ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.log.slope.x.status = testdata%>%data.frame(fits)%>%
  group_by(log.slope,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Pink snapper ----
# status ----
ggmod.ps.status.io <- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.ps.status) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.ps.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "C. auratus",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.ps.status.io

# log.slope.by.status ----
ggmod.ps.log.slope.x.status<- ggplot(aes(x=log.slope,y=maxn,colour=status), data=dat.ps) +
  ylab(" ")+
  xlab('log Slope')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  #geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ps.log.slope.x.status,show.legend=FALSE)+
  geom_line(data=predicts.ps.log.slope.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.ps.log.slope.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  theme(legend.position = c(0.85, 0.85))+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ps.log.slope.x.status

# mean.relief ----
ggmod.ps.mean.relief.io <- ggplot() +
  ylab("")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.ps,aes(x=mean.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ps.mean.relief,aes(x=mean.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.ps.mean.relief,aes(x=mean.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.ps.mean.relief,aes(x=mean.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.ps.mean.relief.io


# MODEL Western King Wrasse (distance.to.ramp.by.status+log.sponges.by.status+status) ----
dat.wkw <- dat %>% filter(scientific=="Labridae Coris auricularis")

gamm=gam(maxn~s(distance.to.ramp,k=3,bs='cr', by=status)+s(log.sponges,k=3,bs='cr', by=status)+ s(site,bs="re") + status, family = poisson,data=dat.wkw)

plot(gamm)

# predict - status ----
mod<-gamm
testdata <- expand.grid(distance.to.ramp=mean(mod$model$distance.to.ramp),
                        log.sponges=mean(mod$model$log.sponges),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.status.io = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - distance.to.ramp.by.status ----
mod<-gamm
testdata <- expand.grid(distance.to.ramp=seq(min(dat$distance.to.ramp),max(dat$distance.to.ramp),length.out = 20),
                        log.sponges=mean(mod$model$log.sponges),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.distance.to.ramp.by.status.io = testdata%>%data.frame(fits)%>%
  group_by(distance.to.ramp,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.sponges.by.status ----
mod<-gamm
testdata <- expand.grid(log.sponges=seq(min(dat$log.sponges),max(dat$log.sponges),length.out = 20),
                        distance.to.ramp=mean(mod$model$distance.to.ramp),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.log.sponges.x.status.io = testdata%>%data.frame(fits)%>%
  group_by(log.sponges,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTs for Western King Wrasse ----
# status ----
ggmod.wkw.status.io <- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.wkw.status.io) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.wkw.status.io$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "C. auricularis",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.position = "none")
ggmod.wkw.status.io

# log.sponges.by.status ----
ggmod.wkw.log.sponges.x.status.io <- ggplot(aes(x=log.sponges,y=maxn,colour=status), data=dat.wkw) +
  ylab(" ")+
  xlab('Log % Sponges')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.wkw.log.sponges.x.status.io,show.legend=TRUE)+
  geom_line(data=predicts.wkw.log.sponges.x.status.io,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.wkw.log.sponges.x.status.io,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  theme(legend.position = c(0.85, 0.85))+
  Theme1

ggmod.wkw.log.sponges.x.status.io

# distance.to.ramp.by.status ----
ggmod.wkw.distance.to.ramp.x.status.io <- ggplot(aes(x=distance.to.ramp,y=maxn,colour=status), data=dat.wkw) +
  ylab(" ")+
  xlab('Distance to ramp (km)')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.wkw.distance.to.ramp.by.status.io,show.legend=TRUE)+
  geom_line(data=predicts.wkw.distance.to.ramp.by.status.io,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.wkw.distance.to.ramp.by.status.io,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1#+
  #theme(legend.position = c(0.85, 0.85))

ggmod.wkw.distance.to.ramp.x.status.io

# MODEL Port Jackson Shark (depth+log.roughness) ----
dat.pj <- dat %>% filter(scientific=="Heterodontidae Heterodontus portusjacksoni")

gamm=gam(maxn~s(depth,k=3,bs='cr')+s(log.roughness,k=3,bs='cr')+s(site,bs="re"), family=poisson,data=dat.pj)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.roughness ----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# Plots for Port Jackson Shark ----
# depth ----
ggmod.pj.depth.io<- ggplot() +
  ylab("Abundance")+
  xlab("Depth (m)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.pj,aes(x=depth,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.pj.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.pj.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "H. portusjacksoni",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.pj.depth.io

# log.roughness ---- 
ggmod.pj.log.roughness.io <- ggplot() +
  ylab("")+
  xlab("Log roughness")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.pj,aes(x=log.roughness,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.log.roughness,aes(x=log.roughness,y=maxn),alpha=0.5)+
  geom_line(data=predicts.pj.log.roughness,aes(x=log.roughness,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.pj.log.roughness,aes(x=log.roughness,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.pj.log.roughness.io



# MODEL Ocean leatherjacket (Depth by Status + SD relief + Status) ----
dat.ol <- dat %>% filter(scientific=="Monacanthidae Nelusetta ayraud")

gamm=gam(maxn~s(depth,k=3,bs='cr', by=status)+s(sd.relief,k=3,bs='cr')+ s(site,bs="re") + status, family=poisson,data=dat.ol)

# predict - status ----
mod<-gamm
testdata <- expand.grid(sd.relief=mean(mod$model$sd.relief),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ol.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth.by.status ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ol.depth.x.status = testdata%>%data.frame(fits)%>%
  group_by(depth,status)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd.relief ----
mod<-gamm
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ol.sd.relief = testdata%>%data.frame(fits)%>%
  group_by(sd.relief)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Ocean leatherjackets ----
# status ----
ggmod.ol.status.io <- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.ol.status) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.ps.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "N. ayraud",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.ol.status.io

# depth.by.status ----
ggmod.ol.depth.x.status<- ggplot(aes(x=depth,y=maxn,colour=status), data=dat.ol) +
  ylab(" ")+
  xlab('Depth (m)')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ol.depth.x.status,show.legend=FALSE)+
  geom_line(data=predicts.ol.depth.x.status,aes(y=maxn - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.ol.depth.x.status,aes(y=maxn + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ol.depth.x.status

# sd.relief ----
ggmod.ol.sd.relief.io <- ggplot() +
  ylab("")+
  xlab("SD relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.ol,aes(x=sd.relief,y=maxn),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ol.sd.relief,aes(x=sd.relief,y=maxn),alpha=0.5)+
  geom_line(data=predicts.ol.sd.relief,aes(x=sd.relief,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.ol.sd.relief,aes(x=sd.relief,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.ol.sd.relief.io




# Combine with cowplot
library(cowplot)

# view plots
plot_grid(ggmod.ta.status, ggmod.ta.mean.relief.x.status, ggmod.ta.log.tpi.x.status,
          ggmod.sr.mean.relief, ggmod.sr.sd.relief, NULL,
          ggmod.ps.status, ggmod.ps.broad.macroalgae.x.status, ggmod.ps.log.slope,
          ggmod.wkw.status, ggmod.wkw.depth.x.status, ggmod.wkw.sd.relief.x.status,
          ggmod.pj.log.slope, ggmod.pj.mean.relief,NULL,
          ncol = 3, labels = c('a','b','c','d','e',' ','f','g','h','i','j','k','l','m',' '),align = "vh")

fhwy.plot <- plot_grid(ggmod.ta.status, ggmod.ta.mean.relief.x.status, ggmod.ta.log.tpi.x.status,
                      ggmod.sr.mean.relief, ggmod.sr.sd.relief, NULL,
                      ggmod.ps.status, ggmod.ps.broad.macroalgae.x.status, ggmod.ps.log.slope,
                      ggmod.wkw.status, ggmod.wkw.depth.x.status, ggmod.wkw.sd.relief.x.status,
                      ggmod.pj.log.slope, ggmod.pj.mean.relief,NULL,
                      ncol = 3, labels = c('a','b','c','d','e',' ','f','g','h','i','j','k','l','m',' '),align = "vh")


plot_grid(ggmod.ta.mean.relief.io, NULL, NULL,
          ggmod.sr.mean.relief.io, ggmod.sr.sd.relief.io, NULL,
          ggmod.ps.status.io, ggmod.ps.mean.relief.io, ggmod.ps.log.slope.x.status,
          ggmod.wkw.status.io,ggmod.wkw.distance.to.ramp.x.status.io, ggmod.wkw.log.sponges.x.status.io,
          ggmod.pj.depth.io,ggmod.pj.log.roughness.io,NULL,
          ggmod.ol.status.io,ggmod.ol.depth.x.status,ggmod.ol.sd.relief.io,
          ncol = 3,
          labels = c('a',' ',' ','b','c',' ','d','e','f','g','h','i','j','k',' ','l','m','n'),align = "vh")

io.plot<- plot_grid(ggmod.ta.mean.relief.io, NULL, NULL,
                    ggmod.sr.mean.relief.io, ggmod.sr.sd.relief.io, NULL,
                    ggmod.ps.status.io, ggmod.ps.mean.relief.io, ggmod.ps.log.slope.x.status,
                    ggmod.wkw.status.io,ggmod.wkw.distance.to.ramp.x.status.io, ggmod.wkw.log.sponges.x.status.io,
                    ggmod.pj.depth.io,ggmod.pj.log.roughness.io,NULL,
                    ggmod.ol.status.io,ggmod.ol.depth.x.status,ggmod.ol.sd.relief.io,
                    ncol = 3,
                    labels = c('a',' ',' ','b','c',' ','d','e','f','g','h','i','j','k',' ','l','m','n'),align = "vh")


setwd(working.dir)

9/5
1.8*6
save_plot("fishing.hwy.abundance.gam.plots.png", fhwy.plot,base_height = 9,base_width = 8.5)
save_plot("inside.outside.abundance.gam.plots.png", io.plot,base_height = 10.8,base_width = 8.5)
