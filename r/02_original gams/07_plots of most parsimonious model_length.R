rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)

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
# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

#bring in and format data
# Length ----
length <-read.csv('complete.length.csv',sep=".") %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

unique(length$sample)

test <- length %>%
  filter(number>0)%>%
  distinct(sample)

total.no.pinkies <- length %>%
  dplyr::filter(species=="auratus") %>%
  filter(number>0)

sum(total.no.pinkies$number) # 196
test <- total.no.pinkies %>%
  filter(length>0)
sum(test$number) # 168 measured

168/196*100 # 85% measured

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv(paste(name, 'bathymetry.derivatives.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHCO1"="FHC01","FHCO2"="FHC02","FHCO3"="FHC03"))) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHCO1"="FHC01","FHCO2"="FHC02","FHCO3"="FHC03"))) %>%
  dplyr::glimpse()

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
  #dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# merge length with list of recreational fished species ----
setwd(d.dir)
dir()

# Had to download and use csv as Nectar can't connect to googlesheets :(
master <- read.csv("australia.life.history.csv") %>%
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

unique(master$fishing.type)

spp.species<-length%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # Checked all of these with rec fish app - all don't have one

# Come back to maybe getting rid of some of these, but for now we continue on
legal <- fished.species %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

pinksnapper.legal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size pink snapper") %>%
  dplyr::glimpse()

pinksnapper.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size pink snapper") %>%
  dplyr::glimpse()

fished.bigger.20cm <- fished.species %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 20 cm") %>%
  dplyr::glimpse()

fished.bigger.30cm <- fished.species %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 30 cm") %>%
  dplyr::glimpse()

all.bigger.20cm <- length %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 20 cm") %>%
  dplyr::glimpse()

all.bigger.30cm <- length %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 30 cm") %>%
  dplyr::glimpse()

# Split metadata into Fishing HWY and In/Out dataframes
summary(metadata)

metadata.fh <- metadata %>%
  dplyr::filter(depth<50)

metadata.io <- metadata %>%
  dplyr::filter(latitude<=(-33.96))

plot(metadata$longitude, metadata$latitude)
plot(metadata.fh$longitude, metadata.fh$latitude)
plot(metadata.io$longitude, metadata.io$latitude)

## Combine all the maxn data to be modeled into a single data frame
combined.length <- bind_rows(legal, sublegal, all.bigger.20cm, all.bigger.30cm) 

unique(combined.length$scientific)

complete.length <- combined.length %>%
  #dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(campaignid,sample,scientific,number) %>%
  tidyr::complete(nesting(campaignid,sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

unique(complete.length$scientific)

length.fh <- complete.length %>%
  semi_join(., metadata.fh) %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 12)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

length.io <- complete.length %>%
  semi_join(., metadata.io) %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 12)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))


unique(length.io$scientific)

# Manually make the most parsimonious GAM models for each taxa ----
#### FISHING HWY ####
dat <- length.fh

unique(dat$scientific)
names(dat)

# MODEL all greater than 20 cm (broad.reef.by.status+sd.relief.by.status+status) ----
dat.20 <- dat %>% filter(scientific=="all greater than 20 cm")

gamm=gam(number~s(broad.reef,k=3,bs='cr', by=status)+s(sd.relief,k=3,bs='cr', by=status)+ s(site,bs="re") + status, family=poisson,data=dat.20)

#plot(gamm)

# predict - status ----
mod<-gamm
testdata <- expand.grid(broad.reef=mean(mod$model$broad.reef),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - broad.reef.by.status ----
mod<-gamm
testdata <- expand.grid(broad.reef=seq(min(dat$broad.reef),max(dat$broad.reef),length.out = 20),
                        sd.relief=mean(mod$model$sd.relief),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.broad.reef.x.status = testdata%>%data.frame(fits)%>%
  group_by(broad.reef,status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - sd.relief.by.status ----
mod<-gamm
testdata <- expand.grid(sd.relief=seq(min(dat$sd.relief),max(dat$sd.relief),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.sd.relief.x.status = testdata%>%data.frame(fits)%>%
  group_by(sd.relief,status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for all greater than 20 cm ----
# status ----
ggmod.20.status<- ggplot(aes(x=status,y=number,fill=status,colour=status), data=predicts.20.status,show.legend=FALSE) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.20.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = number-se.fit,ymax = number+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 20 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.20.status


# broad.reef.by.status ----
ggmod.20.broad.reef.x.status<- ggplot(aes(x=broad.reef,y=number,colour=status), data=dat.20) +
  ylab(" ")+
  xlab('% Reef')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.20.broad.reef.x.status,show.legend=TRUE)+
  geom_line(data=predicts.20.broad.reef.x.status,aes(y=number - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.20.broad.reef.x.status,aes(y=number + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+theme(legend.position = "none")
  #theme(legend.position = c(0.85, 0.85))
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.20.broad.reef.x.status

# sd.relief.by.status ----
ggmod.20.sd.relief.x.status<- ggplot(aes(x=sd.relief,y=number,colour=status), data=dat.20) +
  ylab(" ")+
  xlab('SD relief')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.20.sd.relief.x.status,show.legend=TRUE)+
  geom_line(data=predicts.20.sd.relief.x.status,aes(y=number - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.20.sd.relief.x.status,aes(y=number + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+
  theme(legend.position = c(0.85, 0.85))
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.20.sd.relief.x.status

# MODEL Abundance greater than 30 cm (log Roughness + log TPI + Mean relief) ----
dat.30 <- dat %>% filter(scientific=="all greater than 30 cm")

gamm=gam(number~s(log.roughness,k=3,bs='cr')+s(mean.relief,k=3,bs='cr')+s(log.tpi,k=3,bs='cr')
         + s(site,bs="re"), family=poisson,data=dat.30)

#plot(gamm)

# predict - mean.relief ----
mod<-gamm

testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        log.tpi=mean(mod$model$log.tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.roughness ----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        log.tpi=mean(mod$model$log.tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.tpi ----
mod<-gamm
testdata <- expand.grid(log.tpi=seq(min(dat$log.tpi),max(dat$log.tpi),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.log.tpi = testdata%>%data.frame(fits)%>%
  group_by(log.tpi)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Abundance greater than 30 cm ----
# mean relief ----
ggmod.30.mean.relief<- ggplot() +
  ylab("Abundance")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=mean.relief,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.mean.relief,aes(x=mean.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.30.mean.relief,aes(x=mean.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.30.mean.relief,aes(x=mean.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.mean.relief

# log.roughness ----
ggmod.30.log.roughness<- ggplot() +
  ylab(" ")+
  xlab("log Roughness")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=log.roughness,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.log.roughness,aes(x=log.roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.30.log.roughness,aes(x=log.roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.30.log.roughness,aes(x=log.roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  
  Theme1#+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  #annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.log.roughness

# log.tpi ----
ggmod.30.log.tpi <- ggplot() +
  ylab(" ")+
  xlab("log TPI")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=log.tpi,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.log.tpi,aes(x=log.tpi,y=number),alpha=0.5)+
  geom_line(data=predicts.30.log.tpi,aes(x=log.tpi,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.30.log.tpi,aes(x=log.tpi,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.log.tpi

# MODEL Abundance greater than legal size (broad.macroalgae + depth) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

gamm=gam(number~s(broad.macroalgae,k=3,bs='cr')+s(depth,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.legal)

#plot(gamm)

# predict - broad.macroalgae ----
mod<-gamm

testdata <- expand.grid(broad.macroalgae=seq(min(dat$broad.macroalgae),max(dat$broad.macroalgae),length.out = 20),
                        depth=mean(mod$model$depth),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.broad.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
mod<-gamm

testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Abundance greater than legal size ----
# broad.macroalgae ----
ggmod.legal.broad.macroalgae<- ggplot() +
  ylab("Abundance")+
  xlab("% Macroalgae")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.legal,aes(x=broad.macroalgae,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.broad.macroalgae,aes(x=broad.macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.broad.macroalgae,aes(x=broad.macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.broad.macroalgae,aes(x=broad.macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.legal.broad.macroalgae

# depth ----
ggmod.legal.depth<- ggplot() +
  ylab(" ")+
  xlab("Depth (m)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.legal,aes(x=depth,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=depth,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1

ggmod.legal.depth

# MODEL smaller than legal size (broad.macroalgae+log.slope.by.status+status) ----
dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

gamm=gam(number~s(broad.macroalgae,k=3,bs='cr')+s(log.slope,k=3,bs='cr', by=status)+ s(site,bs="re") + status, family=poisson,data=dat.sublegal)

# predict - status ----
mod<-gamm
testdata <- expand.grid(broad.macroalgae=mean(mod$model$broad.macroalgae),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope.by.status ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.log.slope.x.status = testdata%>%data.frame(fits)%>%
  group_by(log.slope,status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - broad.macroalgae ----
mod<-gamm

testdata <- expand.grid(broad.macroalgae=seq(min(dat$broad.macroalgae),max(dat$broad.macroalgae),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.broad.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for smaller than legal size ----
# status ----
ggmod.sublegal.status<- ggplot(aes(x=status,y=number,fill=status,colour=status), data=predicts.sublegal.status,show.legend=FALSE) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.sublegal.status$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = number-se.fit,ymax = number+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Sub-legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.sublegal.status


# log.slope.by.status ----
ggmod.sublegal.log.slope.x.status<- ggplot(aes(x=log.slope,y=number,colour=status), data=dat.sublegal) +
  ylab(" ")+
  xlab('log Slope')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.sublegal.log.slope.x.status,show.legend=TRUE)+
  geom_line(data=predicts.sublegal.log.slope.x.status,aes(y=number - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.sublegal.log.slope.x.status,aes(y=number + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+#theme(legend.position = "none")+
  theme(legend.position = c(0.85, 0.85))
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.sublegal.log.slope.x.status

ggmod.sublegal.broad.macroalgae<- ggplot() +
  ylab(" ")+
  xlab("% Macroalgae")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sublegal,aes(x=broad.macroalgae,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.broad.macroalgae,aes(x=broad.macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.broad.macroalgae,aes(x=broad.macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.broad.macroalgae,aes(x=broad.macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  #annotate("text", x = -Inf, y=Inf, label = "Greater than legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.sublegal.broad.macroalgae

## Combine all together -----
plot_grid(ggmod.20.status, ggmod.20.sd.relief.x.status, ggmod.20.broad.reef.x.status,
          ggmod.30.log.roughness, ggmod.30.log.tpi, ggmod.30.mean.relief,
          ggmod.legal.broad.macroalgae, ggmod.legal.depth, NULL,
          ggmod.sublegal.status, ggmod.sublegal.broad.macroalgae, ggmod.sublegal.log.slope.x.status,
          ncol = 3,
          labels = c('a','b','c','d','e','f','g','h','','i','j','k'),align = "h")

#### INSIDE/Outside ####
dat <- length.fh

unique(dat$scientific)
names(dat)

# MODEL all greater than 20 cm (broad.reef + log.roughness) ----
dat.20 <- dat %>% filter(scientific=="all greater than 20 cm")

gamm=gam(number~s(broad.reef,k=3,bs='cr')+s(log.roughness,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.20)

#plot(gamm)

# predict - broad.reef ----
mod<-gamm
testdata <- expand.grid(broad.reef=seq(min(dat$broad.reef),max(dat$broad.reef),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.broad.reef.io = testdata%>%data.frame(fits)%>%
  group_by(broad.reef)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.roughness ----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        broad.reef=mean(mod$model$broad.reef),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.log.roughness.io = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for all greater than 20 cm Inside outside ----

# broad.reef  ----
ggmod.20.broad.reef.io <- ggplot() +
  ylab("Abundance")+
  xlab("% Reef")+
  geom_point(data=dat.20,aes(x=broad.reef,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.20.broad.reef.io,aes(x=broad.reef,y=number),alpha=0.5)+
  geom_line(data=predicts.20.broad.reef.io,aes(x=broad.reef,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.20.broad.reef.io,aes(x=broad.reef,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 20 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  Theme1

ggmod.20.broad.reef.io

# log.roughness ----
ggmod.20.log.roughness.io <- ggplot() +
  ylab(" ")+
  xlab("log Roughness")+
  geom_point(data=dat.20,aes(x=log.roughness,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.20.log.roughness.io,aes(x=log.roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.20.log.roughness.io,aes(x=log.roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.20.log.roughness.io,aes(x=log.roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1

ggmod.20.log.roughness.io


# MODEL Abundance greater than 30 cm (Distance to ramp + log Slope) ----
dat.30 <- dat %>% filter(scientific=="all greater than 30 cm")

gamm=gam(number~s(log.slope,k=3,bs='cr')+s(distance.to.ramp,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.30)

# predict - distance.to.ramp ----
mod<-gamm

testdata <- expand.grid(distance.to.ramp=seq(min(dat$distance.to.ramp),max(dat$distance.to.ramp),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.distance.to.ramp.io = testdata%>%data.frame(fits)%>%
  group_by(distance.to.ramp)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        distance.to.ramp=mean(mod$model$distance.to.ramp),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.log.slope.io = testdata%>%data.frame(fits)%>%
  group_by(log.slope)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Abundance greater than 30 cm ----
# mean relief ----
ggmod.30.distance.to.ramp.io<- ggplot() +
  ylab("Abundance")+
  xlab("Distance to ramp (km)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=distance.to.ramp,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.distance.to.ramp.io,aes(x=distance.to.ramp,y=number),alpha=0.5)+
  geom_line(data=predicts.30.distance.to.ramp.io,aes(x=distance.to.ramp,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.30.distance.to.ramp.io,aes(x=distance.to.ramp,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.distance.to.ramp.io

# log.slope ----
ggmod.30.log.slope.io <- ggplot() +
  ylab(" ")+
  xlab("log Slope")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=log.slope,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.log.slope.io,aes(x=log.slope,y=number),alpha=0.5)+
  geom_line(data=predicts.30.log.slope.io,aes(x=log.slope,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.30.log.slope.io,aes(x=log.slope,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.log.slope.io

# MODEL Abundance greater than legal size (log Slope by Status + log % Sponges + Status) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

gamm=gam(number~s(log.slope,k=3,bs='cr', by=status)+s(log.sponges,k=3,bs='cr')+ s(site,bs="re") + status, family=poisson,data=dat.legal)

# predict - status ----
mod<-gamm
testdata <- expand.grid(log.sponges=mean(mod$model$log.sponges),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.status.io = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.slope.by.status ----
mod<-gamm
testdata <- expand.grid(log.slope=seq(min(dat$log.slope),max(dat$log.slope),length.out = 20),
                        log.sponges=mean(mod$model$log.sponges),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.log.slope.x.status.io = testdata%>%data.frame(fits)%>%
  dplyr::group_by(log.slope,status)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.sponges ----
mod<-gamm

testdata <- expand.grid(log.sponges=seq(min(dat$log.sponges),max(dat$log.sponges),length.out = 20),
                        log.slope=mean(mod$model$log.slope),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.log.sponges.io = testdata%>%data.frame(fits)%>%
  group_by(log.sponges)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for greater than legal size ----
# status ----
ggmod.legal.status.io<- ggplot(aes(x=status,y=number,fill=status,colour=status), data=predicts.legal.status.io,show.legend=FALSE) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.legal.status.io$status)))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = number-se.fit,ymax = number+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.legal.status.io

# log.slope.by.status ----
ggmod.legal.log.slope.x.status.io<- ggplot(aes(x=log.slope,y=number,colour=status), data=dat.legal) +
  ylab(" ")+
  xlab('log Slope')+
  scale_color_manual(labels = c("Fished", "No-take"),values=c("grey", "#1470ad"))+
  geom_jitter(width = 0.25,height = 0,alpha=0.75, size=1,show.legend=TRUE)+
  geom_line(data=predicts.legal.log.slope.x.status.io,show.legend=TRUE)+
  geom_line(data=predicts.legal.log.slope.x.status.io,aes(y=number - se.fit),linetype="dashed",show.legend=FALSE)+
  geom_line(data=predicts.legal.log.slope.x.status.io,aes(y=number + se.fit),linetype="dashed",show.legend=FALSE)+
  theme_classic()+
  Theme1+#theme(legend.position = "none")+
  theme(legend.position = c(0.85, 0.85))
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.legal.log.slope.x.status.io

# log sponges ----
ggmod.legal.log.sponges.io <- ggplot() +
  ylab(" ")+
  xlab("log % Sponges")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.legal,aes(x=log.sponges,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.log.sponges.io,aes(x=log.sponges,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.log.sponges.io,aes(x=log.sponges,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.log.sponges.io,aes(x=log.sponges,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1

ggmod.legal.log.sponges.io

# MODEL smaller than legal size (log.roughness+log.tpi+mean.relief) ----
dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

gamm=gam(number~s(log.roughness,k=3,bs='cr')+s(log.tpi,k=3,bs='cr')+s(mean.relief,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.sublegal)

# predict - log.roughness ----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        log.tpi=mean(mod$model$log.tpi),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.log.roughness.io = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - log.tpi ----
mod<-gamm
testdata <- expand.grid(log.tpi=seq(min(dat$log.tpi),max(dat$log.tpi),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.log.tpi.io = testdata%>%data.frame(fits)%>%
  group_by(log.tpi)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        log.tpi=mean(mod$model$log.tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.mean.relief.io = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for smaller than legal size ----
# mean relief ----
ggmod.sublegal.mean.relief.io<- ggplot() +
  ylab(" ")+
  xlab("Mean relief")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sublegal,aes(x=mean.relief,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.mean.relief.io,aes(x=mean.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.mean.relief.io,aes(x=mean.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.mean.relief.io,aes(x=mean.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1

ggmod.sublegal.mean.relief.io

# log.tpi ----
ggmod.sublegal.log.tpi.io <- ggplot() +
  ylab(" ")+
  xlab("log TPI")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sublegal,aes(x=log.tpi,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.log.tpi.io,aes(x=log.tpi,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.log.tpi.io,aes(x=log.tpi,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.log.tpi.io,aes(x=log.tpi,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1

ggmod.sublegal.log.tpi.io

# log.roughness ----
ggmod.sublegal.log.roughness.io <- ggplot() +
  ylab("Abundance")+
  xlab("log Roughness")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sublegal,aes(x=log.roughness,y=number),  alpha=0.75, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.log.roughness.io,aes(x=log.roughness,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.log.roughness.io,aes(x=log.roughness,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.log.roughness.io,aes(x=log.roughness,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Sub-legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.sublegal.log.roughness.io


# FINAL PLOTS -----
# FISHING HWY 
fhwy.plot <- plot_grid(ggmod.20.status, ggmod.20.sd.relief.x.status, ggmod.20.broad.reef.x.status,
          ggmod.30.mean.relief,ggmod.30.log.roughness, ggmod.30.log.tpi, 
          ggmod.legal.broad.macroalgae, ggmod.legal.depth, NULL,
          ggmod.sublegal.status, ggmod.sublegal.broad.macroalgae, ggmod.sublegal.log.slope.x.status,
          ncol = 3,
          labels = c('a','b','c','d','e','f','g','h','','i','j','k'),align = "vh")

# INSIDE OUTSIDE
io.plot <- plot_grid(ggmod.20.broad.reef.io, ggmod.20.log.roughness.io,NULL,
          ggmod.30.distance.to.ramp.io,ggmod.30.log.slope.io,NULL,
          ggmod.legal.status.io,ggmod.legal.log.slope.x.status.io,ggmod.legal.log.sponges.io,
          ggmod.sublegal.log.roughness.io,ggmod.sublegal.log.tpi.io,ggmod.sublegal.mean.relief.io,
          ncol = 3,
          labels = c('a','b','','c','d','','e','f','g','h','i','j','k'),align = "vh")

1.8*4

setwd(working.dir)
save_plot("fishing.hwy.abundance.gam.length.plots.png", fhwy.plot,base_height = 7.2,base_width = 8)
save_plot("inside.outside.abundance.gam.length.plots.png", io.plot,base_height = 7.2,base_width = 8)
