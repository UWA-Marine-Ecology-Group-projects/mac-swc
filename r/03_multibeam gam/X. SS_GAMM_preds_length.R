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
# Theme----
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

# Theme for polar plots-
Themepolar <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
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

# Set the study name ----
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- "Z:/SWC-multibeam-GAMMS"

## Set sub directories----
d.dir <- paste(working.dir,"Data/Tidy",sep="/") 
h.dir <- paste(working.dir, "Data/Habitat/BRUV Style annotation/tidy data",sep="/") 
s.dir <- paste(working.dir,"shapefiles",sep="/")
p.dir <- paste(working.dir,"Plots",sep="/")
m.dir <- paste(working.dir,"Model Out GAM", sep="/")

# Bring in and format the data----
setwd(d.dir)
dir()

# Length ----
length <-read.csv(paste(name, 'complete.length.csv',sep=".")) %>%
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

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv("2020_sw_maxn.env-cov.csv") %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::filter(!flowdir=="NA")%>%
  dplyr::select(sample, depth.1, slope, aspect, roughness, tpi, flowdir, SSTmean_SSTARRS,SSTsterr_SSTARRS, SSTtrend_SSTARRS)%>%
  unique()%>%
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
  filter(!tpi=="NA")%>%  # make sure only multibeam data in here
  dplyr::glimpse()


unique(complete.length$scientific)

# transformations for modelling

complete.length <- complete.length %>%
  dplyr::mutate(log.sd.relief = log(sd.relief + 1)) %>%
#  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(sqrt.slope = sqrt(slope))%>%
  filter(!scientific%in%c("Monacanthidae Nelusetta ayraud"))%>%
  glimpse()  

#remove the outlier

outlier <- complete.length %>%
  filter(number>30)%>%
  glimpse()
complete.length <- complete.length %>%
  filter(number<40)%>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----

dat <- complete.length

unique(dat$scientific)
names(dat)

# MODEL all greater than 20 cm (sd.relief +aspect + log.tpi) ----

dat.20 <- dat %>% filter(scientific=="all greater than 20 cm")
colnames(dat.20)

gamm=gam(number~s(log.sd.relief,k=3,bs='cr')+s(aspect,k=5,bs='cc')+ tpi + s(site,bs="re") , family=poisson,data=dat.20)
mod <- gamm
summary(mod)
gam.check(mod)


# predict - sd.relief ----
mod<-gamm
testdata <- expand.grid(log.sd.relief= seq(min(dat.20$log.sd.relief),max(dat.20$log.sd.relief),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        aspect=mean(mod$model$aspect),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.sd.relief = testdata%>%data.frame(fits)%>%
  group_by(log.sd.relief)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.20.sd.relief

# predict - aspect ----

testdata <- expand.grid(aspect= seq(min(dat.20$aspect),max(dat.20$aspect),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        log.sd.relief=mean(mod$model$log.sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.aspect= testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.20.aspect

# predict - log.tpi ----

testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        log.sd.relief=mean(mod$model$log.sd.relief),
                        aspect=mean(mod$model$aspect),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.20.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

predicts.20.tpi 

# PLOTS for all greater than 20 cm ----
# sd relief ----

ggmod.20.sd.relief <- ggplot(aes(x=log.sd.relief,y=number), data=predicts.20.sd.relief) +
  ylab("Abundance")+
  xlab('Log Sd relief')+
  geom_point(data=dat.20,aes(x=log.sd.relief,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.20.sd.relief,aes(x=log.sd.relief,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+ 
  theme_classic()+
  Theme1+
  theme(legend.position = "none")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 20 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")
ggmod.20.sd.relief


# aspect ----

ggmod.20.aspect <- ggplot(aes(x=aspect,y=number), data=predicts.20.aspect) +
  ylab(" ")+
  xlab('Aspect')+
  geom_point(data=dat.20,aes(x=aspect,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.20.aspect,aes(x=aspect,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+ coord_polar()+
  theme_classic()+
  Themepolar+theme(legend.position = "none")
#theme(legend.position = c(0.85, 0.85))
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.20.aspect

# log tpi ----

ggmod.20.tpi<- ggplot(aes(x=tpi,y=number), data=predicts.20.tpi) +
  ylab(" ")+
  xlab('TPI')+
  geom_point(data=dat.20,aes(x=tpi,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.20.tpi,aes(x=tpi,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  theme(legend.position = c(0.85, 0.85))
ggmod.20.tpi

# MODEL Abundance greater than 30 cm (depth + aspect + log roughness) ----
dat.30 <- dat %>% filter(scientific=="all greater than 30 cm")

gamm=gam(number~s(depth,k=2,bs='cr')+s(aspect,k=5,bs='cc')+s(log.roughness,k=2,bs='cr')
         + s(site,bs="re"), family=poisson,data=dat.30)

#plot(gamm)
mod<-gamm
summary(mod)
gam.check(mod)

# predict - depth ----

testdata <-expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                       log.roughness=mean(mod$model$log.roughness),
                       aspect=mean(mod$model$aspect),
                       site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.30.depth

# predict - log.roughness----

testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        depth=mean(mod$model$depth),
                        aspect=mean(mod$model$aspect),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.30.log.roughness 

# predict - aspect ----

testdata <- expand.grid(aspect=seq(min(dat$aspect),max(dat$aspect),length.out = 20),
                        depth=mean(mod$model$depth),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.30.aspect= testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.30.aspect

# PLOTS for Abundance greater than 30 cm ----

# log roughness ----

ggmod.30.log.roughness<- ggplot(aes(x=log.roughness,y=number), data=predicts.30.log.roughness) +
  ylab("Abundance")+
  xlab("Log Roughness")+
  geom_point(data=dat.30,aes(x=log.roughness,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.log.roughness,aes(x=log.roughness,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

  ggmod.30.log.roughness
  
# depth----

ggmod.30.depth<- ggplot(aes(x=depth,y=number), data=predicts.30.depth)+
  ylab(" ")+
  xlab("Depth")+
  geom_point(data=dat.30,aes(x=depth,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.depth,aes(x=depth,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.depth

# aspect ----

ggmod.30.aspect <- ggplot(aes(x=aspect,y=number), data=predicts.30.aspect)+
  ylab(" ")+
  xlab("Aspect")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.30,aes(x=aspect,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.30.aspect,aes(x=aspect,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+ coord_polar()+
  theme_classic()+
  Themepolar#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than 30 cm",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.30.aspect

# MODEL Abundance greater than legal size (aspect + tpi + log roughness) ----

dat.legal <- dat %>% filter(scientific=="greater than legal size")

gamm=gam(number~s(aspect,k=5,bs='cc') +s(tpi,k=3,bs='cr')+s(log.roughness,k=3,bs='cr') + s(site,bs="re"), family=poisson,data=dat.legal)
mod <- gamm
summary(mod)
gam.check(mod)
#plot(gamm)

# predict - aspect ----


testdata <- expand.grid(aspect=seq(min(dat$aspect),max(dat$aspect),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.aspect = testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.legal.aspect

# predict - tpi----

testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.legal.tpi

# predict - log.roughness----

testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.log.roughness= testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.legal.log.roughness

# PLOTS for Abundance greater than legal size ----
# aspect ----

ggmod.legal.aspect<- ggplot(aes(x=aspect,y=number), data=predicts.legal.aspect) +
  ylab(" ")+
  xlab("Aspect")+
  geom_point(data=dat.legal,aes(x=aspect,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.aspect,aes(x=aspect,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+coord_polar()+
  theme_classic()+
  Themepolar+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

ggmod.legal.aspect

# log.tpi ----

ggmod.legal.tpi<- ggplot(aes(x=tpi,y=number), data=predicts.legal.tpi) +
  ylab("Abundance")+
  xlab("TPI")+
  geom_point(data=dat.legal,aes(x=tpi,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.tpi,aes(x=tpi,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  annotate("text", x = -Inf, y=Inf, label = "Legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  Theme1

ggmod.legal.tpi

# log.roughness ----

ggmod.legal.log.roughness<- ggplot(aes(x=log.roughness,y=number), data=predicts.legal.log.roughness) +
  ylab(" ")+
  xlab("Log Roughness")+
  geom_point(data=dat.legal,aes(x=log.roughness,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.log.roughness,aes(x=log.roughness,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1

ggmod.legal.log.roughness

# MODEL smaller than legal size (aspect + broad macro + log roughness) ----

dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

gamm=gam(number~s(aspect, k=5, bs='cc') +s(log.roughness,k=3,bs='cr')+s(broad.macroalgae,k=5,bs='cr')+  s(site,bs="re"), family=poisson,data=dat.sublegal)
mod<-gamm
summary(gamm)
gam.check(gamm)

# predict - aspect ----

testdata <- expand.grid(aspect=seq(min(dat$aspect),max(dat$aspect),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.aspect = testdata%>%data.frame(fits)%>%
  group_by(aspect)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sublegal.aspect

# predict - log.roughness ----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        broad.macroalgae=mean(mod$model$broad.macroalgae),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sublegal.log.roughness 

# predict - broad macroalgae ----

testdata <- expand.grid(broad.macroalgae=seq(min(dat$broad.macroalgae),max(dat$broad.macroalgae),length.out = 20),
                        aspect=mean(mod$model$aspect),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.broad.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(broad.macroalgae)%>% #only change here
  dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sublegal.broad.macroalgae

# PLOTS for smaller than legal size ----

# aspect ----


ggmod.sublegal.aspect <- ggplot(aes(x=aspect,y=number), data=predicts.sublegal.aspect) +
  ylab(" ")+
  xlab("Aspect")+
  geom_point(data=dat.sublegal,aes(x=aspect,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.aspect,aes(x=aspect,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+ coord_polar() +
  theme_classic()+
  Themepolar#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.sublegal.aspect 


# log.roughness ----

ggmod.sublegal.log.roughness <- ggplot(aes(x=log.roughness,y=number), data=predicts.sublegal.log.roughness) +
  ylab('Abundance')+
  xlab('Log Roughness')+
  geom_point(data=dat.sublegal,aes(x=log.roughness,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.log.roughness,aes(x=log.roughness,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  annotate("text", x = -Inf, y=Inf, label = "Sublegal size",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  Theme1#theme(legend.position = "none")+

ggmod.sublegal.log.roughness


# broad macroalgae----

ggmod.sublegal.broad.macroalgae <- ggplot(aes(x=broad.macroalgae,y=number), data=predicts.sublegal.broad.macroalgae) +
  ylab(" ")+
  xlab("Broad Macroalgae")+
  geom_point(data=dat.sublegal,aes(x=broad.macroalgae,y=number), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.broad.macroalgae,aes(x=broad.macroalgae,y=number),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=number-se.fit, ymax=number+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
#annotate("text", x = -Inf, y=Inf, label = "Greater than legal size",vjust = 1, hjust = -.1,size=4,fontface="italic")

ggmod.sublegal.broad.macroalgae

## Combine all together -----
library(cowplot)

lk <- plot_grid(ggmod.20.sd.relief, ggmod.20.aspect, ggmod.20.tpi,
          ggmod.30.log.roughness,ggmod.30.aspect, ggmod.30.depth,
          ggmod.legal.tpi, ggmod.legal.aspect, ggmod.legal.log.roughness,
          ggmod.sublegal.log.roughness, ggmod.sublegal.aspect,ggmod.sublegal.broad.macroalgae,
          ncol = 3,
          labels = c('a','b','c','d','e','f','g','h','i','j','k','l'),align = "vh")
lk


setwd(p.dir)

save_plot("multibeam.length.gam.length.plots.png", lk,base_height = 13,base_width = 9)
