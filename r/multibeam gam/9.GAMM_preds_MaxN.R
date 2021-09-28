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

# Bathymetry derivatives ----   Em change here 
bathy <- read.csv("2020_sw_maxn.env-cov.csv") %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::filter(!flowdir=="NA")%>%
  dplyr::select(sample, depth.1, slope, aspect, roughness, tpi, flowdir, SSTmean_SSTARRS,SSTsterr_SSTARRS, SSTtrend_SSTARRS)%>%
  unique()%>%
  dplyr::glimpse()


# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

test<-left_join(metadata,ramps)

ggplot() + geom_point(data=test,aes(y=longitude,x=latitude, col=distance.to.ramp),  alpha=0.75, size=1,show.legend=TRUE)

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



## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(species.maxn, 
                           ta.sr)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  filter(!tpi=="NA")%>%  # removes all drops with no multibeam data
  distinct()%>%
  glimpse()

# transformations for modelling ----

maxn.fh <- combined.maxn %>%
  dplyr::mutate(log.sd.relief = log(sd.relief + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(sqrt.slope = sqrt(slope))%>%
  filter(!scientific%in%c("Monacanthidae Nelusetta ayraud"))%>%
  glimpse()  

# remove outlier 

plot(maxn.fh$maxn)

maxn.fh <- maxn.fh %>%
  filter(maxn<600)%>%
  glimpse()


# Manually make the most parsimonious GAM models for each taxa ----
#### FISHING HWY ####
dat <- maxn.fh

unique(dat$scientific)

# MODEL Total abundance (log.tpi + aspect + mean relief) ----
dat.ta <- dat %>% filter(scientific=="total.abundance")
#head(dat.ta)
colnames(dat.ta)

gamm=gam(maxn~s(tpi,k=3,bs='cr') + s(mean.relief, k=3, bs='cr') + s(aspect, k=5,bs='cc') + s(site,bs='re'), family=poisson,data=dat.ta)

# predict - aspect ----
mod<-gamm
summary(mod)
gam.check(mod)

testdata <- expand.grid(aspect= seq(min(dat.ta$aspect),max(dat.ta$aspect),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.aspect = testdata%>%data.frame(fits)%>%
  dplyr::group_by(aspect)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.ta.aspect

# predict - tpi ----

testdata <- expand.grid(tpi= seq(min(dat.ta$tpi),max(dat.ta$tpi),length.out = 20),
                                    mean.relief=mean(mod$model$mean.relief),
                                    aspect=mean(mod$model$aspect),
                                    site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.ta.tpi 

# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief= seq(min(dat.ta$mean.relief),max(dat.ta$mean.relief),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        aspect=mean(mod$model$aspect),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ta.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()


# PLOTS for Total abundance ----
# aspect ----

summary(predicts.ta.aspect$aspect)

ggmod.ta.aspect<- ggplot(aes(x=aspect,y=maxn), data=predicts.ta.aspect,show.legend=FALSE) +
  ylab("Abundance")+
  xlab('Aspect')+
  geom_point(data=dat.ta,aes(x=aspect,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ta.aspect,aes(x=aspect,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+ coord_polar()+
  theme_classic()+
#  ylim(0,100)+
  scale_x_continuous(breaks=seq(0, 6.3, 1))+
  Themepolar+
  theme(legend.position = "none")+
  annotate("text", x = -Inf, y=Inf, label = "Total abundance",vjust = -1, hjust = 0.8,size=4,fontface="italic") # change position
ggmod.ta.aspect


# tpi  ----

ggmod.ta.tpi <- ggplot(aes(x=tpi,y=maxn), data=predicts.ta.tpi) +
  ylab(" ")+
  xlab('TPI')+
  geom_point(data=dat.ta,aes(x=tpi,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ta.tpi,aes(x=tpi,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  #ylim(0,200)+
  Theme1
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ta.tpi

# mean.relief ----

ggmod.ta.mean.relief<- ggplot(aes(x=mean.relief,y=maxn), data=predicts.ta.mean.relief) +
  ylab(" ")+
  xlab('Mean relief')+
  geom_point(data=dat.ta,aes(x=mean.relief,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ta.mean.relief,aes(x=mean.relief,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
 # ylim(0,250)+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.ta.mean.relief

# MODEL Species richness (mean.relief+ sqrt.slope) ----  ## Em pick up here Mon red colour: #ee6c4d
summary(dat$scientific)

dat.sr <- dat %>% filter(scientific=="species.richness")

gamm=gam(maxn~s(mean.relief,k=3,bs='cr')+s(sqrt.slope,k=3,bs='cr')+ s(site,bs="re"), family=poisson,data=dat.sr)
mod <- gamm
summary(mod)
gam.check(mod)


# predict - mean.relief ----
mod<-gamm
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20),
                        sqrt.slope=mean(mod$model$sqrt.slope),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.mean.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sr.mean.relief

# predict - sqrt.slope ----

testdata <- expand.grid(sqrt.slope=seq(min(dat$sqrt.slope),max(dat$sqrt.slope),length.out = 20),
                        mean.relief=mean(mod$model$mean.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sr.sqrt.slope = testdata%>%data.frame(fits)%>%
  group_by(sqrt.slope)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sr.sqrt.slope 

# PLOTS for Species richness ----
# mean relief ----
ggmod.sr.mean.relief<- ggplot(aes(x=mean.relief,y=maxn), data=predicts.sr.mean.relief) +
  ylab("Number of \nspecies")+
  xlab("Mean relief")+
  geom_point(data=dat.sr,aes(x=mean.relief,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.mean.relief,aes(x=mean.relief,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon( aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = "Species richness",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.mean.relief

# sqrt.slope ---- 
ggmod.sr.sqrt.slope <- ggplot(aes(x=sqrt.slope,y=maxn), data=predicts.sr.sqrt.slope) +
  ylab(" ")+
  xlab("Sqrt slope")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("grey", "#1470ad"))+
  geom_point(data=dat.sr,aes(x=sqrt.slope,y=maxn),   colour="#3d5a80", alpha=0.15,size=1,show.legend=FALSE)+
  geom_line(data=predicts.sr.sqrt.slope,aes(x=sqrt.slope,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon( aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=4)+
  annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=4,fontface="italic")#+
#geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
ggmod.sr.sqrt.slope

# MODEL Pink snapper (log.roughness + status) ----

dat.ps <- dat %>% filter(scientific=="Sparidae Chrysophrys auratus")

gamm=gam(maxn~s(log.roughness,k=3,bs='cr') + s(site,bs="re") + status, family=poisson,data=dat.ps)
mod <- gamm
summary(mod)
gam.check(mod)

# predict - status ----

testdata <- expand.grid(log.roughness=mean(mod$model$log.roughnes),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.status = testdata%>%data.frame(fits)%>%
  group_by(status)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.ps.status

# predict - log.roughness ----

testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        site=(mod$model$site),
                        status = c("Fished","No-take"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.ps.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.ps.log.roughness 

# PLOTS for Pink snapper ----
# status ----
ggmod.ps.status<- ggplot(aes(x=status,y=maxn,fill=status,colour=status), data=predicts.ps.status) +
  ylab("Abundance")+
  xlab('Status')+
  scale_fill_manual(labels = c("Fished", "No-take"),values=c("#3d5a80", "#e07a5f"))+
  scale_colour_manual(labels = c("Fished", "No-take"),values=c("black", "black"))+
  scale_x_discrete(limits = rev(levels(predicts.ps.status$status)))+
  geom_bar(stat = "identity", alpha=0.8)+
  geom_errorbar(aes(ymin = maxn-se.fit,ymax = maxn+se.fit),width = 0.5) +
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "C. auratus",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  ylim(-0.2,4)+
  theme(legend.position = "none")
ggmod.ps.status

# log roughness ----

ggmod.ps.log.roughness<- ggplot(aes(x=log.roughness,y=maxn), data=predicts.ps.log.roughness) +
  ylab(" ")+
  xlab('Log roughness')+
  geom_point(data=dat.ps,aes(x=log.roughness,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.ps.log.roughness,aes(x=log.roughness,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  ylim(0,6)+
  theme_classic()+
  Theme1
 # theme(legend.position = c(0.85, 0.85))
ggmod.ps.log.roughness



# MODEL Western King Wrasse (depth +  tpi + sqrt slope) ----
dat.wkw <- dat %>% filter(scientific=="Labridae Coris auricularis")

plot(dat.wkw$maxn)
plot(dat.wkw$log.roughness)


gamm=gam(maxn~s(depth,k=3,bs='cr')+tpi+ sqrt.slope + s(site,bs="re"), family=poisson,data=dat.wkw)

# predict - depth----

mod <- gamm
summary(mod)
gam.check(mod)

testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        tpi=mean(mod$model$tpi),
                        sqrt.slope=mean(mod$model$sqrt.slope),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.depth= testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.wkw.depth

# predict - sqrt slope ----

testdata <- expand.grid(sqrt.slope=seq(min(dat$sqrt.slope),max(dat$sqrt.slope),length.out = 20),
                        depth=mean(mod$model$depth),
                        tpi=mean(mod$model$tpi),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.sqrt.slope = testdata%>%data.frame(fits)%>%
  group_by(sqrt.slope)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.wkw.sqrt.slope

# predict - log tpi ----

testdata <- expand.grid(tpi=seq(min(dat$tpi),max(dat$tpi),length.out = 20),
                        depth=mean(mod$model$depth),
                        sqrt.slope=mean(mod$model$sqrt.slope),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.wkw.tpi = testdata%>%data.frame(fits)%>%
  group_by(tpi)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.wkw.tpi

# PLOTs for Western King Wrasse ----
# distance to ramp ----
ggmod.wkw.depth <- ggplot(aes(x=depth,y=maxn), data=predicts.wkw.depth) +
  ylab("Abundance")+
  xlab('Depth')+
  geom_point(data=dat.wkw,aes(x=depth,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.wkw.depth,aes(x=depth,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  annotate("text", x = -Inf, y=Inf, label = "C. auricularis",vjust = 1, hjust = -.1,size=4,fontface="italic")+
  #ylim(-5,220)+
  theme(legend.position = "none")
ggmod.wkw.depth

# tpi ----

ggmod.wkw.tpi<- ggplot(aes(x=tpi,y=maxn), data=predicts.wkw.tpi) +
  ylab(" ")+
  xlab('TPI')+
  geom_point(data=dat.wkw,aes(x=tpi,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.wkw.tpi,aes(x=tpi,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
 # ylim(0,100)+
  theme_classic()+
  Theme1#+
#annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=4)
ggmod.wkw.tpi

# sqrt.slope ----
ggmod.wkw.sqrt.slope<- ggplot(aes(x=sqrt.slope,y=maxn), data=predicts.wkw.sqrt.slope) +
  ylab(" ")+
  xlab('Sqrt Slope')+
  geom_point(data=dat.wkw,aes(x=sqrt.slope,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.wkw.sqrt.slope,aes(x=sqrt.slope,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
 # ylim(0,100)+
  theme_classic()+
  Theme1

ggmod.wkw.sqrt.slope


# MODEL Port Jackson Shark (depth + log roughness + log sd relief) ----  start here 

dat.pj <- dat %>% filter(scientific=="Heterodontidae Heterodontus portusjacksoni")

gamm=gam(maxn~ depth + s(log.roughness,k=3,bs='cr')+ log.sd.relief + s(site,bs="re"), family=poisson,data=dat.pj)
mod <- gamm
summary(mod)

# predict - depth ----
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        log.roughness=mean(mod$model$log.roughness),
                        log.sd.relief=mean(mod$model$log.sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.pj.depth

# predict - log.roughness----
mod<-gamm
testdata <- expand.grid(log.roughness=seq(min(dat$log.roughness),max(dat$log.roughness),length.out = 20),
                        depth=mean(mod$model$depth),
                        log.sd.relief=mean(mod$model$log.sd.relief),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.log.roughness = testdata%>%data.frame(fits)%>%
  group_by(log.roughness)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.pj.log.roughness

# predict - log.sd.relief----

testdata <- expand.grid(log.sd.relief=seq(min(dat$log.sd.relief),max(dat$log.sd.relief),length.out = 20),
                        depth=mean(mod$model$depth),
                        log.roughness=mean(mod$model$log.roughness),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.pj.log.sd.relief = testdata%>%data.frame(fits)%>%
  group_by(log.sd.relief)%>% #only change here
  dplyr::summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.pj.log.sd.relief

# Plots for Port Jackson Shark ----
# depth ----

ggmod.pj.depth<- ggplot(aes(x=depth,y=maxn), data=predicts.pj.depth) +
  ylab("Abundance")+
  xlab("Depth")+
  geom_point(data=dat.pj,aes(x=depth,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.depth,aes(x=depth,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  annotate("text", x = -Inf, y=Inf, label = "H. portusjacksoni",vjust = 1, hjust = -.1,size=4,fontface="italic")#+

ggmod.pj.depth

# log roughness ---- 

ggmod.pj.log.roughness <- ggplot(aes(x=log.roughness, y=maxn), data=predicts.pj.log.roughness) +
  ylab("")+
  xlab("Log roughness")+
  geom_point(data=dat.pj,aes(x=log.roughness,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.log.roughness,aes(x=log.roughness,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1#+

ggmod.pj.log.roughness

# log sd.relief ----

predicts.pj.sd.relief

ggmod.pj.sd.relief <- ggplot(aes(x=log.sd.relief, y=maxn), data=predicts.pj.log.sd.relief) +
  ylab("")+
  xlab("Log sd relief")+
  geom_point(data=dat.pj,aes(x=log.sd.relief,y=maxn), fill="#3d5a80", colour="#3d5a80", alpha=0.15, size=1,show.legend=FALSE)+
  geom_line(data=predicts.pj.log.sd.relief,aes(x=log.sd.relief,y=maxn),colour="#3d5a80", alpha=0.5)+
  geom_ribbon(aes(ymin=maxn-se.fit, ymax=maxn+se.fit), fill="#3d5a80", alpha=0.3)+
  theme_classic()+
  Theme1#+

ggmod.pj.sd.relief



# Combine with cowplot ----
library(cowplot)

# view plots
lk <- plot_grid(ggmod.ta.aspect, ggmod.ta.tpi, ggmod.ta.mean.relief,
          ggmod.sr.mean.relief, ggmod.sr.sqrt.slope, NULL,
          ggmod.ps.status, ggmod.ps.log.roughness, NULL,
          ggmod.wkw.depth, ggmod.wkw.tpi, ggmod.wkw.sqrt.slope,
          ggmod.pj.depth, ggmod.pj.log.roughness,ggmod.pj.sd.relief,
          ncol = 3, labels = c('a','b','c','d','e',' ','f','g', ' ', 'h','i','j','k','l','m'),align = "v")
lk

setwd(p.dir)
save_plot("abundance.gamm.plot.png", lk, base_height = 13.5,base_width = 9)
