###
# Project: mac - South-west Corner
# Data:    BRUV fish, habitat
# Task:    Control plots for in and out of sanctuary zone
# author:  Claude
# date:    Nov-Dec 2021
##

# Set directories----
rm(list=ls())

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)
library(patchwork)

#standard error
se <- function(x) sd(x)/sqrt(length(x))

#load theme
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    #legend.title = element_blank(),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once
# read in maxn
maxn <- readRDS("data/tidy/dat.maxn.full.rds")%>%
  glimpse()

length <- readRDS("data/tidy/dat.length.full.rds")%>%
  glimpse()

bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  glimpse()

boss <- read.csv("data/staging/2020-2021_south-west_BOSS.complete.maxn.csv")%>%
  glimpse()

full.maxn <- bind_rows(bruv, boss)%>%
  dplyr::filter(maxn>0,!is.na(status)) #fix this properly

#read in SST
sst <- readRDS("data/spatial/oceanography/SwC_SST_winter.rds")%>%
  ungroup()%>%
  dplyr::mutate(year=as.numeric(year))%>%
  glimpse()

locations <-  read.csv("data/spatial/oceanography/network_scale_boundaries.csv", 
                       header = TRUE) %>%
  glimpse()

sst <- sst %>%
  dplyr::filter(Lat <= -33.479 & Lat >= -34.618,Lon <= 115.723 & Lon >= 114.353)%>% #work out how to automate
  dplyr::group_by(year)%>%
  dplyr::summarise(sst.mean=mean(sst,na.rm = T), sd = mean(sd, na.rm = T))%>%
  glimpse()

# get rls thermal niche values ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,rls.thermal.niche)%>%
  distinct()%>%
  glimpse()

cti <- full.maxn %>%
  left_join(master)%>%
  dplyr::filter(!is.na(rls.thermal.niche))%>%
  dplyr::mutate(log.maxn=log1p(maxn),weightedSTI=log.maxn*rls.thermal.niche)%>%
  dplyr::group_by(id,sample,status)%>%
  dplyr::summarise(log.maxn=sum(log.maxn),w.STI = sum(weightedSTI),CTI=w.STI/log.maxn)%>%
  dplyr::ungroup()%>%
  glimpse()

#need to make a new dataframe - year, species richness (plus SE), greater than legal (plus SE)
year <- c("2017","2017","2018","2018","2019","2019","2020","2020","2021","2021","2022","2022")
status <- c("Fished","No-take")
dat <- data.frame(year,status)
dat$year <- as.numeric(dat$year)

#data=
spr.sr <- maxn %>%
  dplyr::filter(scientific%in%"species.richness")%>%
  dplyr::group_by(status)%>%
  summarise(species.richness = mean(maxn),species.richness.se=se(maxn))%>%
  dplyr::mutate(year=as.numeric("2020"))%>%
  glimpse()

spr.l <- length %>%
  dplyr::filter(scientific%in%"greater than legal size")%>%
  dplyr::group_by(status)%>%
  summarise(legal = mean(number),legal.se=se(number))%>%
  dplyr::mutate(year=as.numeric("2020"))%>%
  glimpse()

spr.cti <- cti %>%
  dplyr::group_by(status)%>%
  summarise(cti = mean(CTI),cti.se=se(CTI))%>%
  dplyr::mutate(year=as.numeric("2020"))%>%
  glimpse()

dat.cp <- dat %>%
  left_join(spr.sr)%>%
  left_join(spr.l)%>%
  left_join(spr.cti)%>%
  left_join(sst)%>%
  dplyr::filter(!year=="2022")%>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg.sr <- ggplot(data = dat.cp, aes(x = year, y = species.richness, fill = status))+
  geom_errorbar(data = dat.cp,aes(ymin=species.richness-species.richness.se,ymax= species.richness+species.richness.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2,position=position_dodge(width=0.3),stroke = 1, color = "black")+ 
  theme_classic()+
  scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Species richness")+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.sr

#greater than legal
gg.l <- ggplot(data = dat.cp, aes(x = year, y = legal, fill = status))+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1.5),fill = "#ffeec7")+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 1.5, ymax = 2),fill = "#c7d6ff")+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2, ymax = Inf),fill = "#caffc7")+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.25),fill = "#ffc7c7")+
  geom_errorbar(data = dat.cp,aes(ymin=legal-legal.se,ymax= legal+legal.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.3),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,5))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.l

#CTI
gg.cti <- ggplot()+ 
  geom_line(data = dat.cp,aes(group = 1, x = year, y = sst.mean))+
  geom_ribbon(data = dat.cp,aes(group = 1, x = year, y = sst.mean, 
                              ymin = sst.mean - sd, ymax = sst.mean+sd), 
              alpha = 0.2)+
  geom_errorbar(data = dat.cp,aes(x = year, y = cti,ymin=cti-cti.se,
                                ymax= cti+cti.se, fill = status), 
                width = 0.2, position = position_dodge(width = 0.1))+
  geom_point(data = dat.cp, aes(x = year, y = cti, fill = status),shape = 21,size = 2,
             stroke = 1, color = "black", position = position_dodge(width = 0.1))+
  theme_classic()+
  scale_y_continuous(limits = c(17,20))+
  scale_x_continuous(limits = c(2017,2021.5))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", 
             size=0.5,alpha = 0.5)+
  ylab(expression(paste("Temperature (",degree~C,")")))+
  xlab("Year")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),
                    values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.cti

grid <- gg.sr/gg.l/gg.cti+plot_layout(guides = 'collect')
grid

#save out plot
save_plot("plots/original gamms/control.plot.png",grid,base_height = 6,base_width = 7.5)
