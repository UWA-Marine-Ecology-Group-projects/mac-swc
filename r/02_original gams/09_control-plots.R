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

#need to make a new dataframe - year, species richness (plus SE), greater than legal (plus SE)
year <- c("2018","2018","2019","2019","2020","2020","2021","2021","2022","2022")
status <- c("Fished","No-take")
dat <- data.frame(year,status)

#data=
spr.sr <- maxn %>%
  dplyr::filter(scientific%in%"species.richness")%>%
  dplyr::group_by(status)%>%
  summarise(species.richness = mean(maxn),species.richness.se=se(maxn))%>%
  dplyr::mutate(year="2020")

spr.l <- length %>%
  dplyr::filter(scientific%in%"greater than legal size")%>%
  dplyr::group_by(status)%>%
  summarise(legal = mean(number),legal.se=se(number))%>%
  dplyr::mutate(year="2020")

dat.cp <- dat %>%
  left_join(spr.sr)%>%
  left_join(spr.l)

# plot year by species richness - plus a line for MPA gazetting time ---
gg.sr <- ggplot(data = dat.cp, aes(x = year, y = species.richness, fill = status))+
  geom_errorbar(data = dat.cp,aes(ymin=species.richness-species.richness.se,ymax= species.richness+species.richness.se), width = 0.2,position=position_dodge(width=0.3))+
  geom_point(shape = 21,size = 2,position=position_dodge(width=0.3),stroke = 1, color = "black")+ 
  theme_classic()+
  scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
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
  geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  Theme1
gg.l

# library(ggpubr)
grid <- gg.sr/gg.l+plot_layout(guides = 'collect')
grid

#save out plot
ggsave("plots/original gamms/control.plot.png",grid,dpi=600,width=6.0)
