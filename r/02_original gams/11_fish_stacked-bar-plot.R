###
# Project: mac - South-west Corner
# Data:    BOSS & BRUV fish full sample extent
# Task:    Plotting 10 most abundant species w/ cute pics
# author:  Claude
# date:    Nov-Dec 2021
##

# Set directories----
rm(list=ls())

# Study name ----
name <- '2020-2021_south-west_BOSS-BRUV' # for the study

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

## Set your working directory ----
working.dir <- getwd()
setwd(working.dir)
#OR set manually once

theme_collapse<-theme(      
  panel.grid.major=element_line(colour = "white"), 
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

# read in maxn
bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  mutate(method="BRUV")%>%
  glimpse()

boss <- read.csv("data/staging/2020-2021_south-west_BOSS.complete.maxn.csv")%>%
  mutate(method="BOSS")%>%
  glimpse()

maxn <- bind_rows(bruv,boss)

# workout total maxn for each species ---
maxn.10<-maxn%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#load fish pictures
#1 Coris auricularis
c.a <- readPNG("data/images/Coris auricularis-3cmL.png")
c.a <- as.raster(c.a)

#2 Parapricanthus elongatus - put in pempheris
p.e <- readPNG("data/images/Pempheris klunzingeri-3cmL.png")
p.e <- as.raster(p.e)

#3 Neatypus obliquus
n.o <- readPNG("data/images/Neatypus obliquus-3cmL.png")
n.o <- as.raster(n.o)

#4 Centroberyx sp1 (lineatus/australis) - no pic

#5 Caesioperca spp - no pic

#6 Chromis klunzingeri - no pic

#7 Pseudolabrus biserialis
p.b <- readPNG("data/images/Pseudolabrus biserialis-3cm.png")
p.b <- as.raster(p.b)

#8 Ophthalmolepis lineolatus
o.l <- readPNG("data/images/Opthalmolepis lineolatus-3cm.png")
o.l <- as.raster(o.l)

#9 Nelusetta ayraud
n.a <- readPNG("data/images/Nelusetta ayraudi-3cm.png")
n.a <- as.raster(n.a)

#10 Callanthias australis - no pic


#plot final bar plot
bar.top.10<-ggplot(maxn.10%>%mutate(scientific=str_replace_all(.$scientific,          
  c("Centroberyx sp1"="Centroberyx sp1*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 3700)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.a, xmin=9.75,xmax=10.25,ymin=3300, ymax=3850)+              #1
  annotation_raster(p.e, xmin=8.8,xmax=9.2,ymin=3100, ymax=3400)+                 #2
  annotation_raster(n.o, xmin=7.75, xmax=8.25, ymin=2350, ymax=2800)+             #3
  # annotation_raster(c.t, xmin=6.775,xmax=7.275,ymin=395, ymax=520)+             #4
  # annotation_raster(p.o, xmin=5.75,xmax=6.25,ymin=340, ymax=410)+               #5
  # annotation_raster(c.g, xmin=4.7,xmax=5.3,ymin=310, ymax=420)+                 #6
  annotation_raster(p.b, xmin=3.8,xmax=4.2,ymin=1700, ymax=2200)+                 #7
  annotation_raster(o.l, xmin=2.7,xmax=3.3,ymin=1350, ymax=2100)+                 #8
  annotation_raster(n.a, xmin=1.5,xmax=2.5,ymin=750, ymax=1800)                   #9
  # annotation_raster(a.v, xmin=0.75,xmax=1.25,ymin=220, ymax=320)+                #10
  # ggtitle("10 most abundant species") +
  # theme(plot.title = element_text(hjust = 0))
bar.top.10

#save out plot
ggsave("plots/original gamms/abundant.fish.bar.png",bar.top.10,dpi=600,width=6.0)

