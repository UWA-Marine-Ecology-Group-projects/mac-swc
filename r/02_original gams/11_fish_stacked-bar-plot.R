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
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(plyr)
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

#plot deep fish
#load deep fish
#1 Chromis fumea
c.f <- readPNG("data/images/Pomacentridae-Dark.png")
c.f <- as.raster(c.f)

#2 Pentapodus porosus
p.p <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.p <- as.raster(p.p)

#3 Pomacentrus coelestis
p.c <- readPNG("data/images/Pomacentrus coelestis-3cmL.png")
p.c <- as.raster(p.c)

#4 Cirrhilabrus temminckii
c.t <- readPNG("data/images/Labridae-Dark.png")
c.t <- as.raster(c.t)

#5 Pristotis obtusirostris
p.o <- readPNG("data/images/Pomacentridae-Dark.png")
p.o <- as.raster(p.o)

#6 Carangoides gymnostethus 
c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png")
c.g <- as.raster(c.g)

#7 Lethrinus ravus
l.r <- readPNG("data/images/Lethrinidae-Dark.png")
l.r <- as.raster(l.r)

#8 Leptojulis cyanopleura
l.c <- readPNG("data/images/Labridae-Dark.png")
l.c <- as.raster(l.c)

#9 Nemipterus spp
n.spp <- readPNG("data/images/Nemipterus_bathybius_nb_GIBBONS.png")
n.spp <- as.raster(n.spp)

#10 Alepes vari
a.v <- readPNG("data/images/Pseudocaranx dentex-3cm.png")
a.v <- as.raster(a.v)

#plot final bar plot
bar.deep.top.10<-ggplot(maxn.deep.10%>%mutate(scientific=str_replace_all(.$scientific,          
  c("gymnostethus"="gymnostethus*","ravus"="ravus*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 600)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.f, xmin=9.75,xmax=10.25,ymin=530, ymax=600)+              #1
  annotation_raster(p.p, xmin=8.8,xmax=9.2,ymin=450, ymax=540)+                 #2
  annotation_raster(p.c, xmin=7.75, xmax=8.25, ymin=410, ymax=480)+             #3
  annotation_raster(c.t, xmin=6.775,xmax=7.275,ymin=395, ymax=520)+             #4
  annotation_raster(p.o, xmin=5.75,xmax=6.25,ymin=340, ymax=410)+               #5
  annotation_raster(c.g, xmin=4.7,xmax=5.3,ymin=310, ymax=420)+                 #6
  annotation_raster(l.r, xmin=3.7,xmax=4.3,ymin=275, ymax=390)+                 #7
  annotation_raster(l.c, xmin=2.775,xmax=3.275,ymin=230, ymax=355)+             #8
  annotation_raster(n.spp, xmin=1.8,xmax=2.2,ymin=230, ymax=320)+               #9
  annotation_raster(a.v, xmin=0.75,xmax=1.25,ymin=220, ymax=320)+               #10
  ggtitle("Deep assemblage (20-65m)") +
  theme(plot.title = element_text(hjust = 0))
bar.deep.top.10

#save out plot
ggsave("plots/stacked.bar.plot.deep.png",bar.deep.top.10,dpi=600,width=6.0)

