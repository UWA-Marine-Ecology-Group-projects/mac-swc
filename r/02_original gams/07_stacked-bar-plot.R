###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Plotting 10 most abundant species w/ cute pics
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

# Load fish pictures for plotting ----
#anampses geographicus
a.g <- readPNG("data/images/Anampses_geographicus_nb_TAYLOR.png")
a.g <- as.raster(a.g)

#chaetodon assarius
c.ass <- readPNG("data/images/Chaetodon assarius-3cmL.png")
c.ass <- as.raster(c.ass)

#choerodon rubescens
c.r <- readPNG("data/images/Choerodon rubescens 3cm.png")
c.r <- as.raster(c.r)

#chromis westaustralis
c.w <- readPNG("data/images/Chromis westaustralis-3cmL.png")
c.w <- as.raster(c.w)

#coris auricularis
c.a <- readPNG("data/images/Coris auricularis-3cmL.png")
c.a <- as.raster(c.a)

#suzeicthys cyanolaemus
s.c <- readPNG("data/images/Labridae-Dark.png")
s.c <- as.raster(s.c)

#lethrinus miniatus
l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png")
l.m <- as.raster(l.m)

#neatypus obliquus
n.o <- readPNG("data/images/Neatypus obliquus-3cmL.png")
n.o <- as.raster(n.o)

#parupeneus spilurus
p.s <- readPNG("data/images/Parupeneus_spilurus_nb_TAYLOR.png")
p.s <- as.raster(p.s)

#chrysophrys auratus
c.au <- readPNG("data/images/Chrysophrys auratus 3cm.png")
c.au <- as.raster(c.au)

#pentapous nagasakiensis
p.n <- readPNG("data/images/Pentapodus porosus-3cmL.png")
p.n <- as.raster(p.n)

#lethrinus nebulosus
l.n <- readPNG("data/images/lethrinus nebulosus 3cm.png")
l.n <- as.raster(l.n)

#seriola dumerili
s.d <- readPNG("data/images/seriola_dumerili_nb.png")
s.d <- as.raster(s.d)

#pristipomoides multidens
p.m <- readPNG("data/images/Pristipomoides multidens 3cm.png")
p.m <- as.raster(p.m)

#pseudocaranx spp
p.spp <- readPNG("data/images/Pseudocaranx dentex-3cm.png")
p.spp <- as.raster(p.spp)

#gymnothorax woodwardi
#aint no pic for this one

# #pseudanthias
# p.spp <- readPNG("data/images/Pseudanthias rubrizonatus.png")
# p.spp <- as.raster(p.spp)

# read in maxn
maxn.boss <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  glimpse()

maxn.bruv <- read.csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  glimpse()

maxn <- bind_rows(maxn.boss,maxn.bruv)

#NPZ6

# workout total maxn for each species ---
maxn.npz6.10<-maxn%>%
  dplyr::filter(location%in%"NPZ6")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  glimpse()

test1<-maxn%>%
  dplyr::filter(location%in%"NPZ6"&status%in%"Fished")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  glimpse()

test2<-maxn%>%
  dplyr::filter(location%in%"NPZ6"&status%in%"No-take")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  glimpse()

## Total frequency of occurance 
# I think we could remove this section - but maybe good to see sometimes
bar.npz6<-ggplot(maxn.npz6.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.npz6

## Top ten plot ----
bar.npz6.top.10<-ggplot(maxn.npz6.10%>%mutate(scientific=str_replace_all(.$scientific,
  c("miniatus"="miniatus*","auratus"="auratus*","rubescens"="rubescens*","nebulosus"="nebulosus*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 1140)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.w, xmin=9.7,xmax=10.2,ymin=1080, ymax=1190)+
  annotation_raster(c.a, xmin=8.65,xmax=9.35,ymin=785, ymax=995)+
  annotation_raster(l.m, xmin=7.5, xmax=8.5, ymin=100, ymax=330)+
  annotation_raster(c.au, xmin=6.5,xmax=7.5,ymin=90, ymax=340)+
  annotation_raster(p.s, xmin=5.7,xmax=6.3,ymin=60, ymax=210)+
  annotation_raster(c.r, xmin=4.6,xmax=5.4,ymin=55, ymax=265)+
  annotation_raster(s.c, xmin=3.8,xmax=4.2,ymin=45, ymax=160)+
  annotation_raster(n.o, xmin=2.75,xmax=3.25,ymin=45, ymax=170)+
  annotation_raster(p.n, xmin=1.72,xmax=2.25,ymin=35, ymax=180)+
  annotation_raster(l.n, xmin=0.5,xmax=1.5,ymin=35, ymax=265)
bar.npz6.top.10

#save out plot
ggsave("plots/stacked.bar.plot.npz6.png",bar.npz6.top.10,dpi=600,width=6.0)

#NPZ9

# workout total maxn for each species ---
maxn.npz9.10<-maxn%>%
  dplyr::filter(location%in%"NPZ9")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(11)%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  glimpse()

## Total frequency of occurance 
# I think we could remove this section - but maybe good to see sometimes
bar.npz9<-ggplot(maxn.npz9.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.npz9

## Top ten plot ----
bar.npz9.top.10<-ggplot(maxn.npz9.10%>%mutate(scientific=str_replace_all(.$scientific,
  c("miniatus"="miniatus*","auratus"="auratus*","dumerili"="dumerili*","multidens"="multidens*","Pseudocaranx spp"="Pseudocaranx spp*"))), 
  aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 175)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.ass, xmin=9.75,xmax=10.25,ymin=168, ymax=183)+
  annotation_raster(c.w, xmin=8.75,xmax=9.25,ymin=138, ymax=155)+
  annotation_raster(l.m, xmin=7.5, xmax=8.5, ymin=75, ymax=110)+
  annotation_raster(c.au, xmin=6.5,xmax=7.5,ymin=73, ymax=110)+
  annotation_raster(s.d, xmin=5.5,xmax=6.5,ymin=30, ymax=80)+
  annotation_raster(p.m, xmin=4.5,xmax=5.5,ymin=29, ymax=75)+          
  annotation_raster(c.a, xmin=3.65,xmax=4.25,ymin=20, ymax=50)+
  annotation_raster(p.spp, xmin=2.65,xmax=3.35,ymin=14, ymax=40)+
  annotation_raster(p.s, xmin=1.6,xmax=2.4,ymin=13, ymax=38)
bar.npz9.top.10

#save out plot
ggsave("plots/stacked.bar.plot.npz9.png",bar.npz9.top.10,dpi=600,width=6.0)
