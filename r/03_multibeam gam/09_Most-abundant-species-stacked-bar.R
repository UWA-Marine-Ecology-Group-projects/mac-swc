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

multibeam <- read.csv("data/tidy/2020-2021_south-west_BOSS-BRUV.multibeam-derivatives.csv")%>%
  dplyr::mutate(id=paste(campaignid, sample, sep = "."))%>%
  select(id, multibeam_derivatives_tpi)%>%
  glimpse()

maxn <- bind_rows(bruv,boss) %>%
  dplyr::mutate(id=paste(campaignid, sample, sep = "."))%>%
  dplyr::select(-depth)%>%
  left_join(multibeam)%>%
  dplyr::filter(!is.na(multibeam_derivatives_tpi))%>%                           #remove non-multibeam samples
  glimpse()

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
#1 Parapricanthus elongatus - no pic, use pempheris multiradiata
p.e <- readPNG("data/images/Pempheris_multiradiata_nb_TAYLOR.png")
p.e <- as.raster(p.e)

#2 Coris auricularis
c.a <- readPNG("data/images/Coris auricularis-3cmL.png")
c.a <- as.raster(c.a)

#3 Chromis klunzingeri - no pic, use pomacentridae
c.k <- readPNG("data/images/Pomacentridae-Dark.png")
c.k <- as.raster(c.k)

#4 Caesioperca spp - no pic, use serranidae 
c.s <- readPNG("data/images/Serranidae-Dark.png")
c.s <- as.raster(c.s)

#5 Neatypus obliquus
n.o <- readPNG("data/images/Neatypus obliquus-3cmL.png")
n.o <- as.raster(n.o)

#6 Pseudolabrus biserialis
p.b <- readPNG("data/images/Pseudolabrus biserialis-3cm.png")
p.b <- as.raster(p.b)

#7 Ophthalmolepis lineolatus
o.l <- readPNG("data/images/Opthalmolepis lineolatus-3cm.png")
o.l <- as.raster(o.l)

#8 Nelusetta ayraud
n.a <- readPNG("data/images/Nelusetta ayraudi-3cm.png")
n.a <- as.raster(n.a)

#9 Pempheris klunzingeri
p.k <- readPNG("data/images/Pempheris klunzingeri-3cmL.png")
p.k <- as.raster(p.k)

#10 Austrolabrus maculatus
a.m <- readPNG("data/images/Austrolabrus maculatus-3cm.png")
a.m <- as.raster(a.m)

c.spp <- readPNG("data/images/Glaucosoma buergeri 5cmL.png")
c.spp <- as.raster(c.spp)

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
  annotation_raster(p.e, xmin=9.8,xmax=10.2,ymin=2850, ymax=3200)+              #1
  annotation_raster(c.a, xmin=8.7,xmax=9.3,ymin=2350, ymax=3150)+               #2
  annotation_raster(c.s, xmin=7.8, xmax=8.2, ymin=1800, ymax=2100)+             #3
  annotation_raster(n.o, xmin=6.775,xmax=7.275,ymin=1750, ymax=2100)+           #4
  annotation_raster(c.k, xmin=5.75,xmax=6.25,ymin=1800, ymax=2300)+             #5
  annotation_raster(p.b, xmin=4.75,xmax=5.25,ymin=1300, ymax=1900)+             #6
  annotation_raster(o.l, xmin=3.7,xmax=4.3,ymin=800, ymax=1600)+                #7
  annotation_raster(c.spp, xmin=2.5,xmax=3.5,ymin=250, ymax=1500)+                #8
  annotation_raster(n.a, xmin=1.75,xmax=2.25,ymin=230, ymax=700)+               #9
  annotation_raster(p.k, xmin=0.85,xmax=1.15,ymin=220, ymax=700)                #10
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.top.10

#save out plot
ggsave("plots/multibeam gamms/abundant.fish.bar.png",bar.top.10,dpi=600,width=6.0,height = 6.0)

#targeted species top 10 abundance
# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp",
                                                       "Berycidae Centroberyx sp1"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C","B/C"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))%>%
  # dplyr::select(status,scientific,fishing.type,australian.common.name)%>%
  glimpse()

# workout total maxn for each species ---
maxn.fished.10<-fished.species %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  dplyr::filter(!scientific%in%c("Centroberyx australis","Centroberyx lineatus"))%>%
  glimpse()

#have a look
bar<-ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#1 - Chrysophrys auratus
c.a <- as.raster(readPNG("data/images/Chrysophrys auratus 3cm.png"))

#2 - Pseudocaranx spp
p.spp <- as.raster(readPNG("data/images/Pseudocaranx dentex-3cm.png"))

#3 - Nemadactylus valenciennesi
n.v <- as.raster(readPNG("data/images/Nemadactylus valenciennesi-3cm.png"))

#4 - Epinephelides armatus
e.a <- as.raster(readPNG("data/images/Epinephelides armatus-3cmL.png"))

#5 - Seriola lalandi
s.l <- as.raster(readPNG("data/images/seriola_dumerili_nb.png"))

#5 - Seriola hippos
s.h <- as.raster(readPNG("data/images/Seriola_hippos_nb_HQ_TAYLOR.png"))

#6 - Centroberyx sp1
c.spp <- as.raster(readPNG("data/images/Glaucosoma buergeri 5cmL.png"))

#7 - Glaucosoma hebriacum
g.h <- as.raster(readPNG("data/images/Glaucosoma hebraicum 3cm.png"))

#8 - Furgaleus mackie
f.m <- as.raster(readPNG("data/images/Mustelus antarcticus-3cm.png"))

#9 - Bodianus frenchii
b.f <- as.raster(readPNG("data/images/Bodianus frenchii-3cm.png"))

#10 - Platycephalus spp
p.s <- as.raster(readPNG("data/images/Platycephalus speculator-3cm.png"))

#plot final bar plot
bar.fished.10<-ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 200)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(c.a, xmin=9.6,xmax=10.4,ymin=165, ymax=210)+                #1
  annotation_raster(p.spp, xmin=8.8,xmax=9.2,ymin=156, ymax=180)+               #2
  annotation_raster(c.spp, xmin=7.75, xmax=8.25, ymin=140, ymax=170)+              #3
  annotation_raster(n.v, xmin=6.65,xmax=7.35,ymin=90, ymax=130)+                #4
  annotation_raster(e.a, xmin=5.7,xmax=6.3,ymin=67, ymax=100)+                  #5
  annotation_raster(s.l, xmin=4.5,xmax=5.5,ymin=60, ymax=140)+                #6
  annotation_raster(g.h, xmin=3.55,xmax=4.45,ymin=38, ymax=90)+                 #7
  annotation_raster(f.m, xmin=2.55,xmax=3.45,ymin=35, ymax=120)+                #8
  annotation_raster(b.f, xmin=1.75,xmax=2.25,ymin=33, ymax=70)+                 #9
  annotation_raster(p.s, xmin=0.7,xmax=1.3,ymin=31, ymax=70)                    #10
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.fished.10

#save out plot
ggsave("plots/multibeam gamms/abundant.targets.bar.png",bar.fished.10,dpi=600,width=6.0, height = 6.0)

