###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, broad bathymetry derivatives
# Task:    Plot variable importance scores - multibeam patch
# author:  Claude
# date:    February 2022
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)
library(cowplot)

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

# custom plot of importance scores----
# Load the importance score dataset produced above
dat1 <-read.csv("output/multibeam fish gamms/2020_south-west_stereo-BRUVs_all.var.imp.csv")%>% #from local copy
  dplyr::rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat2 <-read.csv("output/multibeam fish gamms/2020_south-west_stereo-BRUVs_length_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat <- bind_rows(dat1,dat2)%>%
  glimpse()

# Plotting defaults----
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10), # ,face="italic"
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps-
re <- colorRampPalette(c("#1034A6", "white","#F62D2D"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
dat.taxa.label<-dat%>%
  mutate(label=NA)%>%
  mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(predictor=factor(predictor, levels = c("broad.reef","broad.macroalgae","mean.relief","sd.relief",
                                                "depth","roughness","tpi","detrended","distance.to.ramp","status")))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="tpi","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="broad.reef"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="smaller than legal size","X",label))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(-1, 1))+
  scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size","Species richness","Total abundance"))+         #Tidy Taxa names
  scale_x_discrete(labels = c("Reef","Macroalgae","Mean relief","SD relief","Depth","Roughness","TPI","Detrended bathymetry","Distance to ramp","Status"))+   #Tidy predictor names
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  theme(axis.text.y = ggtext::element_markdown())+
  geom_text(aes(label=label))
gg.importance.scores

#save output - changed dimensions for larger text in report
save_plot("plots/multibeam gamms/swc_fish-importance-full.png", gg.importance.scores,base_height = 5,base_width = 7)
