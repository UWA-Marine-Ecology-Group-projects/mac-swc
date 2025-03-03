###
# Project: mac - swc
# Data:    BOSS & BRUV fish, habitat
# Task:    Habitat importance scores
# author:  Claude
# date:    March 2022
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

dat.taxa <-read.csv("output/habitat_fssgam/south-west_full_habitat_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  mutate(label=NA)%>%
  dplyr::filter(!resp.var%in%"broad.invertebrate.complex")%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="biogenic_reef","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="biogenic_reef","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="biogenic_reef","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="broad.consolidated","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="broad.consolidated","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="broad.consolidated","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="broad.macroalgae","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="broad.macroalgae","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="broad.macroalgae","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="broad.seagrasses","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="broad.seagrasses","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="broad.seagrasses","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="broad.sponges","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="broad.sponges","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="broad.sponges","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="broad.unconsolidated","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="broad.unconsolidated","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="broad.unconsolidated","X",label))%>%
  glimpse()

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
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps-
re <- colorRampPalette(c("blue3", "white","red2"))(200)

# Labels-
legend_title<-"Importance"

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa, aes(x=predictor,y=resp.var,fill=importance)) +
   geom_tile(show.legend=T) +
   scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                         limits = c(-1, 1))+
      scale_y_discrete( labels=c("Biogenic reef","Consolidated (rock)", "Macroalgae", "Seagrasses", "Sponges",
                                 "Unconsolidated (sand)"))+
   scale_x_discrete(labels = c("Depth","Detrended","Roughness", "TPI"))+
   xlab(NULL)+
   ylab(NULL)+
   theme_classic()+
   Theme1+
   geom_text(aes(label=label))
gg.importance.scores

#save plots
save_plot("plots/original gamms/full.habitat.importance.scores.png", gg.importance.scores,base_height = 6,base_width = 8.75)
