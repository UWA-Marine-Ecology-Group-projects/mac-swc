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
dat1 <-read.csv("output/fish gamms/2020_south-west_stereo-BRUVs_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat2 <-read.csv("output/fish gamms/2020_south-west_stereo-BRUVs_length_all.var.imp.csv")%>% #from local copy
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
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
dat.taxa.label<-dat%>%
  mutate(label=NA)%>%
  mutate(resp.var = factor(resp.var, levels = c("Scorpididae Neatypus obliquus","Pomacentridae Chromis klunzingeri","Labridae Coris auricularis",
                                                "Sparidae Chrysophrys auratus","smaller than legal size","greater than legal size",
                                                "all greater than 30 cm","all greater than 20 cm","species.richness", "targeted.abundance",
                                                "total.abundance")))%>%  #change order of response variables
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="targeted.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis","X",label))%>%
  mutate(label=ifelse(predictor=="broad.sponges"&resp.var=="Pomacentridae Chromis klunzingeri","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Pomacentridae Chromis klunzingeri","X",label))%>%
  mutate(label=ifelse(predictor=="status"&resp.var=="Pomacentridae Chromis klunzingeri","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="Scorpididae Neatypus obliquus","X",label))%>%
  mutate(label=ifelse(predictor=="sd.relief"&resp.var=="Scorpididae Neatypus obliquus","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Sparidae Chrysophrys auratus","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Sparidae Chrysophrys auratus","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="Sparidae Chrysophrys auratus","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="sd.relief"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="broad.reef"&resp.var=="all greater than 20 cm","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="all greater than 20 cm","X",label))%>%
  mutate(label=ifelse(predictor=="distance.to.ramp"&resp.var=="all greater than 30 cm","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="all greater than 30 cm","X",label))%>%
  mutate(label=ifelse(predictor=="broad.macroalgae"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="sd.relief"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="broad.sponges"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="smaller than legal size","X",label))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  # scale_y_discrete(labels=c("All greater than 20cm", "All greater than 30cm", "Greater than legal size", "*Coris auricularis*",
  #                           "*Chromis klunzingeri*", "*Neatypus obliquus*", "Smaller than legal size","*Chrysophrys auratus*",
  #                           "Species richness","Targeted abundance","Total abundance"))+         #Tidy Taxa names
  scale_x_discrete(labels = c("Aspect","Macroalgae","Reef","Sponges","Depth","Distance to boat ramp","Mean relief","Roughness",
                              "SD relief","Status","TPI"))+   #Tidy predictor names
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  theme(axis.text.y = ggtext::element_markdown())+
  geom_text(aes(label=label))
gg.importance.scores

#save output - changed dimensions for larger text in report
save_plot("plots/original gamms/swc_fish-importance-full.png", gg.importance.scores,base_height = 6.75,base_width = 6.275)
