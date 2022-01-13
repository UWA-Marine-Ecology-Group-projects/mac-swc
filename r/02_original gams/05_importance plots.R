rm(list=ls())

library(dplyr)
library(tidyr)


# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

## Set sub directories----
d.dir <- paste(working.dir,"Data/Tidy",sep="/") 
h.dir <- paste(working.dir, "Data/Habitat/BRUV Style annotation/tidy data",sep="/") 
s.dir <- paste(working.dir,"shapefiles",sep="/")
p.dir <- paste(working.dir,"Plots",sep="/")
m.dir <- paste(working.dir,"Model Out GAM", sep="/")

# Bring in and format the data----
setwd(m.dir)
dir()

# custom plot of importance scores----

# FISHING HIGHWAY ----
# Load the importance score dataset produced above
dat.fh1 <-read.csv("2020_south-west_stereo-BRUVs_FH_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat.fh2 <-read.csv("2020_south-west_stereo-BRUVs_FH_length_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()


dat.fh <- bind_rows(dat.fh1,dat.fh2) %>%
  filter(!resp.var%in%c("targeted.abundance","Labridae Ophthalmolepis lineolatus","Scorpididae Neatypus obliquus",
                        "fished greater than 20 cm","fished greater than 30 cm"))


# Plotting defaults----
library(ggplot2)
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
dat.taxa.label<-dat.fh%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="Distance"&resp.var=="BDS","X",
                      ifelse(predictor=="Status"&resp.var=="BDS","X",
                             ifelse(predictor=="sqrt.X500um"&resp.var=="BDS","X",label))))%>%
  mutate(label=ifelse(predictor=="lobster"&resp.var=="BMS","X",label))%>%
  mutate(label=ifelse(predictor=="sqrt.X4mm"&resp.var=="CPN","X",
                      ifelse(predictor=="lobster"&resp.var=="CPN","X",label)))%>%
  glimpse()

unique(dat.fh$predictor)
unique(dat.fh$resp.var)

# Plot gg.importance.scores ----
library(ggtext)


gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("status",
                            "distance.to.ramp",
                            "depth",
                            "mean.relief",
                            "sd.relief",
                            "broad.reef",
                            "broad.macroalgae",
                            "log.sponges",
                            "aspect",
                            "log.tpi",
                            "log.roughness",
                            "log.slope"),
                   labels=c(
                     "Status",
                     "Distance to ramp",
                     "Depth",
                     "Mean relief",
                     "SD relief",
                     "Reef",
                     "Macroalgae",
                     "Log Sponges",
                     "Aspect",
                     "Log TPI",
                     "Log Roughness",
                     "Log Slope"
                   ))+
  scale_y_discrete(limits = c("smaller than legal size" ,  
                                                            "greater than legal size",
                                                            "all greater than 30 cm",
                                                            "all greater than 20 cm" ,
                                                            "Heterodontidae Heterodontus portusjacksoni",
                                                            "Labridae Coris auricularis",
                                                            "Sparidae Chrysophrys auratus",
                                                            "species.richness",
                                                            "total.abundance"
  ),
                   labels=c(                            "Smaller than legal size",
                                                        "Greater than legal size",
                                                        "Greater than 30 cm",
                                                        "Greater than 20 cm",
                                                        "*H. portusjacksoni*",
                                                        "*C. auricularis*",
                                                        "*C. auratus*",
                                                        "Species richness",
                                                        "Total abundance"
                   ))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  theme(axis.text.y = element_markdown())+
  geom_text(aes(label=label))
gg.importance.scores


# IN/OUT----
# Load the importance score dataset produced above
dat.io1 <-read.csv("2020_south-west_stereo-BRUVs_IO_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat.io2 <-read.csv("2020_south-west_stereo-BRUVs_IO_length_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()


dat.io <- bind_rows(dat.io1,dat.io2) %>%
  filter(!resp.var%in%c("targeted.abundance","Labridae Ophthalmolepis lineolatus","Scorpididae Neatypus obliquus",
                        "fished greater than 20 cm","fished greater than 30 cm","sublegal size pink snapper"))



# 
# Annotations-
dat.taxa.label<-dat.io%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="Distance"&resp.var=="BDS","X",
                      ifelse(predictor=="Status"&resp.var=="BDS","X",
                             ifelse(predictor=="sqrt.X500um"&resp.var=="BDS","X",label))))%>%
  mutate(label=ifelse(predictor=="lobster"&resp.var=="BMS","X",label))%>%
  mutate(label=ifelse(predictor=="sqrt.X4mm"&resp.var=="CPN","X",
                      ifelse(predictor=="lobster"&resp.var=="CPN","X",label)))%>%
  glimpse()

unique(dat.io$predictor)
unique(dat.io$resp.var)

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("status",
                            "distance.to.ramp",
                            "depth",
                            "mean.relief",
                            "sd.relief",
                            "broad.reef",
                            "broad.macroalgae",
                            "log.sponges",
                            "aspect",
                            "log.tpi",
                            "log.roughness",
                            "log.slope"),
                   labels=c(
                     "Status",
                     "Distance to ramp",
                     "Depth",
                     "Mean relief",
                     "SD relief",
                     "Reef",
                     "Macroalgae",
                     "Log Sponges",
                     "Aspect",
                     "Log TPI",
                     "Log Roughness",
                     "Log Slope"
                   ))+
  scale_y_discrete(limits = c("smaller than legal size" ,
                              "greater than legal size",
                              "all greater than 30 cm",
                              "all greater than 20 cm" ,
                              "Monacanthidae Nelusetta ayraud",
                              "Heterodontidae Heterodontus portusjacksoni",
                             
                              "Labridae Coris auricularis",
                              "Sparidae Chrysophrys auratus",
                              "species.richness",
                              "total.abundance"
  ),
  labels=c(                            "Smaller than legal size",
                                       "Greater than legal size",
                                       "Greater than 30 cm",
                                       "Greater than 20 cm",
                                       "*N. ayraud*",
                                       "*H. portusjacksoni*",
                                       
                                       "*C. auricularis*",
                                       "*C. auratus*",
                                       "Species richness",
                                       "Total abundance"
  ))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  theme(axis.text.y = element_markdown())+
  geom_text(aes(label=label))
gg.importance.scores



