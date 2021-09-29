rm(list=ls())

library(dplyr)
library(tidyr)


# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- "Z:/SWC-multibeam-GAMMS"

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


# Load the importance score dataset produced above  
dat.maxn <-read.csv("2020_south-west_stereo-BRUVs_multibeam_all.var.imp.csv")%>% #from local copy
  dplyr::rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat.biomass <-read.csv("2020_south-west_stereo-BRUVs_multibeam_length_all.var.imp.csv")%>% #from local copy
  dplyr::rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()


dat.fh <- bind_rows(dat.maxn,dat.biomass) %>%
  filter(!resp.var%in%c("targeted.abundance","Labridae Ophthalmolepis lineolatus","Scorpididae Neatypus obliquus",
                        "fished greater than 20 cm","fished greater than 30 cm"))%>%
  glimpse()


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
    text=element_text(size=12),
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

# Annotations-  # Puts "X"s for variables in your top models
dat.taxa.label<-dat.fh%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor =="depth" &resp.var=="Heterodontidae Heterodontus portusjacksoni","X",
                      ifelse(predictor=="log.roughness"&resp.var=="Heterodontidae Heterodontus portusjacksoni","X",
                             ifelse(predictor=="log.sd.relief"&resp.var=="Heterodontidae Heterodontus portusjacksoni","X",label))))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="total.abundance","X",
                      ifelse(predictor=="tpi"&resp.var=="total.abundance","X",
                             ifelse(predictor=="mean.relief"&resp.var=="total.abundance","X",label))))%>%
  mutate(label=ifelse(predictor=="mean.relief"&resp.var=="species.richness","X",
                      ifelse(predictor=="sqrt.slope"&resp.var=="species.richness","X",label)))%>%
  mutate(label=ifelse(predictor=="log.roughness"&resp.var=="Sparidae Chrysophrys auratus","X",
                      ifelse(predictor=="status"&resp.var=="Sparidae Chrysophrys auratus","X",label)))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis","X",
                      ifelse(predictor=="tpi"&resp.var=="Labridae Coris auricularis","X",
                             ifelse(predictor=="log.slope"&resp.var=="Labridae Coris auricularis","X",label))))%>%
  mutate(label=ifelse(predictor=="log.sd.relief"&resp.var=="all greater than 20 cm","X",
                      ifelse(predictor=="aspect"&resp.var=="all greater than 20 cm","X",
                             ifelse(predictor=="tpi"&resp.var=="all greater than 20 cm","X",label))))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="all greater than 30 cm","X",
                      ifelse(predictor=="depth"&resp.var=="all greater than 30 cm","X",
                             ifelse(predictor=="sqrt.slope"&resp.var=="all greater than 30 cm","X",label))))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="greater than legal size","X",
                      ifelse(predictor=="log.roughness"&resp.var=="greater than legal size","X",label)))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="smaller than legal size","X",
                      ifelse(predictor=="log.roughness"&resp.var=="smaller than legal size","X",
                             ifelse(predictor=="broad.macroalgae"&resp.var=="smaller than legal size","X",label))))%>%
  glimpse()

unique(dat.fh$predictor)
unique(dat.fh$resp.var)

# Plot gg.importance.scores ----
#install.packages("ggtext") 
library(ggtext)


gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("status",
                          #  "distance.to.ramp",
                            "depth",
                            "mean.relief",
                            "log.sd.relief",
                            "broad.macroalgae",
                            "aspect",
                            "tpi",
                            "log.roughness",
                            "sqrt.slope"),
                   labels=c(
                     "Status",
                    # "Distance to ramp",
                     "Depth",
                     "Mean relief",
                     "SD relief",
                     "Macroalgae",
                     "Aspect",
                     "TPI",
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
  geom_text(aes(label=label),size=10)
gg.importance.scores


setwd(p.dir)
dir()
#?ggsave
ggsave( "importance.scores.tiff",plot=gg.importance.scores,width=8, height=7, dpi=300, device="tiff")
#ggsave("delay_24052020.tiff", plot=plot, height=17, width=15, device="tiff", dpi=300)
