###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Model selection 
# author:  Kingsley Griffin from @beckyfisher/FSSgam
# date:    Feb 2022

# Part 1-FSS modeling----

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
library(mgcv)
# library(MuMIn)
# library(car)
# library(doBy)
# library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
# library(gamm4)
# library(RCurl) #needed to download data from GitHub
library(reshape2)

rm(list=ls())

# install fssgam package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Bring in and format the data----
habi  <- readRDS("data/tidy/dat.full.habitat.rds")                              # merged data from ??
spcov <- readRDS("data/tidy/habitat_spatialcovs.rds")

habi <- merge(habi, spcov, by = "sample")
head(habi)
saveRDS(habi, "data/tidy/habitat_merged.rds")

# habi <- habi[ , -c(9:49)]
colnames(habi)
habi <- melt(habi, measure.vars = c(3:14, 16:17))                               # collect all taxa tags for univariate stats
head(habi)


# Set predictor variables---
pred.vars <- c("depth.y", "tpi", "roughness", "detrended") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(habi[ , pred.vars]), 2)
# several highly correlated terrain variables here but I think we need to keep them?

# # Review of individual predictors for even distribution---
# # Plot of likely transformations - Anna Cresswell loop
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x<-habi[ , i]
#   x = as.numeric(unlist(x))
#   hist((x))#Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }
# 
# review and create cols for best transforms
habi <- habi %>%
  rename(Taxa = variable) %>%
  rename(response = value)

# # Re-set the predictors for modeling----
pred.vars <- c("depth.y","roughness", "tpi", "detrended") 

# Check to make sure Response vector has not more than 80% zeros----
unique.vars     <- unique(as.character(habi$Taxa))
unique.vars.use <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- habi[which(habi$Taxa == unique.vars[i]),]
  if(length(which(temp.dat$response == 0)) / nrow(temp.dat) < 0.8){
    unique.vars.use <- c(unique.vars.use, unique.vars[i])}
}
unique.vars.use
# unique.vars.use <- unique.vars[unique.vars != "turf"]
# unique.vars.use <- unique.vars.use[11:12] # only macroalgae and sponge of interest. remove unknown, open water
# unique.vars.use     

# Run the full subset model selection----
outdir    <- ("output/habitat_fssgam/") #Set wd for example outputs
resp.vars <- unique.vars.use
use.dat   <- habi[habi$Taxa %in% c(unique.vars.use), ]
# factor.vars <- c("Status")# Status as a Factor with two levels
out.all <- list()
var.imp <- list()
name <- "eg"

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- habi[habi$Taxa == resp.vars[i],]
  use.dat <- use.dat[!(use.dat$broad.total.points.annotated - 
                         use.dat$response < 0), ] # added to fix weird point
  # use.dat$Site <- as.factor(use.dat$Site)
  Model1  <- gam(cbind(response, (broad.total.points.annotated - response)) ~ 
                   s(depth.y, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                               test.fit = Model1,
                               pred.vars.cont = pred.vars,
                               # pred.vars.fact=factor.vars,
                               # linear.vars="Distance",
                               # cyclic.vars = c("aspect"),
                               k = 5,
                               cov.cutoff = 0.7
                               # null.terms = "s(Site, bs='re')"
                               )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 200), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(outdir, name, "all.mod.fits.csv", sep = ""))
write.csv(all.var.imp,         file = paste(outdir, name, "all.var.imp.csv", sep = ""))
out.all
 # Generic importance plots- - unsure why we're not getting any value for the other preds. internal m.cor exclusion?
heatmap.2(all.var.imp, notecex = 0.4,  dendrogram = "none",
          col = colorRampPalette(c("white", "yellow", "red"))(10),
          trace = "none", key.title = "", keysize = 2,
          notecol = "black", key = T,
          sepcolor = "black", margins = c(20, 20), lhei = c(2, 6), Rowv = FALSE, Colv = FALSE)

# 
# # Part 2 - custom plot of importance scores----
# 
# dat.taxa <- as.data.frame(all.var.imp)
# 
# # Load the importance score dataset produced above
# # dat.taxa <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_model_out/clams_all.var.imp.csv"))%>% #from github
# dat.taxa <- dat.taxa %>%
#   # read.csv("clams_all.var.imp.csv") %>% #from local copy
#   mutate(resp.var = rownames(dat.taxa)) %>%
#   # rename(resp.var = rownames(dat.taxa)) %>%
#   gather(key = predictor, value = importance, 1:ncol(.)-1) %>%
#   glimpse()
# 
# 
# # Plotting defaults----
# library(ggplot2)
# # Theme-
# Theme1 <-
#   theme( # use theme_get() to see available options
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.background = element_rect(fill="white"),
#     legend.key = element_blank(), # switch off the rectangle around symbols in the legend
#     legend.text = element_text(size=8),
#     legend.title = element_text(size=8, face="bold"),
#     legend.position = "top",
#     legend.direction="horizontal",
#     text=element_text(size=10),
#     strip.text.y = element_text(size = 10,angle = 0),
#     axis.title.x=element_text(vjust=0.3, size=10),
#     axis.title.y=element_text(vjust=0.6, angle=90, size=10),
#     axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
#     axis.text.y=element_text(size=10,face="italic"),
#     axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
#     axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
#     strip.background = element_blank())
# 
# 
# # colour ramps-
# re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
# 
# # Labels-
# legend_title<-"Importance"
# 
# # Annotations-
# dat.taxa.label<-dat.taxa%>%
#   mutate(label=NA)%>%
#   mutate(label=ifelse(predictor=="Distance"&resp.var=="BDS","X",ifelse(predictor=="Status"&resp.var=="BDS","X",ifelse(predictor=="sqrt.X500um"&resp.var=="BDS","X",label))))%>%
#   mutate(label=ifelse(predictor=="lobster"&resp.var=="BMS","X",label))%>%
#   mutate(label=ifelse(predictor=="sqrt.X4mm"&resp.var=="CPN","X",ifelse(predictor=="lobster"&resp.var=="CPN","X",label)))%>%
#   glimpse()
# 
# # Plot gg.importance.scores ----
# gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
#   geom_tile(show.legend=T) +
#   scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
#                        limits = c(0, max(dat.taxa.label$importance)))+
#   xlab(NULL)+
#   ylab(NULL)+
#   theme_classic()+
#   Theme1+
#   geom_text(aes(label=label))
# gg.importance.scores
# 
# # Part 3 - plots of the most parsimonious models----
# 
# ### now  make a nice plot of the most interesting models-----
# library(gridExtra)
# library(grid)
# # Theme-
# Theme1 <-
#   theme( # use theme_get() to see available options
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # legend.background = element_rect(fill="white"),
#     legend.background = element_blank(),
#     legend.key = element_blank(), # switch off the rectangle around symbols in the legend
#     legend.text = element_text(size=15),
#     legend.title = element_blank(),
#     legend.position = c(0.2, 0.8),
#     text=element_text(size=15),
#     strip.text.y = element_text(size = 15,angle = 0),
#     axis.title.x=element_text(vjust=0.3, size=15),
#     axis.title.y=element_text(vjust=0.6, angle=90, size=15),
#     axis.text.x=element_text(size=15),
#     axis.text.y=element_text(size=15),
#     axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
#     axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
#     strip.background = element_blank())
# 
# # 
# # Bring in and format the raw data----
# 
# 
# # Load the dataset - from github
# # dat <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_dataset.csv?token=AOSO6uyYhat9-Era46nbjALQpTydsTskks5ZY3vhwA%3D%3D"))%>%
# # Load the dataset - from local files
# dat <-read.csv("case_study2_dataset.csv")%>%
#   rename(response=Abundance)%>%
#   #   Transform variables
#   mutate(sqrt.X4mm=sqrt(X4mm))%>%
#   mutate(sqrt.X2mm=sqrt(X2mm))%>%
#   mutate(sqrt.X1mm=sqrt(X1mm))%>%
#   mutate(sqrt.X500um=sqrt(X500um))%>%
#   mutate(distance=as.numeric(as.character(Distance)))%>%
#   na.omit()%>%
#   glimpse()
# 
# 
# 
# 
# # Manually make the most parsimonious GAM models for each taxa ----
# setwd("~/GitHub/FSSgam/case_study2_model_out")
# 
# 
# # MODEL Bivalve.Dosina.subrosea 500um + distance x Status ----
# dat.bds<-dat%>%filter(Taxa=="BDS")
# gamm=gam(response~s(sqrt.X500um,k=3,bs='cr')+s(distance,k=1,bs='cr', by=Status)+ s(Location,Site,bs="re")+ Status, family=tw(),data=dat.bds)
# 
# # predict - status from MODEL Bivalve.Dosina.subrosea----
# mod<-gamm
# testdata <- expand.grid(distance=mean(mod$model$distance),
#                         sqrt.X500um=mean(mod$model$sqrt.X500um),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# #head(fits,2)
# predicts.bds.status = testdata%>%data.frame(fits)%>%
#   group_by(Status)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.bds.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.bds.status<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # predict - distance.x.status from MODEL Bivalve.Dosina.subrosea----
# mod<-gamm
# testdata <- expand.grid(distance=seq(min(dat$distance),max(dat$distance),length.out = 20),
#                         sqrt.X500um=mean(mod$model$sqrt.X500um),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# #head(fits,2)
# predicts.bds.distance.x.status = testdata%>%data.frame(fits)%>%
#   group_by(distance,Status)%>% #only change here
#   # group_by(sqrt.X500um,Status)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.bds.distance.x.status,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.bds.distance.x.status<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # predict 500um from MODEL Bivalve.Dosina.subrosea----
# mod<-gamm
# testdata <- expand.grid(sqrt.X500um=seq(min(dat$sqrt.X500um),max(dat$sqrt.X500um),length.out = 20),
#                         distance=mean(mod$model$distance),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# #head(fits,2)
# predicts.bds.500um = testdata%>%data.frame(fits)%>%
#   group_by(sqrt.X500um)%>% #only change here
#   # group_by(sqrt.X500um,Status)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.bds.500um,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.bds.500um<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # MODEL Bivalve.Myadora.striata  Lobster----
# dat.bms<-dat%>%filter(Taxa=="BMS")
# head(dat.bms,2)
# gamm=gam(response~s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.bms)
# 
# # predict - lobster from model for Bivalve.Myadora.striata ----
# mod<-gamm
# testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# #head(fits,2)
# predicts.bms.lobster = testdata%>%data.frame(fits)%>%
#   group_by(lobster)%>% #only change here
#   # group_by(sqrt.X500um,Status)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.bms.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.bms.lobster<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # MODEL Decapod.P.novazelandiae 4mm + Lobster----
# dat.cpn<-dat%>%filter(Taxa=="CPN")
# head(dat.cpn,2)
# gamm=gam(response~s(sqrt.X4mm,k=3,bs='cr')+s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"), family=tw(),data=dat.cpn)
# 
# # predict - sqrt.X4mm from model for Decapod.P.novazelandiae ----
# mod<-gamm
# testdata <- expand.grid(sqrt.X4mm=seq(min(dat$sqrt.X4mm),max(dat$sqrt.X4mm),length.out = 20),
#                         lobster=mean(mod$model$lobster),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# head(fits,2)
# predicts.cpn.4mm = testdata%>%data.frame(fits)%>%
#   group_by(sqrt.X4mm)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.cpn.4mm,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.cpn.4mm<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # predict - lobster from model for Decapod.P.novazelandiae ----
# mod<-gamm
# testdata <- expand.grid(lobster=seq(min(dat$lobster),max(dat$lobster),length.out = 20),
#                         sqrt.X4mm=mean(mod$model$sqrt.X4mm),
#                         Location=(mod$model$Location),
#                         Site=(mod$model$Site),
#                         Status = c("Fished","No-take"))%>%
#   distinct()%>%
#   glimpse()
# 
# fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# #head(fits,2)
# predicts.cpn.lobster = testdata%>%data.frame(fits)%>%
#   group_by(lobster)%>% #only change here
#   # group_by(sqrt.X500um,Status)%>% #only change here
#   summarise(response=mean(fit),se.fit=mean(se.fit))%>%
#   ungroup()
# write.csv(predicts.cpn.lobster,"predicts.csv") #there is some BUG in dplyr - that this fixes
# predicts.cpn.lobster<-read.csv("predicts.csv")%>%
#   glimpse()
# 
# # PLOTS for Bivalve.Dosina.subrosea 500um + distance x Status ----
# ggmod.bds.status<- ggplot(aes(x=Status,y=response,fill=Status,colour=Status), data=predicts.bds.status) +
#   ylab(" ")+
#   xlab('Status')+
#   #   ggtitle(substitute(italic(name)))+
#   scale_fill_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
#   scale_colour_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
#   scale_x_discrete(limits = rev(levels(predicts.bds.status$Status)))+
#   geom_bar(stat = "identity")+
#   geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(a)",vjust = 1, hjust = -.1,size=5)+
#   annotate("text", x = -Inf, y=Inf, label = "   Dosinia subrosea",vjust = 1, hjust = -.1,size=5,fontface="italic")
# ggmod.bds.status
# 
# ggmod.bds.distance.x.status<- ggplot(aes(x=distance,y=response,colour=Status), data=dat.bds) +
#   ylab(" ")+
#   xlab('Distance (m)')+
#   #   ggtitle(substitute(italic(name)))+
#   scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
#   geom_jitter(width = 0.25,height = 0,alpha=0.75, size=2,show.legend=FALSE)+
#   # geom_point(alpha=0.75, size=2)+
#   geom_line(data=predicts.bds.distance.x.status,show.legend=FALSE)+
#   geom_line(data=predicts.bds.distance.x.status,aes(y=response - se.fit),linetype="dashed",show.legend=FALSE)+
#   geom_line(data=predicts.bds.distance.x.status,aes(y=response + se.fit),linetype="dashed",show.legend=FALSE)+
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=5)
# ggmod.bds.distance.x.status
# 
# ggmod.bds.500um<- ggplot() +
#   ylab(" ")+
#   xlab('Grain size: 500 um (sqrt)')+
#   #   ggtitle(substitute(italic(name)))+
#   scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
#   #   geom_jitter(width = 0.25,height = 0)+
#   geom_point(data=dat.bds,aes(x=sqrt.X500um,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
#   geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response),alpha=0.5)+
#   geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.bds.500um,aes(x=sqrt.X500um,y=response + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(c)",vjust = 1, hjust = -.1,size=5)
# ggmod.bds.500um
# 
# # PLOTS Bivalve M.striata lobster ----
# ggmod.bms.lobster<- ggplot() +
#   ylab("Abundance")+
#   xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
#   scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
#   geom_point(data=dat.bms,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
#   geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response),alpha=0.5)+
#   geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.bms.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
#   annotate("text", x = -Inf, y=Inf, label = "   Myadora striata",vjust = 1, hjust = -.1,size=5,fontface="italic")+
#   geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
# ggmod.bms.lobster
# 
# # PLOTS Decapod.P.novazelandiae 4mm + lobster ----
# ggmod.cpn.lobster<- ggplot() +
#   ylab(" ")+
#   xlab(bquote('Density of legal lobster (no./25' *m^-2*')'))+
#   scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
#   geom_point(data=dat.cpn,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
#   geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response),alpha=0.5)+
#   geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.cpn.lobster,aes(x=lobster,y=response + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(e)",vjust = 1, hjust = -.1,size=5)+
#   annotate("text", x = -Inf, y=Inf, label = "  Pagurus novizelandiae",vjust = 1, hjust = -.1,size=5,fontface="italic")+
#   geom_blank(data=dat.cpn,aes(x=lobster,y=response*1.05))#to nudge data off annotations
# ggmod.cpn.lobster
# 
# ggmod.cpn.4mm<- ggplot() +
#   ylab(" ")+
#   xlab('Grain size: 4 mm (sqrt)')+
#   scale_color_manual(labels = c("Fished", "No-take"),values=c("red", "black"))+
#   geom_point(data=dat.cpn,aes(x=sqrt.X4mm,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
#   geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response),alpha=0.5)+
#   geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response - se.fit),linetype="dashed",alpha=0.5)+
#   geom_line(data=predicts.cpn.4mm,aes(x=sqrt.X4mm,y=response + se.fit),linetype="dashed",alpha=0.5)+
#   theme_classic()+
#   Theme1+
#   annotate("text", x = -Inf, y=Inf, label = "(f)",vjust = 1, hjust = -.1,size=5)+
#   annotate("text", x = -Inf, y=Inf, label = " ",vjust = 1, hjust = -.1,size=5,fontface="italic")
# ggmod.cpn.4mm
# 
# # combined.plot using grid() and gridExtra()------
# blank <- grid.rect(gp=gpar(col="white"))
# 
# # To see what they will look like use grid.arrange() - make sure Plot window is large enough! - or will error!
# grid.arrange(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
#              ggmod.bms.lobster,blank,blank,
#              ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)
# 
# # Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
# combine.plot<-arrangeGrob(ggmod.bds.status,ggmod.bds.distance.x.status,ggmod.bds.500um,
#                           ggmod.bms.lobster,blank,blank,
#                           ggmod.cpn.lobster,ggmod.cpn.4mm,blank,nrow=3,ncol=3)
# 
# ggsave(combine.plot,file="Langlois_gamm.plot.png", width = 30, height = 30,units = "cm")
# 
# 


