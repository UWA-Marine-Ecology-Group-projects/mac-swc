###
# Project: mac - South-west Corner
# Data:    BRUV fish and habitat, fine bathymetry derivatives
# Task:    Run FSSgam for lengths
# author:  Claude & Brooke
# date:    February 2022
##

library(rstanarm)
library(tidyverse)
library(dplyr)
library(mgcv)
library(FSSgam)
library(MuMIn)
library(doBy)
library(GlobalArchive)
library(googlesheets4)

rm(list=ls())

# Set the study name
name <- '2020-2021_south-west_BOSS-BRUV_length' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

#bring in the data
dat <- readRDS('data/tidy/dat.length.multibeam.rds')%>%
  glimpse()

test <- dat %>%
  dplyr::group_by(campaignid,sample)%>%
  dplyr::summarise(n=n())

# Set predictor variables 
pred.vars=c("mean.relief","broad.macroalgae","broad.reef","distance.to.ramp", 
            "multibeam_derivatives_tpi","multibeam_derivatives_roughness","multibeam_derivatives_depth",
            "multibeam_derivatives_detrended")

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$maxn==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use 

resp.vars <- unique.vars.use

out.all <- list()
var.imp <- list()
fss.all=list() # added from beckys example
top.all=list()# added from beckys example

# Run the full subset model selection----
savedir <- "output/multibeam fish gamms"
resp.vars=unique.vars.use
use.dat=as.data.frame(dat)
use.dat$sample <- as.factor(use.dat$sample)
str(use.dat)

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$scientific==resp.vars[i]),])
  use.dat$site <- as.factor(use.dat$site)
  Model1=gam(number~s(multibeam_derivatives_depth,k=3,bs='cr')+s(site,bs='re'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               factor.smooth.interactions = FALSE,
                               # smooth.smooth.interactions = c("depth"),
                               pred.vars.cont=pred.vars,
                               #pred.vars.fact=factor.vars,
                               #linear.vars="depth",
                               k=3,
                               null.terms="s(site,bs='re')"
  )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T, n.cores = 8)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=5),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))
