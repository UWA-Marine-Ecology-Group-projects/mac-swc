#devtools::install_github("beckyfisher/FSSgam_package") #run once
require(rstanarm)
require(tidyverse)
require(dplyr)
require(mgcv)
require(FSSgam)
require(MuMIn)
require(doBy)
require(GlobalArchive)
require(googlesheets4)
require(stringr)
require(data.table)

rm(list=ls())

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
setwd(d.dir)
dir()

# MaxN ----
maxn <-read.csv(paste(name, 'complete.maxn.csv',sep=".")) %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv(paste(name, 'bathymetry.derivatives.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Habitat ----
habitat.2020.10 <- read.csv("2020-10_south-west_stereo-BRUVs_BRUV_style.broad.habitat.csv") %>%
  dplyr::select(-c(rowid.x,rowid.y)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.10)

habitat.2020.06 <- read.csv("2020-06._broad.habitat_BRUV_Style.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.06) # 0-100

habitat <-bind_rows(habitat.2020.06, habitat.2020.10) %>%
  tidyr::replace_na(list(broad.Consolidated=0,
                         broad.Macroalgae=0,
                         broad.Seagrasses=0,
                         broad.Sponges=0,
                         broad.Unconsolidated=0,
                         broad.Bryozoa=0,
                         broad.Hydroids=0,
                         broad.Octocoral.Black=0,
                         broad.Stony.corals=0,
                         fov.Facing.Up=0)) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + broad.seagrasses + broad.sponges + broad.stony.corals) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(campaignid,sample,everything()) %>% # re-ordering hab columns 
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample,total.abundance,species.richness) %>%
  tidyr::gather(.,"scientific","maxn",2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
setwd(d.dir)
dir()

# Had to download and use csv as Nectar can't connect to googlesheets :(
master <- read.csv("australia.life.history.csv") %>%
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,fishing.type,australian.common.name)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

unique(master$fishing.type)

spp.species<-maxn%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) # Brooke removed leatherjackets, sea sweeps and goat fish
  
unique(fished.species$scientific)

list.for.tim <- fished.species %>% 
  distinct(scientific, australian.common.name, fishing.type)

write.csv(list.for.tim,"fished.species.list.csv")

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse()

# Select species of interest to model ----
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",
                           "Glaucosomatidae Glaucosoma hebraicum",
                           "Labridae Coris auricularis",
                           "Scorpididae Neatypus obliquus",
                           "Labridae Ophthalmolepis lineolatus",
                           "Heterodontidae Heterodontus portusjacksoni",
                           "Monacanthidae Nelusetta ayraud"
                           ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()

test.samples <- species.maxn %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n())

unique(species.maxn$scientific)

# 287 samples x 7 species
287*7

centroberyx <- maxn %>%
  dplyr::filter(genus%in%c("Centroberyx")) %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(centroberyx=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,centroberyx) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse() # there is actually not that many

unique(maxn$scientific)

# Split metadata into Fishing HWY and In/Out dataframes
summary(metadata)

metadata.fh <- metadata %>%
  dplyr::filter(depth<50)

metadata.io <- metadata %>%
  dplyr::filter(latitude<=(-33.96))

plot(metadata$longitude, metadata$latitude)

plot(metadata.fh$longitude, metadata.fh$latitude)
plot(metadata.io$longitude, metadata.io$latitude)

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(fished.maxn, species.maxn, 
                           ta.sr, centroberyx)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  filter(!scientific%in%c("Glaucosomatidae Glaucosoma hebraicum",
                          "centroberyx",
                          "Labridae Ophthalmolepis lineolatus",
                          "Scorpididae Neatypus obliquus",
                          "targeted.abundance"
                          )) %>%
  distinct()

# rows should be 4 predictors x 287 samples
4*287
11*287 # when specific species are included

unique(combined.maxn$sample) # 287

maxn.fh <- combined.maxn %>%
  semi_join(., metadata.fh) %>%
  filter(!scientific%in%c("Monacanthidae Nelusetta ayraud"))

unique(maxn.fh$sample)
unique(maxn.fh$scientific)

maxn.io <- combined.maxn %>%
  semi_join(., metadata.io)

unique(combined.maxn$scientific)

names(maxn.fh)

# Set predictor variables---
pred.vars=c("depth", "slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa", "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief" )

dat <- maxn.fh
dat <- combined.maxn

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars], use = "complete.obs"),2)
# reef and sand correlated

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# use 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# sponges - log
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth - use non-transformed
# broad.reef - use non-transformed
# tpi - log
# roughness log
# slope log

# remove
# sand - correlated with reef and mean relief
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few

# StART OF CHARLOTTES A4
names(maxn.fh)

maxn.fh <- maxn.fh %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

maxn.io <- maxn.io %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
            "distance.to.ramp",
            "aspect", "log.tpi","log.roughness","log.slope",
            "depth")

#### FSSgam using lme4 + random site ####
setwd(m.dir)

names(maxn.fh)

# Remove any unused columns from the dataset 
dat <- maxn.fh%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, maxn,
                "mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp",
                "aspect", "log.tpi","log.roughness","log.slope",
                "depth") %>%
  #dplyr::filter(scientific%in%c("total.abundance","species.richness",
  #                              "Sparidae Chrysophrys auratus", "Labridae Coris auricularis")) %>%
  as.data.frame()

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$maxn==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use  

resp.vars <- unique.vars.use
factor.vars <- c("status")

out.all <- list()
var.imp <- list()
fss.all=list() # added from beckys example
top.all=list()# added from beckys example

for(i in 1:length(resp.vars)){
  
use.dat <- dat[which(dat$scientific==resp.vars[i]),]

Model1 <- uGamm(maxn~s(mean.relief, k=5, bs='cr'),
                family=poisson, random=~(1|site), 
                data=use.dat, 
                lme4=TRUE)

model.set <- generate.model.set(use.dat=use.dat,
                                test.fit=Model1,
                                pred.vars.cont=pred.vars,
                                pred.vars.fact=factor.vars,
                                smooth.smooth.interactions=FALSE,
                                max.predictors=3,
                                k=5,
                                null.terms = "planned.or.exploratory")


out.list=fit.model.set(model.set,
                       max.models=600,
                       parallel=T)

names(out.list)

# out.list=fit.model.set(model.set) # this line is different, 
fss.all=c(fss.all,list(out.list)) # new
mod.table=out.list$mod.data.out
mod.table=mod.table[order(mod.table$AICc),]
out.i=mod.table[which(mod.table$delta.AICc<=2),]
out.all=c(out.all,list(out.i))
var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),] # new term
top.all=c(top.all,list(all.less.2AICc)) # new term

# plot the all best models
par(oma=c(1,1,4,1))

for(r in 1:nrow(out.i)){
  best.model.name=as.character(out.i$modname[r])
  
  best.model=out.list$success.models[[best.model.name]]
  
  png(file=paste(name,r,resp.vars[i],"FH_mod_fits.png",sep="_"))
  if(best.model.name!="null"){
    plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16)
    mtext(side=3,text=resp.vars[i],outer=T)}
  dev.off()
}
}
dev.off()


# started running at 8:48 AM - FIN 9:43

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)

write.csv(all.mod.fits,file=paste(name,"FH_all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"FH_all.var.imp.csv",sep="_"))


############################################# Inside/Outside national park zone #############################################
# Remove any unused columns from the dataset 
dat <- maxn.io%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, maxn,
                "mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp",
                "aspect", "log.tpi","log.roughness","log.slope",
                "depth") %>%
  # dplyr::filter(scientific%in%c("total.abundance","species.richness",
  #                               "Sparidae Chrysophrys auratus", "Labridae Coris auricularis")) %>%
  as.data.frame()

# Only use if less than 80% zero
unique.vars=unique(as.character(dat$scientific))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$maxn==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

resp.vars <- unique.vars.use
factor.vars <- c("status")

out.all <- list()
var.imp <- list()
fss.all=list() # added from beckys example
top.all=list()# added from beckys example

for(i in 1:length(resp.vars)){
  
  use.dat <- dat[which(dat$scientific==resp.vars[i]),]
  
  Model1 <- uGamm(maxn~s(mean.relief, k=5, bs='cr'),
                  family=poisson, random=~(1|site), 
                  data=use.dat, 
                  lme4=TRUE)
  
  model.set <- generate.model.set(use.dat=use.dat,
                                  test.fit=Model1,
                                  pred.vars.cont=pred.vars,
                                  pred.vars.fact=factor.vars,
                                  smooth.smooth.interactions=FALSE,
                                  max.predictors=3,
                                  k=5,
                                  null.terms = "planned.or.exploratory")
  
  
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  
  names(out.list)
  
  # out.list=fit.model.set(model.set) # this line is different, 
  fss.all=c(fss.all,list(out.list)) # new
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),] # new term
  top.all=c(top.all,list(all.less.2AICc)) # new term
  
  # plot the all best models
  par(oma=c(1,1,4,1))
  
  for(r in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[r])
    
    best.model=out.list$success.models[[best.model.name]]
    
    png(file=paste(name,r,resp.vars[i],"IO_mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=3,text=resp.vars[i],outer=T)}
    dev.off()
  }
}
dev.off()

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars

# TEST 1 - no species. started @ 9:00 AM - failed at 9:21
# TEST 2 - only total abundance @ 9:21 - worked at 9:30
# test 3 - total and target abundance at 9:31
# test 4 - 80% zeros @ 11:09 AM - checked at 11:37

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)

write.csv(all.mod.fits,file=paste(name,"IO_all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"IO_all.var.imp.csv",sep="_"))
