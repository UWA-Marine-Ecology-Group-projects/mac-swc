require(rstanarm)
require(tidyverse)
require(dplyr)
require(mgcv)
require(FSSgam)
require(MuMIn)
require(doBy)
require(GlobalArchive)
require(googlesheets4)

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

# Length ----
length <-read.csv(paste(name, 'complete.length.csv',sep=".")) %>%
  dplyr::select(campaignid, sample, length, number, family, genus, species) %>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" ")) %>%
  dplyr::glimpse()

unique(length$sample)

test <- length %>%
  filter(number>0)%>%
  distinct(sample)

total.no.pinkies <- length %>%
  dplyr::filter(species=="auratus") %>%
  filter(number>0)

sum(total.no.pinkies$number) # 196
test <- total.no.pinkies %>%
  filter(length>0)
sum(test$number) # 168 measured

168/196*100 # 85% measured

# Metadata ----
metadata <- read.csv(paste(name, 'checked.metadata.csv',sep=".")) %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv(paste(name, 'bathymetry.derivatives.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHCO1"="FHC01","FHCO2"="FHC02","FHCO3"="FHC03"))) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHCO1"="FHC01","FHCO2"="FHC02","FHCO3"="FHC03"))) %>%
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
  #dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# merge length with list of recreational fished species ----
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
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

unique(master$fishing.type)

spp.species<-length%>%
  filter(species=="spp")%>%
  distinct(scientific,family,genus,species)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Carangidae Unknown spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Scombridae Sarda spp",
                                                       "Scombridae Unknown spp",
                                                       "Sillaginidae Sillago spp"),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>%
  dplyr::filter(!species%in%c("nigricans","lineolatus","cirratus"))%>% # Brooke removed dusky morwong, maori wrasse, common saw shark
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) %>% # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Carangidae Pseudocaranx spp"),250,minlegal.wa))%>%
  dplyr::mutate(minlegal.wa=ifelse(scientific%in%c("Platycephalidae Platycephalus spp"),300,minlegal.wa))
  
without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # Checked all of these with rec fish app - all don't have one

# Come back to maybe getting rid of some of these, but for now we continue on
legal <- fished.species %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

pinksnapper.legal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size pink snapper") %>%
  dplyr::glimpse()

pinksnapper.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("auratus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size pink snapper") %>%
  dplyr::glimpse()

fished.bigger.20cm <- fished.species %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 20 cm") %>%
  dplyr::glimpse()

fished.bigger.30cm <- fished.species %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "fished greater than 30 cm") %>%
  dplyr::glimpse()

all.bigger.20cm <- length %>%
  dplyr::filter(length>200) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 20 cm") %>%
  dplyr::glimpse()

all.bigger.30cm <- length %>%
  dplyr::filter(length>300) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "all greater than 30 cm") %>%
  dplyr::glimpse()

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
combined.length <- bind_rows(legal, sublegal, fished.bigger.20cm, fished.bigger.30cm, all.bigger.20cm, all.bigger.30cm, 
                             pinksnapper.legal, pinksnapper.sublegal) # add pink snapper and other indicator species

unique(combined.length$scientific)
  
complete.length <- combined.length %>%
  #dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(campaignid,sample,scientific,number) %>%
  tidyr::complete(nesting(campaignid,sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,bathy) %>%
  dplyr::left_join(.,ramps) %>%
  dplyr::left_join(.,habitat) %>%
  dplyr::filter(successful.length%in%c("Yes")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

unique(complete.length$scientific)

length.fh <- complete.length %>%
  semi_join(., metadata.fh) %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 12)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))

length.io <- complete.length %>%
  semi_join(., metadata.io) %>%
  dplyr::mutate(log.sponges = log(broad.sponges + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 12)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(log.slope = log(slope + 1))


unique(length.io$scientific)

### FISHING HWY #####

# Remove any unused columns from the dataset 
dat <- length.fh%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, number,
                "mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp",
                "aspect", "log.tpi","log.roughness","log.slope",
                "depth") %>%
  dplyr::filter(!scientific%in%c("fished greater than 20 cm","fished greater than 30 cm")) %>%
  as.data.frame()

unique(dat$scientific)

unique.vars=unique(as.character(dat$scientific))
unique.vars



unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use  

resp.vars <- unique.vars.use
factor.vars <- c("status")

out.all <- list()
var.imp <- list()
fss.all=list() # added from beckys example
top.all=list()# added from beckys example


# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
            "distance.to.ramp",
            "aspect", "log.tpi","log.roughness","log.slope",
            "depth")

setwd(m.dir)

for(i in 1:length(resp.vars)){
  
  use.dat <- dat[which(dat$scientific==resp.vars[i]),]
  
  Model1 <- uGamm(number~s(mean.relief, k=5, bs='cr'),
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
    
    png(file=paste(name,r,resp.vars[i],"FH_length_mod_fits.png",sep="_"))
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

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)

write.csv(all.mod.fits,file=paste(name,"FH_length_all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"FH_length_all.var.imp.csv",sep="_"))

### IN/OUT #####

# Remove any unused columns from the dataset 
dat <- length.io%>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, number,
                "mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp",
                "aspect", "log.tpi","log.roughness","log.slope",
                "depth") %>%
  dplyr::filter(!scientific%in%c("fished greater than 20 cm","fished greater than 30 cm")) %>%
  as.data.frame()

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use  

resp.vars <- unique.vars.use
factor.vars <- c("status")

out.all <- list()
var.imp <- list()
fss.all=list() # added from beckys example
top.all=list()# added from beckys example


# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","log.sponges","broad.macroalgae","broad.reef",
            "distance.to.ramp",
            "aspect", "log.tpi","log.roughness","log.slope",
            "depth")

setwd(m.dir)

for(i in 1:length(resp.vars)){
  
  use.dat <- dat[which(dat$scientific==resp.vars[i]),]
  
  Model1 <- uGamm(number~s(mean.relief, k=5, bs='cr'),
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
    
    png(file=paste(name,r,resp.vars[i],"IO_length_mod_fits.png",sep="_"))
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

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)

write.csv(all.mod.fits,file=paste(name,"IO_length_all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"IO_length_all.var.imp.csv",sep="_"))
