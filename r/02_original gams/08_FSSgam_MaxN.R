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
library(googlesheets4)

rm(list=ls())

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

## Set working directory----
working.dir <- getwd()
setwd(working.dir)

# Bring in and format the data----
# MaxN ----
maxn <-read.csv("data/tidy/2020_south-west_stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, scientific, maxn, family, genus, species) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Metadata ----
metadata <- read.csv("data/tidy/2020_south-west_stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(status = as.factor(status)) %>%
  dplyr::mutate(sample = as.factor(sample)) %>%
  dplyr::mutate(planned.or.exploratory = as.factor(planned.or.exploratory)) %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::filter(successful.count%in%c("Yes")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Bathymetry derivatives ----
bathy <- read.csv('data/tidy/2020_south-west_stereo-BRUVs.bathymetry.derivatives.csv') %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Distance to boat ramp ----
ramps <- read.csv('data/tidy/2020_south-west_stereo-BRUVs.distance.to.ramp.csv') %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

# Habitat ----
habitat.2020.10 <- read.csv("data/tidy/2020-10_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count,habitat.backwards.image.saved)) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.10)

habitat.2020.06 <- read.csv("data/tidy/2020-06_south-west_stereo-BRUVS_random-points_broad.habitat.csv") %>%
  dplyr::select(-c(latitude,longitude,date,time,site,location,successful.count)) %>%
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::glimpse()

summary(habitat.2020.06) # 0-100

habitat <-bind_rows(habitat.2020.06, habitat.2020.10) %>%
  tidyr::replace_na(list(broad.consolidated=0,
                         broad.macroalgae=0,
                         broad.seagrasses=0,
                         broad.sponges=0,
                         broad.unconsolidated=0,
                         broad.bryozoa=0,
                         broad.hydroids=0,
                         broad.octocoral.black=0,
                         broad.stony.corals=0,
                         fov.facing.up=0,
                         broad.ascidians=0,
                         broad.true.anemones=0,
                         broad.crinoids=0)) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.reef = broad.bryozoa + broad.consolidated + broad.hydroids + broad.macroalgae + broad.octocoral.black + broad.seagrasses + broad.sponges + broad.stony.corals) %>%
  dplyr::mutate(broad.ascidians = broad.ascidians/broad.total.points.annotated,
                broad.bryozoa = broad.bryozoa/broad.total.points.annotated,
                broad.consolidated = broad.consolidated/broad.total.points.annotated,
                broad.crinoids = broad.crinoids/broad.total.points.annotated,
                broad.hydroids = broad.hydroids/broad.total.points.annotated,
                broad.invertebrate.complex = broad.invertebrate.complex/broad.total.points.annotated,
                broad.macroalgae = broad.macroalgae/broad.total.points.annotated,
                broad.octocoral.black = broad.octocoral.black/broad.total.points.annotated,
                broad.reef = broad.reef/broad.total.points.annotated,
                broad.seagrasses = broad.seagrasses/broad.total.points.annotated,
                broad.sponges = broad.sponges/broad.total.points.annotated,
                broad.stony.corals = broad.stony.corals/broad.total.points.annotated,
                broad.true.anemones = broad.true.anemones/broad.total.points.annotated,
                broad.unconsolidated = broad.unconsolidated/broad.total.points.annotated)%>%
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
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
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
## Total frequency of occurrence
# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  dplyr::top_n(10)%>%
  ungroup()
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",               #not many snapper will likely get excluded
                                  "Labridae Coris auricularis",
                                  "Scorpididae Neatypus obliquus",
                                  "Pomacentridae Chromis klunzingeri"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()%>%
  glimpse()

test.samples <- species.maxn %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n())

unique(species.maxn$scientific)

# 311 samples x 7 species
311*7

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

## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(fished.maxn, species.maxn, 
                           ta.sr, centroberyx)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  distinct()

unique(combined.maxn$scientific)
11*311 # when specific species are included

length(unique(combined.maxn$sample)) # 311

# Set predictor variables---
pred.vars=c("depth", "slope", "aspect", "roughness", "tpi", "distance.to.ramp", "broad.bryozoa",
            "broad.consolidated", "broad.hydroids", "broad.macroalgae", "broad.octocoral.black", 
            "broad.reef", "broad.seagrasses", "broad.sponges", "broad.stony.corals", "mean.relief", "sd.relief", "broad.unconsolidated")
dat <- combined.maxn

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars], use = "complete.obs"),2)
#slope and roughness
#unconsolidated and reef
par(mfrow=c(1,1))
plot(dat$maxn)

# remove 2 samples with maxn >400? Prob an outlier and driving a lot of relationships
outlier <- dat %>%
  filter(maxn>400)%>%
  glimpse()
 
dat <- dat %>%
  filter(maxn <400)%>%
  glimpse()

plot(dat$maxn)

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# use 
# sd.relief - use non-transformed
# mean.relief - use non-transformed
# sponges - log             -----Claude thinks maybe leave as non-transformed?
# macroalgae - use non-transformed
# distance.to.ramp - use non-transformed
# aspect - use non-transformed
# depth - use non-transformed
# broad.reef - use non-transformed
# tpi - log               ---maybe not bother? but there is one outlier
# roughness log            ---???

# remove
# sand - correlated with reef and mean relief
# slope - correlated with roughness
# stony corals - too few
# seagrasses - too few
# octocoral - too few
# hydroids - too few
# consolidated - too few
# bryozoa - too few

# Set predictor variables 
pred.vars=c("mean.relief","sd.relief","broad.sponges","broad.macroalgae","broad.reef",
            "distance.to.ramp","aspect", "tpi","roughness","depth")

# Remove any unused columns from the dataset
dat <- dat %>%
  dplyr::select(sample, status, site, planned.or.exploratory, scientific, maxn,
                "mean.relief","sd.relief","broad.sponges","broad.macroalgae","broad.reef",
                "distance.to.ramp","aspect", "tpi","roughness","depth") %>%
  dplyr::filter(!sample%in%c("IO267"))%>%   #remove one weird TPI value (-11) come back to try and check on it
  as.data.frame()

unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$maxn==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use

# Run the full subset model selection----
savedir <- "output/fish gamms"
resp.vars=unique.vars.use
use.dat=as.data.frame(dat)
str(use.dat)

factor.vars=c("status")# Status as a Factor with two levels
cyclic.vars=c("aspect")
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$scientific==resp.vars[i]),])
  Model1=gam(maxn~s(depth,k=3,bs='cr') + 
             s(site,bs='re'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               factor.smooth.interactions = FALSE,
                               # smooth.smooth.interactions = c("depth"),
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               cyclic.vars = cyclic.vars,
                               #linear.vars="depth",
                               k=3,
                                null.terms="s(site ,bs='re')"
  )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
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