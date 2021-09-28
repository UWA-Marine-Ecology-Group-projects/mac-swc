rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(raster)
library(patchwork)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(visreg)
library(mgcv)
install.packages("sp")

# clear workspace ----
rm(list = ls())

# Set the study name
name <- '2020_south-west_stereo-BRUVs' # for the study

# set working directories ----
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

w.dir <- "Z:/SWC-multibeam-GAMMS"
# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
h.dir <- (paste(w.dir, "Data/Habitat/BRUV Style annotation/tidy data"))
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')

## put script for uploading data and running gam here

# Bring in and format the raw data----

setwd(dt.dir)
dir()

# Load the dataset -
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

# Bathymetry derivatives ----   Em change here 
bathy <- read.csv("2020_sw_maxn.env-cov.csv") %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::filter(!flowdir=="NA")%>%
  dplyr::select(sample, depth.1, slope, aspect, roughness, tpi, flowdir, SSTmean_SSTARRS,SSTsterr_SSTARRS, SSTtrend_SSTARRS)%>%
  unique()%>%
  dplyr::glimpse()


# Distance to boat ramp ----
ramps <- read.csv(paste(name, 'distance.to.ramp.csv',sep=".")) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c("FHC01"="FHCO1","FHC02"="FHCO2","FHC03"="FHCO3"))) %>%
  dplyr::glimpse()

test<-left_join(metadata,ramps)

ggplot() + geom_point(data=test,aes(y=longitude,x=latitude, col=distance.to.ramp),  alpha=0.75, size=1,show.legend=TRUE)

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
setwd(dt.dir)
dir()

# Select species of interest to model ----
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Sparidae Chrysophrys auratus",
                                  "Labridae Coris auricularis",
                                  "Heterodontidae Heterodontus portusjacksoni",
                                  "Monacanthidae Nelusetta ayraud"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()



## Combine all the maxn data to be modeled into a single data frame
combined.maxn <- bind_rows(species.maxn, 
                           ta.sr)%>%
  left_join(metadata) %>%
  left_join(bathy) %>%
  left_join(ramps) %>%
  left_join(habitat) %>%
  filter(!tpi=="NA")%>%  # removes all drops with no multibeam data
  distinct()%>%
  glimpse()

# transformations for modelling ----

maxn.fh <- combined.maxn %>%
  dplyr::mutate(log.sd.relief = log(sd.relief + 1)) %>%
  dplyr::mutate(log.tpi = log(tpi + 2)) %>%
  dplyr::mutate(log.roughness = log(roughness + 1)) %>%
  dplyr::mutate(sqrt.slope = sqrt(slope))%>%
  filter(!scientific%in%c("Monacanthidae Nelusetta ayraud"))%>%
  glimpse()  

# remove outlier 

plot(maxn.fh$maxn)

maxn.fh <- maxn.fh %>%
  filter(maxn<600)%>%
  glimpse()

# Load Multibeam ----

b <- raster(paste(r.dir, "SwC_Multibeam.tiff", sep='/'))
plot(b)

# crop to extent --
#e <- drawExtent()
e <- extent(288664.7 , 311265.2 , 6220416 , 6234275 )
b <- crop(b, e)
plot(b)
b # 4x4m resolution

#### Transform from utm to lat long ----

# open reference file 
ref <- raster(paste(r.dir, "GB-SW_250mBathy.tif", sep='/'))
ref.crs <- proj4string(ref)

b <- projectRaster(b, crs = ref.crs)
plot(b)
proj4string(b) # check it worked


# Calculate bathy derivatives ----
s <- terrain(b, 'slope')
plot(s)
a <- terrain(b, 'aspect')
plot(a)
r <- terrain(b, 'roughness')
plot(r)
t <- terrain(b, 'TPI')
plot(t)
f <- terrain(b, 'flowdir')
plot(f)

ders <- stack(b,s,a,r,t,f)
names(ders) <- c("depth", "slope",  "aspect" ,  "roughness"  ,   "tpi" ,   "flowdir")
ders
plot(ders$depth)

# transform these to match models ----

ders$log.roughness <-log(ders$roughness + 1)
ders

# to select a single layer form this stack 
rough <- raster(ders, layer=4)
rough

log.rough <- raster(ders, layer=7)# looks legit.
log.rough

## Want to predict to 'average' site not individual sites
## Take out site random effect
## Do this by creating raster of zeros and calling it site

rzeros <- b
rzeros[] <- 0

rstack <- stack(ders, rzeros)
rstack

names(rstack) <- c("depth", "slope", "aspect", "roughness", "tpi", 
                   "flowdir", "log.roughness", "site")

rstack


## Prediction for species richness ---- 

# top model = sqrt slope + sd relief 

# transformation for model: 

rstack$sqrt.slope <- sqrt(rstack$slope)

# GAMM Species richness ----

dat.sr <- maxn.fh %>% filter(scientific=="species.richness")
colnames(dat.sr)

gamm=gam(maxn~s(sqrt.slope,k=3,bs='cr') + s(site,bs="re"), family=poisson,data=dat.sr)

mod <- gamm
summary(mod)
par(mfrow = c(2,2))
gam.check(mod)
visreg(mod)

gam.vcomp(mod)


# create predictions ----
sr.pred <- predict(rstack, mod, se=TRUE)

par(mfrow = c(1,1))
plot(sr.pred)



# Righto, but what about error? and for model terms not in all multibeam area?

## Kingsley's code below: ----

## alternative method using data frame instead of raster
# as.data frame with xy=TRUE
newrastdat <- as.data.frame(rstack, xy = TRUE, na.rm = TRUE) # convert raster to df, keeping cell coordinates
head(newrastdat) 

# compare range of rasters with range of training data
summary(newrastdat)
summary(dat.sr)

# trim out raster values outside of range
newrastdat <- newrastdat[newrastdat$slope <= max(dat.sr$slope), ]

# predict to new dat
rpreds <- predict(mod, newrastdat, type= "response", se.fit = TRUE)
glimpse(rpreds)

preds <- unlist(rpreds$fit) # the predict function outputs a list - make df
pse   <- unlist(rpreds$se.fit)

head(preds.df)

sp_preds <- cbind(preds, pse, newrastdat) # combine back with the raster data
p_rast   <- rasterFromXYZ(cbind(sp_preds$x, sp_preds$y, sp_preds$preds)) # make prediction raster
p_se     <- rasterFromXYZ(cbind(sp_preds$x, sp_preds$y, sp_preds$pse))   # make se raster
plot(stack(p_rast, p_se))

summary(dat.sr$maxn)

final.preds <- stack(p_rast, p_se)

names(final.preds) <- c("sprichness", "se")

final.preds

setwd(r.dir)
dir()

# save stack of derivatives
writeRaster(final.preds, "sr_predictions.tif", sep='/', overwrite=T)

## Creating columns for bivartiate colour scheme ----
## Create new column in raster with A, B , C and 1, 2, 3 groupings

# ifelse  for preds in preds df  # try sp_preds df with ceiling to round up 
head(sp_preds)
str(sp_preds)


sp_preds$preds<- as.numeric(sp_preds$preds)
summary(sp_preds$preds)
#preds.df$preds.2<- ceiling(preds.df$preds) # round up for ifelse
#glimpse(preds.df$preds.2)

sp_preds$preds.r<- ceiling(sp_preds$preds) # round up for ifelse
glimpse(sp_preds)

preds_letter <- sp_preds %>%
  dplyr::mutate(Var1 = ifelse(preds.r %in% 9:12, "A",
                           ifelse(preds.r %in% 13:16, "B",
                              ifelse(preds.r %in% 17:20, "C","D"))))%>%
  glimpse()

# ifelse  for se in preds_letter df

preds_letter$pse.r<- ceiling(preds_letter$pse) # round up for ifelse
glimpse(preds_letter)

preds_letters <- preds_letter %>%
  dplyr::mutate(Var2 = ifelse(pse.r %in% 0:1, "1",
                              ifelse(pse.r %in% 1:2, "2",
                                     ifelse(pse.r %in% 2:3, "3","4"))))%>%
  glimpse()

## combine the Var1 and Var2 column

preds_letters$Var3 <- paste(preds_letters$Var1,preds_letters$Var2,sep="")
glimpse(preds_letters) #yew

preds_letters$Var3 <- as.factor(preds_letters$Var3)  

## convert into a raster with Var3


p_rast   <- rasterFromXYZ(cbind(preds_letters$x, preds_letters$y, preds_letters$preds)) # make prediction raster
p_se     <- rasterFromXYZ(cbind(preds_letters$x, preds_letters$y, preds_letters$pse))   # make se raster
p_var3   <- rasterFromXYZ(cbind(preds_letters$x, preds_letters$y, preds_letters$Var3))
plot(stack(p_rast, p_se, p_var3))

## After lunch code to tweek

final.preds.bivariate <- stack(p_rast, p_se, p_var3)

names(final.preds.bivariate) <- c("sprichness", "se", "bivariatecodes")

final.preds.bivariate

setwd(r.dir)
dir()

# save stack of derivatives
writeRaster(final.preds.bivariate , "sr_predictions_code.tif", sep='/', overwrite=T)


# Not quite what we want. Create shapefile in separate script
