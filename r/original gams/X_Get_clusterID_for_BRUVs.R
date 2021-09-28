### script to get cluster id for each BRUV ####



library(ggplot2)
library(tidyr)
library(dplyr)
library(rstudioapi)
library(stringr)
library(sf)
library(rgdal)
library(sp)
library(raster)
library(viridis)
library(RColorBrewer)

# clear workspace ----
rm(list = ls())


## Set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <-  paste(w.dir, "Data/Tidy", sep='/')
s.dir <- paste(w.dir, 'shapefiles', sep='/')



## Load data ----
file.name <- "2020_south-west_stereo-BRUVs.complete.maxn.csv"

df <- read.csv(paste(dt.dir, file.name, sep='/')) %>% 
  glimpse()

# prepare df --
df <- df %>% 
  unite(unique.name, c(family, genus, species), sep = " ", remove = FALSE) %>% # make full name
  unite(full.name, c(genus, species), sep = " ", remove = FALSE) %>% # make full name
  mutate_at(vars(campaignid, sample, unique.name, full.name), list(~as.factor(.))) %>% 
  glimpse()

str(df)


# add cluster numbers ----

# check cluster data ---

# open fish highway clusters and bruvs --
fw <- readOGR(paste(s.dir, "planned-SW-Bruvs-Oct2020", "BruvNcluster-FHWY-d7.shp", sep='/'))
plot(fw, col='red')
fwd <- as.data.frame(fw)
head(fwd)
str(fwd) # 114 clusters

fwd <- fwd %>%
  mutate_at(vars(ID, n), list(~as.factor(.))) %>%
  dplyr::rename(number = UniqueID, cluster = ID) %>%
  dplyr::mutate(sample = paste("FH", number, sep='')) %>% # create sample that includes which dataset it comes from
  # LATER ALSO INCLUDE CAPTAINS PICKs!
  dplyr::mutate(dataset = "FH") %>% # create column for dataset factor : fish highway or in and out.
  dplyr::mutate(class = "MBH") %>% # create column for class: MBH, legacy
  mutate_at(vars(sample, dataset), list(~as.factor(.))) %>% # make new columns as factors
  glimpse() # check n is only one level of factor

# check and rename clusters
levels(fwd$cluster)
cluster <- levels(fwd$cluster)
cluster.new <- paste(paste0('C.FH.',1:24), sep = ',')
cluster.levs <- as.data.frame(cbind(cluster, cluster.new))
str(cluster.levs)
cluster.levs$cluster <- as.factor(cluster.levs$cluster)
cluster.levs$cluster.new <- as.factor(cluster.levs$cluster.new)

# merge with fish hwy data --
fwd2 <- merge(cluster.levs, fwd, by = 'cluster')
head(fwd2)
str(fwd2) # 114 obs


# open in and out clusters and bruvs --
io <- readOGR(paste(s.dir, "planned-SW-Bruvs-Oct2020", "BruvNclusters-InNOutMP-d3.shp", sep ='/'))
plot(io, add=T)
iod <- as.data.frame(io)
head(iod)
str(iod) # 397 bruvs 

#define unwanted levels of n
unwanted <- c(2,3)

iod <- iod %>% 
  dplyr::select(class, BRUVid, clusterx, clustery, clusterID, clustercla, n, feature_x, feature_y, nearest_x, nearest_y, coords.x1, coords.x2) %>%
  mutate_at(vars(class, BRUVid, clusterID, clustercla, n), list(~as.factor(.))) %>%
  dplyr::rename(number = BRUVid, cluster = clusterID, cluster.class = clustercla) %>%
  dplyr::filter(!as.integer(n) %in% unwanted) %>% # remove unwanted levels of n -- this is from Qgis, when getting the cluster no. for each bruv
  dplyr::mutate(sample = paste("IO", number, sep='')) %>% # create sample that includes which dataset it comes from
  # LATER ALSO INCLUDE CAPTAINS PICKs!
  dplyr::mutate(dataset = "IO") %>% # create column for dataset factor : fish highway or in and out.
  mutate_at(vars(sample, dataset), list(~as.factor(.))) %>% # make new columns as factors
  glimpse() 

str(iod) # now 376 bruvs and n has only 1 level, this means only one cluster number per BRUV

levels(iod$cluster)
cluster <- levels(iod$cluster)
cluster.new <- paste(paste0('C.IO.',1:70), sep = ',')
cluster.levs <- as.data.frame(cbind(cluster, cluster.new))
str(cluster.levs)
cluster.levs$cluster <- as.factor(cluster.levs$cluster)
cluster.levs$cluster.new <- as.factor(cluster.levs$cluster.new)

# merge with in and out data --
iod2 <- merge(cluster.levs, iod, by = 'cluster', all = T)
head(iod2)
str(iod2) # 376 obs

# join fish highway and in and out data ----

# make sure they have the same columns --
names(fwd2)

fhw <- fwd2 %>% 
  dplyr::select(cluster, cluster.new, number, n, coords.x1, coords.x2, sample, dataset, class) %>% 
  glimpse()

names(iod2)

ino <- iod2 %>%
  dplyr::select(cluster, cluster.new, number, n, coords.x1, coords.x2, sample, dataset, class)%>% 
  glimpse()

# Join
planned.sw <- rbind(fhw, ino)
str(planned.sw) # 490 obs

#plot to check
p <- planned.sw
coordinates(p) <- ~coords.x1+coords.x2
plot(p)


# SAVE new metadata of all SW Bruvs including associated cluster ----
#write.csv(planned.sw, paste(dt.dir, "2020_south-west_stereo_BRUVs.metadata.clusters.csv", sep='/'))