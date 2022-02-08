
### Make complete.maxn and complete.length.number.mass data from Checked.maxn and Checked.length data created from EventMeasure or generic stereo-video annotations via GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons


### OBJECTIVES ###
# 1. Import checked data
# 2. Make factors
# 3. Make complete.maxn long.format data with zeros filled in:
      ## PeriodTime will represent the first PeriodTime of MaxN if PeriodTime has been set to zero at Time on Seabed in EM.
      ## complete.maxn data is useful for species and abundance metrics - that do not account for body size or range/sample unit size
# 4. Make complete.length.number.mass data with zeros filled in:
      ## useful for calculating abundance/mass based on length rules (e.g. greater than legal)
      ## useful for controling for range/sample unit size
      ## useful for length analyses (e.g. mean length, KDE, histograms) - after expansion by number of lengths per sample per species - see example below
# 5. Make mass estimates from Length using a and b from life.history
# 6. Write complete data sets for further analysis


### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(fst)

# Study name---
study<-"2020-2021_south-west_BOSS"

## Set your working directory ----
working.dir <- getwd() # to directory of current file - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"data",sep="/")
plots.dir<-paste(working.dir,"plots",sep="/")
download.dir<-paste(data.dir,"raw",sep="/")

to.be.checked.dir<-paste(data.dir,"staging",sep="/") 
tidy.dir<-paste(data.dir,"tidy",sep="/")
error.dir=paste(data.dir,"errors to check",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Read in metadata----
metadata<-read_csv(file=paste(study,"checked.metadata.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::glimpse()

length(unique(metadata$id))                                                     # 279 

samples <- metadata %>%                                                         #get all samples so we can keep in 0s
  distinct(id,sample)

# Make complete.maxn: fill in 0s and join in factors----
dat<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::select(c(id,campaignid,sample,family,genus,species,maxn))%>%
  dplyr::full_join(samples)%>%
  tidyr::complete(nesting(id,campaignid,sample),nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0))%>%
  dplyr::group_by(id,sample,family,genus,species)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>% #always a good idea to ungroup() after you have finished using the group_by()!
  dplyr::mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::select(id,sample,scientific,maxn)%>%
  spread(scientific,maxn, fill = 0)%>% 
 # dplyr::mutate(sample=ifelse(is.na(sample),id,sample))%>%
  dplyr::glimpse()

length(unique(dat$id))                                                          #good

# Make family, genus and species names to merge back in after data is complete ---
maxn.families<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::filter(!(family=="Unknown"))%>%
  dplyr::select(c(family,genus,species,scientific))%>%
  dplyr::distinct()%>% #to join back in after complete
  glimpse()

# Make complete data and join with metadata
complete.maxn<-dat%>%
  gather(key=scientific, value = maxn,-sample)%>%
  dplyr::inner_join(maxn.families,by=c("scientific"))%>%
  dplyr::left_join(metadata)%>% # Joining metadata will use a lot of memory - # out if you need too
  dplyr::glimpse()

length(unique(complete.maxn$sample))

# WRITE FINAL complete and expanded data----
setwd(tidy.dir)
dir()

write.csv(complete.maxn, file=paste(study,"complete.maxn.csv",sep = "."), row.names=FALSE)

setwd(working.dir)
