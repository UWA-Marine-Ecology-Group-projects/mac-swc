
### Error checks of MaxN and Length files created from EventMeasure or generic stereo-video annotations via GlobalArchive

## This script is designed to be used interatively to suggest species name and length corrections that should be made to original EventMeasure (.EMObs) or generic annotation files AND for subsequent data analysis.

# NOTE: ERRORS SHOULD BE FIXED IN THE .EMObs AND RE-UPLOADED TO GLOBAL ARCHIVE!


### OBJECTIVES ###
# 1. Import data and run BASIC error reports
# 2. Limit length data by range and precision rules
# 3. run SERIOUS error reports against a master species list
# 4. Visualise what MaxN are missing in the stereoMaxN
# 5. Write data for analysis that passes checks

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

# Please email tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au if you would like the life.history or synonyms googlesheets shared with you or to have your local species information added.


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
library(readr)
library(ggplot2)
library(stringr)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"2020-2021_south-west_BOSS"

## Folder Structure ----
# This script uses one main folder ('working directory')

# Three subfolders will already be created within the 'working directory'. They are 'Downloads','Data to be checked' and 'Tidy data' (Script 1)

# In addition, this script will make new folders for:
#'Plots' to save initial checking plots
#'Errors to check' to save all the error files e.g. lists of taxa that are not in the life history sheet

## Set your working directory ----
working.dir <- getwd() # sets working directory to that of this script - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"data",sep="/")
plots.dir<-paste(working.dir,"plots",sep="/")
download.dir<-paste(data.dir,"raw",sep="/")

to.be.checked.dir<-paste(data.dir,"staging",sep="/") 
tidy.dir<-paste(data.dir,"tidy",sep="/")
error.dir=paste(data.dir,"errors to check",sep="/")

## Create a folder for Plots and Errors ----
# The two lines below will create the 'Plots' and 'Errors to check' subfolders within the working directory
# dir.create(file.path(working.dir, "Plots"))
# dir.create(file.path(data.dir, "Errors to check"))

# Import unchecked data from staging folder----
setwd(to.be.checked.dir)

# Import metadata ---
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::mutate(species=tolower(species))%>%
  dplyr::select(campaignid,sample,family,genus,species,maxn)%>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>%           # remove any NAs in taxa name
  # dplyr::mutate(genus=ifelse((family%in%c("Carangidae")&species%in%c("sp10")),
  # "Pseudocaranx",genus)) %>%
  # dplyr::mutate(species=ifelse((species%in%c("sp10")),"spp",species)) %>%
  dplyr::mutate(genus=ifelse((genus%in%c("Paraquula")),"Parequula",genus)) %>%
  dplyr::mutate(id=paste(campaignid,sample,sep = " "))%>%
  dplyr::glimpse()

# Check that there is no fish with family unknown
length(unique(maxn$id))                                                         #264 - minus those drops with zero fish

#no length data

# SERIOUS data checks using the life.history googlesheet ----
# Checks on fish length vs their max.length in the life.history sheet will be done below

# life.history checks will:
# 1. Check for species occurence vs their known distribution
# 2. Check for any species that may have changed names and suggest synonyms
# 3. Check measured length vs max.length for that species

# Make sure to select the correct Country and Marine Region that matches your data (see the two filter lines below)
# Follow this link to see a map of the marine regions used in the life history sheet
#  https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions

# These Marine Region abbreviations are:
# 'SW' - South-west
# 'NW' - North-west
# 'N' - North
# 'CS' - Coral Sea
# 'TE' - Temperate East
# 'SE' - South-east
# 'Christmas.Island' - Christmas Island
# 'Cocos.Keeling' - Cocos (Keeling) Island
# 'Lord.Howe.Island' - Lord Howe Island

# Use the abbreviation in the code below
setwd(tidy.dir)
dir()
library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>% 
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>%                           # Change country here
  dplyr::filter(grepl('SW', marine.region))%>%                                  # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

synonymsurl <- "https://docs.google.com/spreadsheets/d/1R0uU9Q0VkUDQFgGTK3VnIGxmc101jxhlny926ztWoiQ/edit#gid=567803926"

synonyms<- googlesheets4::read_sheet(synonymsurl)%>% 
  distinct()%>%
  ga.clean.names()%>%
  dplyr::select(-comment)

# Update by synonyms ----
# This function will change the names of species that have been reclassified (i.e. Pagrus auratus to Chrysophrys auratus). This function also fixes some common spelling mistakes (i.e. Chyrosophyrs	auratus to Chrysophrys auratus)

# Use return.changes=T to view the taxa.names.updated
# Use save.report to save .csv file in your error directory
maxn<-ga.change.synonyms(maxn,return.changes=T,save.report = T)

# Check MaxN for species that have not previously been observed in your region ----
maxn.species.not.previously.observed<-master%>%
  dplyr::anti_join(maxn,.,by=c("family","genus","species"))%>% 
  dplyr::distinct(campaignid,sample,family,genus,species)%>%                    # use this line to show specific drops OR
  #distinct(family,genus,species)%>%                                            # use this line to keep only fam, gen, spe
  dplyr::filter(!species%in%c("spp","sp1","sp","sp10","sus"))%>%                # Ignore spp in the report
  dplyr::glimpse()

setwd(error.dir)
write.csv(maxn.species.not.previously.observed,
          file=paste(study,"maxn.species.not.previously.observed.csv",sep = "."), 
          row.names=FALSE)

# WRITE FINAL checked data----
setwd(tidy.dir)
dir()

write.csv(metadata, file=paste(study,"checked.metadata.csv",sep = "."), row.names=FALSE)
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)

setwd(working.dir)
# Go to FORMAT script (3) 
