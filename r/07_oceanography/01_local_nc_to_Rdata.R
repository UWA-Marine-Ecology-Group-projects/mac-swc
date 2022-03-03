###
# Project: mac- swc
# Data:    Oceanography - SST, SLA, currents & acidification
# Task:    Load in netCDF files from local copy
# author:  Jess Kolbusz & Claude
# date:    Feb 2022
##

#### --- FILES ARE TOO BIG FOR GITHUB
#### --- IN GITIGNORE
#### --- STORE LOCAL COPY IN ~data/oceanography/spatial/oceanography/large

# Clear memory----
rm(list=ls())
gc() #free unused memory

library(dplyr)
library(magrittr)
library(RNetCDF)
library(weathermetrics)
library(lubridate)

#set working directory
working.dir <- getwd()
setwd(working.dir)

## get data locations /limits that need from MPA
locations <-   read.csv("data/spatial/oceanography/network_scale_boundaries.csv", header = TRUE) %>%
  glimpse()

#i use the "zone" column for each since it distinguishes them all spatially
Zone <- 'SW Capes' #NW or SW
locs <- locations[locations$Zone %in% c(Zone), ]               # just wa parks nearby

#gets bounds
Lon_w <- locs$lon_w
Lon_e <- locs$lon_e
Lat_n <- locs$lat_n
Lat_s <- locs$lat_s

######### SEA LEVEL ANOMALY #########
#the numbers are just how the download from IMOS works - can rename the files if easier
#Altimeter and tidegauge estimates of adjusted sea level anomaly mapped onto a grid using optimal interpolation (OI)
#print.nc also shows reference for the data 
#IMOS - OceanCurrent - Gridded sea level anomaly - Delayed mode - DM01
#There is also this one you could look at - IMOS - OceanCurrent - Gridded sea level anomaly - Near real time
#but I looks like there is less QA/QC applied

nc_file_to_get_sla <- open.nc("data/spatial/oceanography/large/OceanCurrent_IMOS/IMOS_aggregation_20220210T021303Z.nc",write = TRUE)
print.nc(nc_file_to_get_sla) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_sla, 'TIME')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sla <- utcal.nc("days since 1985-01-01 00:00:00 UTC", time_nc,type = "c")
dates_sla <- as.Date(time_nc_sla)

lat <- var.get.nc(nc_file_to_get_sla, 'LATITUDE') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_sla, 'LONGITUDE')

#get lats of sst file which correspond to the lats of the zone
lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sst
sla_all <- var.get.nc(nc_file_to_get_sla,'GSLA', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla)));
# ucur_all <- var.get.nc(nc_file_to_get_sla,'UCUR', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla))); #sea level anomaly
# vcur_all <- var.get.nc(nc_file_to_get_sla,'VCUR', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sla))); #sea level anomaly

time_data <- list()
time_data$dates <- as.character(dates_sla)
time_data$month <- lubridate::month(as.POSIXlt(time_data$dates, format="%Y-%m-%d"))
time_data <- as.data.frame(time_data)

lat_sla <- check_lat
lon_sla <- check_lon

arr = array(sla_all, dim=c(length(lon_i),length(lat_i),length(time_data$dates)),
            dimnames = list(lon_sla, lat_sla,time_data$dates))

arr_long <- arr %>%
  reshape2::melt(varnames = c("Lon","Lat","Date"))

arr_long <- arr_long %>%
  dplyr::mutate(Date = as.Date(Date))%>%
  dplyr::mutate(year = year(Date),month = month(Date))%>%
  glimpse()

plot_sla_month <- arr_long %>% 
  group_by(month, Lon, Lat) %>% 
  summarise(sla = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>% 
  ungroup()%>%
  glimpse()

plot_sla_year <- arr_long %>% 
  group_by(year, Lon, Lat) %>% 
  summarise(sla = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>% 
  ungroup()%>%
  glimpse()

plot_sla_ts <- arr_long %>% 
  group_by(year,month, Lon, Lat) %>% 
  summarise(sla = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>% 
  ungroup()%>%
  glimpse()

saveRDS(plot_sla_ts, "data/spatial/oceanography/SwC_SLA_ts.rds")
saveRDS(plot_sla_month,"data/spatial/oceanography/SwC_SLA_month.rds")
saveRDS(plot_sla_year,"data/spatial/oceanography/SwC_SLA_year.rds")

#clear out the memory
rm(list= ls()[!(ls() %in% c('working.dir','locations', 'Zone','locs','Lon_w',
                            'Lon_e','Lat_n','Lat_s'))])
gc()

######### SST #########
#IMOS - SRS - SST - L3S - Single Sensor - 6 day - day and night time - Australia
nc_file_to_get_sst <- open.nc("data/spatial/oceanography/large/IMOS_aggregation_20220224T013630Z/IMOS_aggregation_20220224T013630Z.nc",write = TRUE)
print.nc(nc_file_to_get_sst) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_sst, 'time')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sst <- utcal.nc("seconds since 1981-01-01 00:00:00", time_nc,type = "c")
dates_sst <- as.Date(time_nc_sst)

lat <- var.get.nc(nc_file_to_get_sst, 'lat') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_sst, 'lon')

#get lats of sst file which correspond to the lats of the zone
lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sst
sst_all <- var.get.nc(nc_file_to_get_sst,'sea_surface_temperature', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_sst)));
sst_all <- kelvin.to.celsius(sst_all, round = 2) 

time_data <- list()
time_data$dates <- as.character(dates_sst)
time_data$month <- lubridate::month(as.POSIXlt(time_data$dates, format="%Y-%m-%d"))
time_data <- as.data.frame(time_data)
str(time_data)

lat_sst <- check_lat
lon_sst <- check_lon

arr = array(sst_all, dim=c(length(lon_i),length(lat_i),length(time_data$dates)),
            dimnames = list(lon_sst, lat_sst,time_data$dates))

arr_long <- arr %>%
  reshape2::melt(varnames = c("Lon","Lat","Date"))

arr_long <- arr_long %>%
  dplyr::mutate(Date = as.Date(Date))%>%
  dplyr::mutate(year = year(Date),month = month(Date))%>%
  glimpse()

plot_sst_winter <- arr_long %>% 
  dplyr::filter(month %in%c("7","8","9"))%>%
  group_by(year, Lon, Lat) %>% 
  summarise(sst = mean(value,na.rm = TRUE), sd = sd(value, na.rm = TRUE)) %>%
  ungroup()%>%
  glimpse()

plot_sst_year <- arr_long %>% 
  group_by(year, Lon, Lat) %>% 
  summarise(sst = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>%
  ungroup()%>%
  glimpse()

plot_sst_month <- arr_long %>% 
  group_by(month, Lon, Lat) %>% 
  summarise(sst = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>%
  ungroup()%>%
  glimpse()

plot_sst_ts <- arr_long %>% 
  group_by(year,month, Lon, Lat) %>% 
  summarise(sst = mean(value,na.rm = TRUE), sd = sd(value,na.rm = TRUE)) %>%
  ungroup()%>%
  glimpse()

saveRDS(plot_sst_winter,"data/spatial/oceanography/SwC_SST_winter.rds")
saveRDS(plot_sst_year,"data/spatial/oceanography/SwC_SST_year.rds")
saveRDS(plot_sst_month,"data/spatial/oceanography/SwC_SST_month.rds")
saveRDS(plot_sst_ts,"data/spatial/oceanography/SwC_SST_ts.rds")

#clear out the memory
rm(list= ls()[!(ls() %in% c('working.dir','locations', 'Zone','locs','Lon_w',
                            'Lon_e','Lat_n','Lat_s'))])
gc()

##### Acidification ####
#Ocean_acidification_historical_reconstructionfrom AODN portal
nc_file_to_get_acd <- open.nc("data/spatial/oceanography/large/acidification/IMOS_aggregation_20220217T050920Z.nc",write = TRUE)
print.nc(nc_file_to_get_acd) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_acd, 'TIME')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_acd <- utcal.nc("months since 1800-01-01 00:00:00", time_nc,type = "c")
dates_acd <- as.Date(time_nc_acd)

lat <- var.get.nc(nc_file_to_get_acd, 'LATITUDE') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_acd, 'LONGITUDE')

lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sea level anomalies
acd_all <- var.get.nc(nc_file_to_get_acd,'pH_T', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_acd))); #sea level anomaly

## get average plots - time series
acd_ts_all <- as.data.frame(dates_acd)

#different function to get mean since is only 2 in certain direction
acd_ts_all$acdd <-apply(acd_all, 2, mean, na.rm = TRUE) #for larger ares is in 3D so use apply(acd_all, 3, mean, na.rm = TRUE) #acd_all for monties is only 1 cell #for abrolhos -> apply(acd_all, 2, mean, na.rm = TRUE) #3 is 3rd dumension
acd_ts_all$month <- as.numeric(format(as.Date(acd_ts_all$dates_acd), "%m"))
acd_ts_all$year <- as.numeric(format(as.Date(acd_ts_all$dates_acd), "%Y"))

acd_ts_monthly <- acd_ts_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(acd_mean = mean(acdd, na.rm = TRUE), acd_sd = sd(acdd, na.rm = TRUE)) %>%
  glimpse()

## save acidification, don't need to get lat and lon for acd since is only time series 
saveRDS(acd_ts_monthly,"data/spatial/oceanography/SwC_acidification.rds")

##### -----DEGREE HEATING WEEKS ####
#download from
#https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
#input bounds and times
nc_file_to_get_dhw <- open.nc("data/spatial/oceanography/large/DHW_2021/dhw_5km_82f1_a212_461c.nc",write = TRUE)
print.nc(nc_file_to_get_dhw) #shows you all the file details

time_nc<- var.get.nc(nc_file_to_get_dhw, 'time')  #NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_dhw <- utcal.nc("seconds since 1970-01-01 00:00:00", time_nc,type = "c")
dates_dhw <- as.Date(time_nc_dhw)

lat <- var.get.nc(nc_file_to_get_dhw, 'latitude') #some latitude, some lat -> watch for spelling
lon <- var.get.nc(nc_file_to_get_dhw, 'longitude')

#get lats of sst file which correspond to the lats of the zone
lat_i <- which(lat <= Lat_n & lat >= Lat_s)
lon_i <- which(lon <= Lon_e & lon >= Lon_w)

#check values that taken out
check_lat <- lat[lat_i]
check_lat
check_lon <- lon[lon_i]
check_lon

#load only the subset of data
#get all sst
dhw_all <- var.get.nc(nc_file_to_get_dhw,'CRW_DHW', start = c(lon_i[1], lat_i[1],1), count = c(length(lon_i), length(lat_i), length(dates_dhw)));

time_data <- list()
time_data$dates <- as.character(dates_dhw)
time_data$month <- lubridate::month(as.POSIXlt(time_data$dates, format="%Y-%m-%d"))
time_data <- as.data.frame(time_data)

lat_dhw <- check_lat
lon_dhw <- check_lon

arr = array(dhw_all, dim=c(length(lon_i),length(lat_i),length(time_data$dates)),
            dimnames = list(lon_dhw, lat_dhw,time_data$dates))

arr_long <- arr %>%
  reshape2::melt(varnames = c("Lon","Lat","Date"))

arr_long <- arr_long %>%
  dplyr::mutate(Date = as.Date(Date))%>%
  dplyr::mutate(year = year(Date),month = month(Date))%>%
  glimpse()

plot_dhw_month <- arr_long %>% 
  group_by(month, Lon, Lat) %>% 
  summarise(dhw = mean(value,na.rm = TRUE)) %>% 
  ungroup()%>%
  glimpse()

plot_dhw_year <- arr_long %>% 
  group_by(year, Lon, Lat) %>% 
  summarise(dhw = mean(value,na.rm = TRUE)) %>% 
  ungroup()%>%
  glimpse()

saveRDS(plot_dhw_month,"data/spatial/oceanography/SwC_DHW_month.rds")
saveRDS(plot_dhw_year,"data/spatial/oceanography/SwC_DHW_year.rds")
