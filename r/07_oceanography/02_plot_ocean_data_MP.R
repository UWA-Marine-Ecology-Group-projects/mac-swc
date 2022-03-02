###
# Project: mac - swc
# Data:    Oceanography - SST, SLA, currents & acidification
# Task:    Plot oceanography trends
# author:  Jess Kolbusz & Claude
# date:    Feb 2022
##

# Clear memory----
rm(list=ls())
gc()

#remotes::install_github("hvillalo/satin2") #for quiver plots in R
library(sf)
library(reshape2)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(ggquiver)

#set working directory
working.dir <- getwd()
setwd(working.dir)

Zone = 'SW Capes'

#lims of the spatial plots # change for each mp, bigger than you think because of arrrows #
xxlim = c(112.8, 115) #all NW c(114, 117)#ABR c(112.8, 115) #long
yylim = c(-29.5, -26) #all NW c(-21.5, -19) #ABR  #lat

#all this is on git already - i don't know how to code in different for git??
# only one I use for the plots so far is the "aus" one for the coast outline
#setting up mapping/coastal are for spatial, taken from kingsley script X_siteplots
aus    <- st_read("data/spatial/shp/61395_mif/australia/cstauscd_r.mif") #data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                        # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
#extra bits haven't used or loaded for these maps
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")         # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Montebello Islands", #"Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby
NW_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- NW_mpa[NW_mpa$ResName %in% c("Montebello", "Jurien", "Shark Bay"), ]    # just nat parks nearby
cwatr  <- readRDS('output/coastal_waters_limit_trimmed.rds')                    # coastal waters line trimmed in 'R/GA_coast_trim.R'
bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)
st_crs(dirkh)       <- st_crs(aumpa) 

## get data locations /limits that need from MPA
## do control F replace to replace names in the script 
##### SLA ####
sla.data <- readRDS("data/spatial/oceanography/Abrolhos_SLA_month.rds")%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                               "June","July","August","September","October",
                               "November","December")))%>%
  glimpse()

min_sla =round(min(min(sla.data$sla,na.rm = TRUE), na.rm = TRUE),digits = 2)
max_sla= round(max(max(sla.data$sla,na.rm = TRUE), na.rm = TRUE), digits = 2)

title_legend <- "SLA"
p_1 <- ggplot() +
  geom_tile(data = sla.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sla))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))+
  facet_wrap(~month, nrow = 4, ncol = 3)
p_1

ggsave('plots/spatial/Abrolhos_SLA_monthly_spatial.png',p_1, dpi = 300, width = 8, height = 9)
dev.off()

######### SST #########

sst.data <- readRDS("data/spatial/oceanography/Abrolhos_SST_month.rds")%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                                                     "June","July","August","September","October",
                                                     "November","December")))%>%
  glimpse()

min_sst =round(min(min(sst.data$sst,na.rm = TRUE), na.rm = TRUE))
max_sst= round(max(max(sst.data$sst,na.rm = TRUE), na.rm = TRUE))

title_legend <- "SST"
p_2 <- ggplot() +
  geom_tile(data = sst.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sst))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 0.5),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), 
  #             vecsize=arrow_size, color = "white")+
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  # ggtitle(month.name[[i]])+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))+
  facet_wrap(~month, nrow = 4, ncol = 3)
p_2

ggsave('plots/spatial/Abrolhos_SST_monthly_spatial.png',p_2, dpi = 300, width = 8, height = 9)

dev.off()


##### DEGREE HEATING WEEKS ####

dhw.data <- readRDS("data/spatial/oceanography/Abrolhos_DHW_month.rds")%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                                                     "June","July","August","September","October",
                                                     "November","December")))%>%
  glimpse()

min_dhw = round(min(min(dhw.data$dhw,na.rm = TRUE), na.rm = TRUE))
max_dhw = round(max(max(dhw.data$dhw,na.rm = TRUE), na.rm = TRUE))

title_legend <- "DHW"
p_3 <- ggplot() +
  geom_tile(data = dhw.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = dhw))+
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_dhw, to = max_dhw, by = 2),
                       limits = c(min_dhw, max_dhw)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))+
  facet_wrap(~month, nrow = 4, ncol = 3)
p_3

ggsave('plots/spatial/Abrolhos_DHW_monthly_spatial.png',p_3, dpi = 300, width = 8, height = 9)

dev.off()

##### ACIDIFICATION #####
acd_ts_monthly <- readRDS("data/spatial/oceanography/Abrolhos_acidification.rds")%>%
  glimpse()

legend_title = "Season"
acd_mean_plot <- ggplot(data = acd_ts_monthly, aes(x = year, y = acd_mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = acd_mean-acd_sd, ymax = acd_mean+acd_sd), fill = "black",alpha = 0.15) +
  theme_classic() +
  labs(x = "Year", y = "pH")
acd_mean_plot #plot with the other time series

##### Average plots - time series ####
#plot for sla, summer and winter mean
sla.monthly <- readRDS("data/spatial/oceanography/Abrolhos_SLA_ts.rds")%>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", 
                                   month %in% c(12,1,2) ~ "Summer", 
                                  month %in% c(3,4,5) ~ "Autumn", 
                                  month %in% c(9,10,11) ~ "Spring" )) %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarise(sla_mean_sea = mean(sla, na.rm = TRUE), sla_sd_sea = mean(sd, na.rm = TRUE)) %>%
  glimpse()

sla_plot <- sla.monthly %>% filter(grepl('Winter|Summer', season))

sla_mean_plot <- ggplot() + 
  geom_line(data = sla_plot, aes(x = year, y = sla_mean_sea, color = season)) + 
  geom_ribbon(data = sla_plot,aes(x = year, y = sla_mean_sea,
                                  ymin = sla_mean_sea-sla_sd_sea, 
                                  ymax = sla_mean_sea+sla_sd_sea, fill = season), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  labs(x = "Year", y = "SLA (m)", color = legend_title)+
  scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))
sla_mean_plot


#plot for sst summer and winter mean
sst_tss <- readRDS("data/spatial/oceanography/Abrolhos_SST_ts.rds")%>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", month %in% c(12,1,2) ~ "Summer", 
                                   month %in% c(3,4,5) ~ "Autumn", month %in% c(9,10,11) ~ "Spring" )) %>%
  group_by(year, season) %>%
  summarise(sst_mean = mean(sst, na.rm = TRUE),sd_sst = mean(sd, na.rm = TRUE)) %>%
  glimpse()

sst_plot <- sst_tss %>% filter(grepl('Winter|Summer', season))

#plot for sst, summer and winter mean
sst_mean_plot <- ggplot() + 
  geom_line(data = sst_plot, aes(x = year, y = sst_mean, color = season)) + 
  geom_ribbon(data = sst_plot,aes(x = year, y = sst_mean,
                                  ymin = sst_mean-sd_sst, 
                                  ymax = sst_mean+sd_sst, fill = season), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  labs(x = "Year", y = expression(paste("SST (",degree~C,")")), color = legend_title)+
  scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))
sst_mean_plot

acd_mean_plot+sla_mean_plot+sst_mean_plot + plot_layout(ncol = 1, nrow = 3)

ggsave('plots/spatial/Abrolhos_acd_sla_sst_ts.png', dpi = 300, width = 8, height = 9)


