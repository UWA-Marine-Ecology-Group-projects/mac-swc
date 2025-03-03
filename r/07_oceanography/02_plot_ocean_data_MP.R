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
# detach("package:cowplot", unload=TRUE)

#set working directory
working.dir <- getwd()
setwd(working.dir)

#set name to load and export data
Zone = "SwC"

#lims of the spatial plots # change for each mp, bigger than you think because of arrrows #
xxlim = c(114.353, 115.723) 
yylim = c(-34.618, -33.479) 

#set crs
wgscrs <- CRS("+proj=longlat +datum=WGS84")

#load australian outline
aus    <- st_read("data/spatial/shapefiles/61395_mif/australia/cstauscd_r.mif") #data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == "mainland", ]
#load marine park boundaries, state and commonwealth
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")         # all aus mpas
wampa  <- st_read("data/spatial/shapefiles/test1.shp", crs = wgscrs)                           # all wa mpas
st_crs(aus)         <- st_crs(aumpa)

##### SLA ####
sla.data <- readRDS(paste0("data/spatial/oceanography/",Zone,"_SLA_month.rds"))%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                                                     "June","July","August","September","October",
                                                     "November","December")))%>%
  glimpse()

min_sla =round(min(min(sla.data$sla,na.rm = TRUE), na.rm = TRUE),digits = 2)
max_sla= round(max(max(sla.data$sla,na.rm = TRUE), na.rm = TRUE), digits = 2)

title_legend <- "SLA (m)"
p_1 <- ggplot() +
  geom_tile(data = sla.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sla, color = sla))+
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  scale_color_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend, color = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(114.5,115.0,115.5))+
  facet_wrap(~month, nrow = 4, ncol = 3)
p_1

ggsave(paste0('plots/spatial/',Zone,'_SLA_monthly_spatial.png'),p_1, dpi = 300, width = 6, height = 4.5)
dev.off()

######### SST #########

sst.data <- readRDS(paste0("data/spatial/oceanography/",Zone,"_SST_month.rds"))%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                                                     "June","July","August","September","October",
                                                     "November","December")))%>%
  glimpse()

min_sst =round(min(min(sst.data$sst,na.rm = TRUE), na.rm = TRUE))
max_sst= round(max(max(sst.data$sst,na.rm = TRUE), na.rm = TRUE))

title_legend <- expression(paste("SST (",degree~C,")"))
p_2 <- ggplot() +
  geom_tile(data = sst.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sst, color = sst))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 1),
                       limits = c(min_sst, max_sst)) +
  scale_color_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 1),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), 
  #             vecsize=arrow_size, color = "white")+
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend,color = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  # ggtitle(month.name[[i]])+
  scale_x_continuous(breaks=c(114.5,115.0,115.5))+
  facet_wrap(~month, nrow = 4, ncol = 3)
p_2

ggsave(paste0('plots/spatial/',Zone,'_SST_monthly_spatial.png'),p_2, dpi = 300, width = 6, height = 4.5)

dev.off()

##### DEGREE HEATING WEEKS ####
dhw.heatwave <- readRDS(paste0("data/spatial/oceanography/",Zone,"_DHW_heatwave.rds"))%>%
  ungroup()%>%
  dplyr::mutate(title=ifelse(year=='2011',"2011 May",year))%>%
  dplyr::mutate(title=ifelse(title=='2021',"2021 May",title))%>%
  glimpse()

min_dhw = round(min(min(dhw.heatwave$dhw,na.rm = TRUE), na.rm = TRUE))
max_dhw = round(max(max(dhw.heatwave$dhw,na.rm = TRUE), na.rm = TRUE))

title_legend <- "DHW"
p_3 <- ggplot() +
  geom_tile(data = dhw.heatwave, 
            aes(x = Lon, y = Lat, fill = dhw, color = dhw))+
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = 0, to = max_dhw, by = 5),
                       limits = c(0, max_dhw)) +
  scale_color_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = 0, to = max_dhw, by = 5),
                       limits = c(0, max_dhw)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend, color = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(114.5,115.0,115.5))+
  facet_wrap(~title)
p_3

ggsave(paste0('plots/spatial/',Zone,'_DHW_monthly_spatial.png'),p_3, dpi = 300, width = 6, height = 3.5)

dev.off()

##### ACIDIFICATION #####
acd_ts_monthly <- readRDS(paste0("data/spatial/oceanography/",Zone,"_acidification.rds"))%>%
  dplyr::filter(!year %in% c("1870", "2013")) %>% # These 2 years have inaccurate averages as they are only on 6 months
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
sla.monthly <- readRDS(paste0("data/spatial/oceanography/",Zone,"_SLA_ts.rds"))%>%
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
sst_tss <- readRDS(paste0("data/spatial/oceanography/",Zone,"_SST_ts.rds"))%>%
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

#plot for dhw data 
dhw_plot <- readRDS(paste0("data/spatial/oceanography/",Zone,"_DHW_ts.rds"))%>%
  group_by(year) %>%
  summarise(dhw_mean = mean(dhw, na.rm = TRUE),sd_dhw = mean(sd, na.rm = TRUE)) %>%
  glimpse()

dhw_mean_plot <- ggplot() + 
  geom_vline(xintercept = 2011, color = "red", linetype = 5, alpha = 0.5)+
  geom_vline(xintercept = 2021, color = "red", linetype = 5, alpha = 0.5)+
  geom_line(data = dhw_plot, aes(x = year, y = dhw_mean)) + 
  geom_ribbon(data = dhw_plot,aes(x = year, y = dhw_mean,
                                  ymin = dhw_mean-sd_dhw, 
                                  ymax = dhw_mean+sd_dhw), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  scale_x_continuous(limits = c(1993,2022))+
  labs(x = "Year", y = "DHW")
dhw_mean_plot

acd_mean_plot+sla_mean_plot+sst_mean_plot + dhw_mean_plot+plot_layout(ncol = 1, nrow = 4)

ggsave(paste0('plots/spatial/',Zone,'_acd_sla_sst_ts.png'), dpi = 300, width = 6, height = 6.75)
