###
# Project: mac - South-west Corner
# Data:    BRUV fish, habitat
# Task:    Control plots for in and out of sanctuary zone
# author:  Claude
# date:    Nov-Dec 2021
##

# Set directories----
rm(list=ls())

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)
library(patchwork)
library(grid)

raw.dat <- readRDS("data/tidy/2023-03_control-plot-data.rds") %>%
  dplyr::mutate(year = 2020) %>%
  glimpse()

#need to make a new dataframe - year, species richness (plus SE), greater than legal (plus SE)
year <- c("2017","2017","2018","2018","2019","2019","2020","2020","2021","2021","2022","2022")
status <- c("Fished","No-take")
depth.zone <- c("shallow", "mesophotic", "rariphotic")
dat <- data.frame(year,status, depth.zone) %>%
  complete(year, status, depth.zone)
dat$year <- as.numeric(dat$year)

#join together for plotting
dat.cp <- dat %>%
  left_join(raw.dat)%>%
  dplyr::filter(!depth.zone %in% "shallow") %>%
  glimpse()

# Greater than size of first maturity
gg.mat <- ggplot(data = dat.cp, aes(x = year, y = mature.mean, 
                                    ymin = mature.mean - mature.se,
                                    ymax = mature.mean + mature.se, 
                                    fill = status)) +
  geom_errorbar(data = dat.cp, position = position_dodge(width = 0.5))+ 
  geom_point(data = dat.cp,
             position = position_dodge(width = 0.5),
             shape = 21, size = 2, stroke = 1, color = "black")+  #
  theme_classic() +
  # scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 2018.5, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than size of maturity")+
  xlab("Year")+
  labs(title = "a)")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  facet_wrap(~depth.zone,
             labeller = labeller(depth.zone = c("mesophotic" = "Mesophotic",
                                                "rariphotic" = "Rariphotic")))
gg.mat

ggsave(filename = "plots/control-plots/mature-control.plots.png",gg.mat, units = "in", dpi = 300,
       height = 4, width = 8)

# Smaller than size of first maturity
gg.imm <- ggplot(data = dat.cp, aes(x = year, y = immature.mean, 
                                    ymin = immature.mean - immature.se,
                                    ymax = immature.mean + immature.se, 
                                    fill = status)) +
  geom_errorbar(data = dat.cp, position = position_dodge(width = 0.5))+ 
  geom_point(data = dat.cp,
             position = position_dodge(width = 0.5),
             shape = 21, size = 2, stroke = 1, color = "black")+  #
  theme_classic() +
  # scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 2018.5, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Smaller than size of maturity")+
  xlab("Year")+
  labs(title = "a)")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  facet_wrap(~depth.zone,
             labeller = labeller(depth.zone = c("mesophotic" = "Mesophotic",
                                                "rariphotic" = "Rariphotic")))
gg.imm

ggsave(filename = "plots/control-plots/immature-control.plots.png",gg.imm, units = "in", dpi = 300,
       height = 4, width = 8)

# Greater than legal size (updated size limits)
gg.leg <- ggplot(data = dat.cp, aes(x = year, y = legal.mean, 
                                    ymin = legal.mean - legal.se,
                                    ymax = legal.mean + legal.se, 
                                    fill = status)) +
  geom_errorbar(data = dat.cp, position = position_dodge(width = 0.5))+ 
  geom_point(data = dat.cp,
             position = position_dodge(width = 0.5),
             shape = 21, size = 2, stroke = 1, color = "black")+  #
  theme_classic() +
  # scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 2018.5, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  labs(title = "a)")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  facet_wrap(~depth.zone,
             labeller = labeller(depth.zone = c("mesophotic" = "Mesophotic",
                                                "rariphotic" = "Rariphotic")))
gg.leg

ggsave(filename = "plots/control-plots/legal-control.plots.png",gg.leg, units = "in", dpi = 300,
       height = 4, width = 8)

# Smaller than legal size (updated size limits)
gg.sub <- ggplot(data = dat.cp, aes(x = year, y = sublegal.mean, 
                                    ymin = sublegal.mean - sublegal.se,
                                    ymax = sublegal.mean + sublegal.se, 
                                    fill = status)) +
  geom_errorbar(data = dat.cp, position = position_dodge(width = 0.5))+ 
  geom_point(data = dat.cp,
             position = position_dodge(width = 0.5),
             shape = 21, size = 2, stroke = 1, color = "black")+  #
  theme_classic() +
  # scale_y_continuous(limits = c(5,15))+
  geom_vline(xintercept = 2018.5, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Smaller than legal size")+
  xlab("Year")+
  labs(title = "a)")+
  scale_fill_manual(labels = c("Special Purpose Zone", "National Park Zone"),values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Marine Park Zone"))+
  facet_wrap(~depth.zone,
             labeller = labeller(depth.zone = c("mesophotic" = "Mesophotic",
                                                "rariphotic" = "Rariphotic")))
gg.sub

ggsave(filename = "plots/control-plots/sublegal-control.plots.png",gg.sub, units = "in", dpi = 300,
       height = 4, width = 8)

