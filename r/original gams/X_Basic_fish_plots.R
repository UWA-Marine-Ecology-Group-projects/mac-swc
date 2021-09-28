## Script to do fish plots ----

# libraries ----
#install.packages("ggplot2")
library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(raster)
library(sp)
library(sf)
library(rgdal)
library(plyr)
library(maptools)
#library(mapmisc)
#install.packages("ggsn")
#devtools::install_github("3wen/legendMap")
#devtools::install_github("clauswilke/ggtextures")
library(ggtextures)
library(ggsn)
library(grid)
library(maps)
library(broom)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(colorspace)
library(RColorBrewer)
#install.packages("magritrr")
library(magritrr)
#install.packages("tidytext")
library(tidytext)
#install.packages("ggtextures")
library(ggtextures) # to add images to ggplot
#install.packages("magick")
library(magick) # to add images to ggplot
library(viridis)



# clear workspace ----
rm(list = ls())

# functions ----
se <- function(x) sd(x)/sqrt(length(x))

# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')


# load maxn data----
file.name <- "2020-06_sw_maxn.metadata.csv"
mxn <- read.csv(paste(dt.dir, file.name, sep='/')) %>%
  mutate_at(vars(full.name, sample, cluster), list(as.factor)) %>%
  glimpse()


# calculate mean, sd and se for each species, per cluster --
summ.mxn <- mxn %>%
  dplyr::filter(cluster != "nc") %>% # remove BRUVs that don't belong to any cluster
  dplyr::group_by(cluster, full.name) %>%
  dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  ungroup() %>%
  glimpse()

# calculate top 10 species mean, sd and se per cluster --
summ.mxn2 <- mxn %>%
  dplyr::filter(cluster != "nc") %>% # remove BRUVs that don't belong to any cluster
  dplyr::group_by(cluster, full.name) %>% 
  dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  #ungroup() %>% 
  #top_n(10, mean) %>% # get largest 10 means per cluster
  slice_max(order_by=mean, n=10, with_ties=FALSE) %>%
  #arrange(desc(mean)) %>%
  arrange(mean) %>%
  dplyr::mutate(order = dplyr::row_number()) %>%
  ungroup() %>%
  # 2. Arrange by mean maxn
  arrange(cluster) %>%
  # 3. Add order column of row numbers
  #dplyr::mutate(order = dplyr::row_number()) %>%
  mutate_at(vars(order), list(as.double)) %>%
  glimpse()

summary(summ.mxn2)
summ.mxn2 <- droplevels(summ.mxn2)

## calculate summary and get top 10 mean max n per cluster --
top.mxn <- mxn %>%
  #dplyr::filter(cluster != "nc") %>% # remove BRUVs that don't belong to any cluster
  dplyr::group_by(full.name) %>%
  dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  top_n(10, mean) %>% # get largest 10 means per cluster
  #dplyr::summarise(mean = mean(maxn), sd = sd(maxn), se = se(maxn)) %>%
  arrange(desc(mean)) %>%
  arrange(desc(full.name)) %>%
  #arrange(desc(cluster)) %>% # arrange from largest to smallest
  ungroup() %>%
  # 2. Arrange by mean maxn
  arrange(mean, full.name) %>%
  # 3. Add order column of row numbers
  dplyr::mutate(order = dplyr::row_number()) %>%
  glimpse()


## PLOT ----

# choose colors --
bluepal <- choose_palette()

theme_set(theme_bw())

pd <-ggplot(data=top.mxn, aes(order, mean), y=mean) +
  geom_bar(stat="identity", color = "black", aes(fill = full.name)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~cluster, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  scale_fill_manual(values = bluepal(10)) +
  #scale_fill_manual(values = zonecolors) +
  labs(x = "Species", y = "mean MaxN") +
  # Add categories to axis
  scale_x_continuous(
    breaks = top.mxn$order,
    labels = top.mxn$full.name,
    expand = c(0,0)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 10, face="italic"), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip()

pd


## loop plot ----

# see palettes 
#display.brewer.all()
#display.brewer.all(colorblindFriendly = TRUE)
# View a single RColorBrewer palette by specifying its name
#display.brewer.pal(n = 10, name = "YlGnBu")
# Hexadecimal color specification 
#
pal2 <- brewer.pal(n = 10, name = "YlGnBu")

bluepal <- choose_palette()
pal2 <- bluepal(10)

setwd(p.dir)

# Specify all the names for each graph: cluster names
ynames <- levels(summ.mxn2$cluster)



for(i in 1:length(levels(summ.mxn2$cluster))){
  
  #fplot = summ.mxn2[c(1:3,6)] # select columns you want (always want the first 3 and the others will vary depending on i)
  dfplot = summ.mxn2 %>%  filter(cluster == ynames[i]) # select columns you want (always want the first 3 and the others will vary depending on i)
  breaks = dfplot$order
  labels = dfplot$full.name
  
  tiff(paste(paste(p.dir, "top10mxn/", sep='/'), paste(ynames[i], "tiff", sep = "."), sep=''), width = 3, height = 3.5, units = "in", pointsize =8, compression="lzw", res = 600)
  print(
    ggplot(dfplot, aes(x=order, y=mean, fill = order)) +
      geom_bar(stat="identity") +
      #scale_shape_manual(values = c(24, 21, 25)) +
      geom_point(size = 2, color = "transparent") +
      #scale_shape_manual(values = c(21, 21, 21)) +
      #scale_colour_manual(values = c("Black", "Dark gray", "Black")) + 
      #scale_fill_manual(values = bluepal(10)) +
      scale_fill_gradientn(colours = pal2) +
      #geom_smooth(aes_string(x="Year", y="Mean", group = "Depth", colour = "Depth"), size = 0.1, se = FALSE, method = "lm", fill = "light grey") +
      #geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.3, color = "black", size = 0.2) +
      # se TRUE when you want confidence intervals
      #facet_wrap(~ Depth, ncol = 1, scales = "free_y") +
      #scale_y_continuous(breaks = pretty_breaks(n = 4)) +
      #facet_wrap(~ Location, ncol = 1, strip.position = "right") +
      #facet_grid(~ Location) +
      xlab("Species") +
      #ylab(paste(ynames[i], " Mean MaxN")) +
      ylab("Mean MaxN") +
      #scale_x_discrete(breaks = dfplot$order,labels = dfplot$full.name,expand = c(0,0)) +
      scale_x_continuous(breaks = breaks, labels = labels, expand = c(0,0)) +
      #scale_y_continuous(breaks = trans_breaks(identity, identity, n = 4)) +
      #scale_y_continuous(breaks = pretty_breaks(n = 4)) +
      #geom_rect(inherit.aes=FALSE, aes(xmin = 2011, ymax = 2011, ymin = -Inf, ymax = Inf), color = "transparent", fill ="light grey", alpha = 0.3)+
      #ylim(-5, 100) +
      theme(axis.text.x = element_text(size = 7, color = "black", angle = 45, hjust = 0.5, vjust = 0.5), 
            panel.background = element_rect(fill="white"),
            panel.border = element_rect(color = "grey", fill = NA),
            axis.text.y = element_text(size = 7, color = "black"),
            axis.title.y = element_text(size = 8, color = "black"),
            axis.title.x = element_text(size = 8, color = "black"),
            axis.ticks = element_line(size=0.2),
            axis.ticks.length=unit(.05, "cm"),
            #strip.background = element_rect(color = "light grey"),
            #strip.text.x = element_text(size = 8, color = "black"),
            #strip.text.y = element_text(size = 8, color = "black"),
            #legend.title = element_text(size = 8, face = "bold"),
            #legend.text = element_text(size = 8),
            legend.position = "none",
            #legend.key = element_rect(fill = "transparent", color = "transparent"),
            #legend.key.size = unit(0.5,"cm")) 
      ) +
      coord_flip()
  )

  dev.off()
}
