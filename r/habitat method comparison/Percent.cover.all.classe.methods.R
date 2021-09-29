####    Bar graph of % cover of benthic classes   ####
## Clear workspace ----
rm(list = ls())


install.packages("ggplot2")
install.packages("ggthemes")
install.packages("extrafont")
install.packages("broman") # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
install.packages("raster")
install.packages("sp")
install.packages("sf")
install.packages("rgdal")
install.packages("plyr")
install.packages("maptools")
#install.packages(mapmisc)
#install.packages("ggsn")
#devtools::install_github("3wen/legendMap")
install.packages("ggsn")
install.packages("grid")
install.packages("maps")
install.packages("broom")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggspatial")
install.packages("colorspace")


#### Maps of all surveys in GB: BRUVS, AUV, FTV amd DTV --
library(here)
library(ggplot2)
# library(ggthemes)
# library(extrafont)
# library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
# library(raster)
# library(sp)
# library(sf)
# library(rgdal)
# library(plyr)
# library(maptools)
# #library(mapmisc)
# #install.packages("ggsn")
# #devtools::install_github("3wen/legendMap")
# library(ggsn)
# library(grid)
# library(maps)
# library(broom)
# library(tidyverse)
library(dplyr)
library(tidyr)
# library(ggspatial)
library(colorspace)

here()
tidy.dir <- here("tidy data")
raw.dir<- here("raw data")

## Read GB shapefile ----
#gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))

###  BOSSstyle PLOT ----

## read BOSSdata data ----
setwd(tidy.dir)
dir()

BOSSstyle <- read.csv("20201119_Multibeamed_BOSSstyle._broad.habitat.csv")
str(BOSSstyle)
head(BOSSstyle)

# BOSS to long format ----

BOSSlong <- gather(BOSSstyle, Class, measurement, broad.Consolidated:broad.Unconsolidated, factor_key =T)
head(BOSSlong)
str(BOSSlong)
#levels(bruvlong$ZoneName)[levels(bruvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

bav <- aggregate(measurement~Class, data = BOSSlong, mean)
# define SE function --
se <- function(x) sd(x)/sqrt(length(x))
bse <- aggregate(measurement~Class, data = BOSSlong, se)
head(bse)

b <- cbind(bav, se = bse[,2])
head(b)
b$lower <- b$measurement-b$se
b$upper <- b$measurement+b$se
head(b)
str(b)

# reorder factors for plotting ----
#b$ZoneName <- ordered(b$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
b$Class <- ordered(b$Class, levels = c("broad.Seagrasses", "broad.Unconsolidated", "broad.Macroalgae", 
                                         "broad.Consolidated", "broad.Sponges", "broad.Stony.corals"))



# Bruv colors ----
# bluepal <- choose_palette()
# greenpal <- choose_palette()

# BRUV plot ----
theme_set(theme_bw())
pb <-ggplot(data=b, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(8)) +
  labs(title = "BOSS", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pb


###PLOT BRUV style ###

bruvstyle <- read.csv("20201119_Multibeamed_BRUVstyle._broad.habitat.csv")
str(bruvstyle)
head(bruvstyle)

# Bruv to long format ----

bruvlong <- gather(bruvstyle, Class, measurement, broad.Consolidated:broad.Unconsolidated, factor_key =T)
head(bruvlong)
str(bruvlong)
#levels(bruvlong$ZoneName)[levels(bruvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

bav2 <- aggregate(measurement~Class, data = bruvlong, mean)
# define SE function --
se2 <- function(x) sd(x)/sqrt(length(x))
bse2 <- aggregate(measurement~Class, data = bruvlong, se)
head(bse2)

b2 <- cbind(bav2, se = bse2[,2])
head(b2)
b2$lower <- b2$measurement-b2$se
b2$upper <- b2$measurement+b2$se
head(b2)
str(b2)

# reorder factors for plotting ----
#b$ZoneName <- ordered(b$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
b2$Class <- ordered(b2$Class, levels = c("broad.Seagrasses", "broad.Unconsolidated", "broad.Macroalgae", 
                                         "broad.Consolidated", "broad.Sponges", "broad.Stony.corals"))


# Bruv colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()

# BRUV plot ----
theme_set(theme_bw())
pb <-ggplot(data=b2, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(8)) +
  labs(title = "BRUV Style", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pb



##PLOT Downward ###

Down <- read.csv("20201119_Multibeamed_Downwards._broad.habitat.csv")
str(Down)
head(Down)

# Bruv to long format ----

Downlong <- gather(Down, Class, measurement, broad.Consolidated:broad.Unconsolidated, factor_key =T)
head(Downlong)
str(Downlong)
#levels(bruvlong$ZoneName)[levels(bruvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

bav3 <- aggregate(measurement~Class, data = Downlong, mean)
# define SE function --
se3 <- function(x) sd(x)/sqrt(length(x))
bse3 <- aggregate(measurement~Class, data = Downlong, se)
head(bse3)

b3 <- cbind(bav3, se = bse3[,2])
head(b3)
b3$lower <- b3$measurement-b3$se
b3$upper <- b3$measurement+b3$se
head(b3)
str(b3)

# reorder factors for plotting ----
#b$ZoneName <- ordered(b$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
b3$Class <- ordered(b3$Class, levels = c("broad.Seagrasses", "broad.Unconsolidated", "broad.Macroalgae", 
                                      "broad.Consolidated", "broad.Sponges", "broad.Stony.corals"))


# colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()

# Downwards plot ----
theme_set(theme_bw())
pb <-ggplot(data=b3, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(8)) +
  labs(title = "Downwards", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pb





#### REAL BRUV ----
RealBRUV <- read.csv("202006_Multibeamed_BRUV._broad.habitat.csv")
str(RealBRUV)
head(RealBRUV)

# Bruv to long format ----

RealBRUVlong <- gather(RealBRUV, Class, measurement, broad.Consolidated:broad.Unconsolidated, factor_key =T)
head(RealBRUVlong)
str(RealBRUVlong)
#levels(bruvlong$ZoneName)[levels(bruvlong$ZoneName)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"

bav4 <- aggregate(measurement~Class, data = RealBRUVlong, mean)
# define SE function --
se4 <- function(x) sd(x)/sqrt(length(x))
bse4 <- aggregate(measurement~Class, data = RealBRUVlong, se)
head(bse4)

b4 <- cbind(bav4, se = bse4[,2])
head(b4)
b4$lower <- b4$measurement-b4$se
b4$upper <- b4$measurement+b4$se
head(b4)
str(b4)

# reorder factors for plotting ----
#b$ZoneName <- ordered(b$ZoneName, levels=c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone"))
b4$Class <- ordered(b4$Class, levels = c("broad.Seagrasses", "broad.Unconsolidated", "broad.Macroalgae", 
                                         "broad.Consolidated", "broad.Sponges", "broad.Stony.corals"))


# colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()

# Downwards plot ----
theme_set(theme_bw())
pb <-ggplot(data=b4, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~ZoneName, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(8)) +
  labs(title = "Real BRUV", y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pb






## join all methods ----

b
b$Method <- "BOSS"
names(b) <- b("Class",   "measurement", "se" , "Method")
b <- b[-c(2, 6), ]
head(b)


b2 
b2$Method <- "BRUV Style"
b2 <- b2[-c(2, 6), ]
head(b2)


b3 
b3$Method <- "Downwards"
b3 <- b3[-c(5), ]
head(b3)



b4 
b4$Method <- "real BRUV"
head(b4)
b4$Method 


all <- rbind(b,b2,b3,b4)
head(all)
str(all)
all$Method <- as.factor(all$Method)

# match names
levels(all$Class)
#levels(all$Class)[levels(all$Class)=="total.seagrass"] <- "Seagrasses"
#levels(all$Class)[levels(all$Class)=="total.sponges"] <- "Sponges"
#levels(all$Class)[levels(all$Class)=="corals"] <- "Stony corals"
#levels(all$Class)[levels(all$Class)=="Stony.corals"] <- "Stony corals"
#levels(all$Class)[levels(all$Class)=="Turf.algae"] <- "Turf algae"

# remove other 
#all <- all[all$Class!="other.inverts",]
#all <- droplevels(all)
#levels(all$Class)

# change the zones
#all$ZoneName
#levels(all$ZoneName)[levels(all$ZoneName)=="Habitat Protection Zone"] <- "HPZ"
#levels(all$ZoneName)[levels(all$ZoneName)=="Multiple Use Zone"] <- "MUZ"
#levels(all$ZoneName)[levels(all$ZoneName)=="National Park Zone"] <- "NPZ"
#levels(all$ZoneName)[levels(all$ZoneName)=="Special Purpose Zone"] <- "SPZ"

#all$ZoneName <- ordered(all$ZoneName, levels=c("NPZ","HPZ", "MUZ", "SPZ"))

head(all)
str(all)
# save ----
#write.csv(all, paste(d.dir, "mean.class.cover.allmethods.csv", sep='/'))

## To start plotting LOAD ALL DATA HERE ----
#all <- read.csv(paste(d.dir, "mean.class.cover.allmethods.csv", sep ='/'))

# reorder factors for plotting ----
all$Class <- ordered(all$Class, levels=c("broad.Seagrasses", "broad.Unconsolidated", "broad.Macroalgae", 
                                         "broad.Consolidated", "broad.Sponges"))
#all$ZoneName <- ordered(all$ZoneName, levels=c("NPZ","HPZ", "MUZ", "SPZ"))
all$Method <- ordered(all$Method, levels = c("BOSS",  "BRUV Style" , "Downwards", "real BRUV"))

## color plot ---
allpal <- c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")
# colors ----
bluepal <- choose_palette()
greenpal <- choose_palette()

## plot all ----

theme_set(theme_bw())
pall <-ggplot(data=all, aes(x=Class, y=measurement, fill=Class)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(~Method) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 10, color = "black", face ="bold"))

pall


## plot all2 ----

theme_set(theme_bw())
pall2 <-ggplot(data=all, aes(x=Method, y=measurement, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Class) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 8, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold"))

pall2

## save ----
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, scale=1, dpi = 300)



####

## Separate plot ---
str(all)
levels(all$Class) # "Seagrasses"     "Unconsolidated" "Turf algae"     "Macroalgae"     "Consolidated"   "Sponges"        "Stony corals"  


## plot main classes ----
all2 <- all[all$Class !="Macroalgae",]
all2 <- all2[all2$Class !="Consolidated",]
all2 <- all2[all2$Class !="Sponges" ,]
all2 <- all2[all2$Class !="Stony corals" ,]
all2 <- droplevels(all2)
levels(all2$Class)


theme_set(theme_bw())
pall2 <-ggplot(data=all2, aes(x=Method, y=measurement, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Class) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 8, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold"))

pall2

## save ----
ggsave("Cover.Method.MainClass.png", plot = pall2, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, scale=1, dpi = 300)

## plot less dom classes ----
levels(all$Class)
all3 <- all[all$Class !="Seagrasses",]
all3 <- all3[all3$Class !="Unconsolidated",]
all3 <- all3[all3$Class !="Turf algae" ,]
all3 <- droplevels(all3)
levels(all3$Class)


theme_set(theme_bw())
pall2 <-ggplot(data=all3, aes(x=Method, y=measurement, fill=ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = measurement-se, ymax = measurement+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(ZoneName~Class) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  scale_fill_manual(values = greenpal(7)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 8, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold"))

pall2

## save ----
ggsave("Cover.Method.LessClass.png", plot = pall2, path = p.dir, width = 200, height = 134, units = "mm", dpi = 300)
#ggsave("Cover.Method.Class.Zone.png", plot = pall2, path = p.dir, scale=1, dpi = 300)