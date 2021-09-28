# Load libraries ----
#install.packages("tibble")
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
o.dir <- paste(w.dir, "outputs", sep = '/')


library(tidyverse)
library(fst)

# Functions for plotting ----
# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))

se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

# Theme for plotting ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.y=element_text(size=14),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=14, face="bold.italic"))


# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')


# Read in data ----
setwd(dt.dir)
dir()

maxn<-read_csv("2020_south-west_stereo-BRUVs.complete.maxn.csv")
#length<-read_fst("complete.length.fst")
#mass<-read_fst("complete.mass.fst")

maxn$full.name = paste(maxn$genus,maxn$species)


# Maxn abundance Archetype 1 plot ----
maxn.filtered<-maxn%>%
  #filter(campaignid%in%c("2015-09_Exmouth.Gulf_stereoBRUVs"))%>%
  #filter(family%in%c("Lethrinidae"))%>%
  filter(full.name%in%c("Asymbolus occiduus",
                        "Callanthias australis",
                        "Centroberyx australis",
                        "Gerres sp1",
                        "Gerres subfasciatus",
                        "Kyphosus sydneyanus",
                        "Neosebastes nigropunctatus",
                        "Parequula elongata",
                        "Pristiophorus cirratus",
                        "Pterygotrigla polyommata",
                        "Squalus sp1",
                        "Squalus spp",
                        "Trachurus declivis"))

  ## calculate summary and get top 10 mean max n per cluster --
  top.mxn <- maxn.filtered %>%
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
  
  
  #PLOT#

#bluepal <- choose_palette()

theme_set(theme_bw())

Archetype.plot <-ggplot(data=top.mxn, aes(order, mean), y=mean) +
  geom_bar(stat="identity", color = "black", aes(fill = full.name)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~cluster, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  #scale_fill_manual(values = bluepal(10)) +
  #scale_fill_manual(values = zonecolors) +
  scale_fill_grey (start = 0.8, end = 0.8, na.value = "red", aesthetics = "fill") +
  labs(x = "Species", 
       y = "Average Abundance (MaxN)") +
  # Add categories to axis
  scale_x_continuous(
    breaks = top.mxn$order,
    labels = top.mxn$full.name,
    expand = c(0,0)
  ) +
  ylim(0, 10) +
  ggtitle("Deep Archetype") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14, face="italic"), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip() 
  


Archetype.plot

ggsave(paste(o.dir, "Archetype1.png", sep='/'), plot = Archetype.plot, device = "png", scale = 1, dpi = 600)


  


# Maxn abundance Archetype 2 plot ----
maxn.filtered2<-maxn%>%
  #filter(campaignid%in%c("2015-09_Exmouth.Gulf_stereoBRUVs"))%>%
  #filter(family%in%c("Lethrinidae"))%>%
  filter(full.name%in%c("Acanthaluteres brownii",
                        "Anoplocapros lenticularis",
                        "Bathytoshia brevicaudata",
                        "Centroberyx gerrardi",
                        "Centroberyx lineatus",
                        "Chrysophrys auratus",
                        "Coris auricularis",
                        "Enoplosus armatus",
                        "Epinephelides armatus",
                        "Furgaleus macki",
                        "Latropiscis purpurissatus",
                        "Meuschenia freycineti",
                        "Meuschenia scaber",
                        "Mustelus antarcticus",
                        "Neatypus obliquus",
                        "Nelusetta ayraud",
                        "Nemadactylus valenciennesi",
                        "Neosebastes bougainvillii",
                        "Neosebastes pandus",
                        "Oplegnathus woodwardi",
                        "Parapercis haackei",
                        "Parapercis ramsayi",
                        "Parupeneus chrysopleuron",
                        "Pseudocaranx spp",
                        "Seriola hippos",
                        "Sillago spp",
                        "Suezichthys bifurcatus",
                        "Tilodon sexfasciatus",
                        "Trygonorrhina dumerilii",
                        "Urolophus circularis"))

## calculate summary and get top 10 mean max n per cluster --
top.mxn2 <- maxn.filtered2 %>%
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


#PLOT#

#bluepal <- choose_palette()

theme_set(theme_bw())

Archetype.plot2 <-ggplot(data=top.mxn2, aes(order, mean), y=mean) +
  geom_bar(stat="identity", color = "black", aes(fill = full.name)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~cluster, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  #scale_fill_manual(values = bluepal(10)) +
  #scale_fill_manual(values = zonecolors) +
  scale_fill_grey (start = 0.8, end = 0.8, na.value = "red", aesthetics = "fill") +
  labs(x = "Species", y = "Average Abundance (MaxN)") +
  # Add categories to axis
  scale_x_continuous(
    breaks = top.mxn2$order,
    labels = top.mxn2$full.name,
    expand = c(0,0)
  ) +
  ylim(0,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14, face="italic"), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip() +
  ggtitle("Ubiquitous Archetype")


Archetype.plot2

ggsave(paste(o.dir, "Archetype2.png", sep='/'), plot = Archetype.plot2, device = "png", scale = 1, dpi = 600)

# Maxn abundance Archetype 3 plot ----
maxn.filtered3<-maxn%>%
  #filter(campaignid%in%c("2015-09_Exmouth.Gulf_stereoBRUVs"))%>%
  #filter(family%in%c("Lethrinidae"))%>%
  filter(full.name%in%c("Acanthaluteres vittiger",
                        "Achoerodus gouldii",
                        "Austrolabrus maculatus",
                        "Bodianus frenchii",
                        "Caesioperca rasor",
                        "Carcharhinus brevipinna",
                        "Chelmonops curiosus",
                        "Choerodon rubescens",
                        "Chromis klunzingeri",
                        "Chromis westaustralis",
                        "Dinolestes lewini",
                        "Eupetrichthys angustipes",
                        "Glaucosoma hebraicum",
                        "Heterodontus portusjacksoni",
                        "Hypoplectrodes nigroruber",
                        "Meuschenia flavolineata",
                        "Meuschenia galii",
                        "Meuschenia hippocrepis",
                        "Meuschenia venusta",
                        "Myliobatis tenuicaudatus",
                        "Notolabrus parilus",
                        "Ophthalmolepis lineolatus",
                        "Parascyllium variolatum",
                        "Parequula melbournensis",
                        "Parma bicolor",
                        "Pempheris klunzingeri",
                        "Pseudolabrus biserialis",
                        "Scobinichthys granulatus",
                        "Scorpis aequipinnis",
                        "Scorpis georgiana",
                        "Siphonognathus caninis",
                        "Sphyraena novaehollandiae",
                        "Trachinops noarlungae",
                        "Trygonoptera ovalis",
                        "Upeneichthys vlamingii"))

## calculate summary and get top 10 mean max n per cluster --
top.mxn3 <- maxn.filtered3 %>%
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


#PLOT#

#bluepal <- choose_palette()

theme_set(theme_bw())

Archetype.plot3 <-ggplot(data=top.mxn3, aes(order, mean), y=mean) +
  geom_bar(stat="identity", color = "black", aes(fill = full.name)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~cluster, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#93dfb8" ,"#eceabe", "#80daeb",  "#dbd7d2")) +
  #scale_fill_manual(values = bluepal(10)) +
  #scale_fill_manual(values = zonecolors) +
  scale_fill_grey (start = 0.8, end = 0.8, na.value = "red", aesthetics = "fill")+
  labs(x = "Species", y = "Average Abundance (MaxN)") +
  # Add categories to axis
  scale_x_continuous(
    breaks = top.mxn3$order,
    labels = top.mxn3$full.name,
    expand = c(0,0)
  ) +
  ylim(0,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14, face="italic"), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(color = 'black', fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold")) +
  coord_flip() +
  ggtitle("Shallow Archetype")


Archetype.plot3

ggsave(paste(o.dir, "Archetype3.png", sep='/'), plot = Archetype.plot3, device = "png", scale = 1, dpi = 600)
