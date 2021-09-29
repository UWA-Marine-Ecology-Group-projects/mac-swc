##Script for cleaning BOSS habitat data prior to analyses
install.packages("here")
install.packages("rprojroot")
# Clear memory ----
rm(list=ls())

# Libraries required ----
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(ggplot2)

# Set work directories----
setwd(project_path)
here("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/boss-habitat-swcmp")
tidy.dir <- here("tidy data")
raw.dir <- here("raw data")


# Functions----
se <- function(x) sd(x)/sqrt(length(x))




# p.est <- mean(binary)
# variance <- (p.est*(1-p.est))/nrow(binary)
# std.dev <- sqrt(variance)



# Study name----

study <- "20201119_Multibeamed_BOSSstyle"



# Read in metadata----
setwd(raw.dir)
metadata <- read_csv("metadata37rows.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, time, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview


# Load and format annotation data----
setwd(raw.dir)
dir()

habitat <- read.delim(paste(study,"Dot Point Measurements.txt",sep = "_"),header=T,skip=4,stringsAsFactors=FALSE)%>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","N"="","E"="","S"="","W"=""))) %>%# remove N,E,S,W from sample
  mutate(filename=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>% #keep filename but remove .jpg (need this for later to ensure unique ID)
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  mutate(filename=as.character(filename)) %>%
  select(filename,sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview





# Check number of points per image ----
number.of.annotations<-habitat%>%
  dplyr::group_by(filename)%>%
  dplyr::summarise(number.of.annotations=n()) %>% # count the number of annotations per image
glimpse()
wrong.number<-number.of.annotations%>%
  filter(!number.of.annotations==50) %>% 
  glimpse() # see images where there is too many or too little annotations (in this example there are none), go back into the *.TMObs file to fix this before re-exporting DO NOT FIX IN THE TXT FILE

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat


# Crazy point munge----

habitat20 <- habitat %>% group_by(sample) %>% sample_n(20)%>%
  mutate(number.points="p.20")%>%
  glimpse()

habitat40 <- habitat %>% group_by(sample) %>% sample_n(40)%>%
  mutate(number.points="p.40")%>%
  glimpse()

habitat100 <- habitat %>% group_by(sample) %>% sample_n(100)%>%
  mutate(number.points="p.100")%>%
  glimpse()

habitat200 <- habitat %>% group_by(sample) %>% sample_n(200)%>%
  mutate(number.points="p.200")%>%
  glimpse()

## I need to check this data frame ----
habitat.all.points<-rbind(habitat20,habitat40,habitat100,habitat200)%>%
  mutate(value=1)%>%
  mutate(key=broad)%>%
  spread(key=broad,value=value, fill=0)%>%
  tidyr::complete(nesting(number.points,key)) %>%
  glimpse()

# define factor columns ----
habitat.all.points$sample <- as.factor(habitat.all.points$sample)
habitat.all.points$number.points <- as.factor(habitat.all.points$number.points)
summary(habitat.all.points)


ns<-habitat.all.points%>%
group_by(number.points, key)%>%
  summarise(n()) %>%
  glimpse()


#1. Save out mean, se and sd ##
habitat.all.mean<-habitat.all.points%>%
group_by(number.points)%>%
    summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                   "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (mean="mean"))%>%
  glimpse()

habitat.all.se<-habitat.all.points%>%
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (se="se")) %>%
  glimpse()


habitat.all.sd<-habitat.all.points%>%
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (sd="sd"))


habitat.all.n<-habitat.all.points%>% # this is not working
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (n="n"))





#2. change to long format for plotting
longmean<-habitat.all.mean %>%
  gather(broad, mean, -c(number.points), factor_key=TRUE)
longse<-habitat.all.se %>%
  gather(broad, se, -c(number.points), factor_key=TRUE)
longsd<-habitat.all.sd %>%
  gather(broad, sd, -c(number.points), factor_key=TRUE)



#3. merge dataframes 
habitat.all.meanse<-merge(longmean,longse, by=c("number.points", "broad")) 
habitat.all.sum<-merge(habitat.all.meanse,longsd, by=c("number.points", "broad"))



#4. save number of points as factor and re-order
habitat.all.sum$number.points=factor(habitat.all.sum$number.points, levels = c ("p.20", "p.40","p.100", "p.200"))
glimpse(habitat.all.sum)


# plot ----
theme_set(theme_bw())
pall2 <-ggplot(data=habitat.all.sum2, aes(x=number.points, y=mean, fill=broad)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(~broad) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(7)) +
  #labs(title = "Autonomous Underwater Vehicle", y = "Mean % cover") +
  labs(y = "Probability of occurence") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size=10, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "grey90"),
        strip.text.x = element_text(size = 8, color = "black", face ="bold"),
        strip.text.y = element_text(size = 10, color = "black", face ="bold"))

pall2


## Anitas: ----



# 1. Save out mean, se and sd per image ----
habitat.all.mean.img<-habitat.all.points%>%
  group_by(sample, number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (mean="mean"))%>%
  glimpse()


habitat.all.se.img<-habitat.all.points%>%
  group_by(sample, number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (se="se")) %>%
  glimpse()

head(habitat.all.points)
glimpse(habitat.all.points)


habitat.all.sd.img<-habitat.all.points%>%
  group_by(sample, number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (sd="sd"))%>%
  glimpse()

habitat.all.n.img<-habitat.all.points%>% # this is not working
  group_by(sample, number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (n="n"))%>%
  glimpse()



# 2. change to long format for plotting, per image ----
longmean2<-habitat.all.mean.img %>%
  gather(broad, mean, -c(sample, number.points), factor_key=TRUE) %>% glimpse()
longse2<-habitat.all.se.img %>%
  gather(broad, se, -c(sample, number.points), factor_key=TRUE) %>% glimpse()
longsd2<-habitat.all.sd.img %>%
  gather(broad, sd, -c(sample, number.points), factor_key=TRUE) %>% glimpse()

# 3. merge dataframes with per image ----
habitat.all.meanse2<-merge(longmean2,longse2, by=c("sample", "broad","number.points")) %>% glimpse()
habitat.all.sum2<-merge(habitat.all.meanse2,longsd2, by=c("sample", "number.points", "broad"))  %>% glimpse()


# 4. save number of points as factor and re-order 2 ----
habitat.all.sum2$number.points=factor(habitat.all.sum$number.points, levels = c ("p.20", "p.40","p.100", "p.200"))
glimpse(habitat.all.sum2)


# 5. plot ----
theme_set(theme_bw())
pall2 <-ggplot(data=habitat.all.sum2 %>% 
                 #dplyr::filter(broad != 'Hydroids' & broad != 'Open Water' & broad !='Stony corals' ),
                 #dplyr::filter(broad == 'Macroalgae'),
                 dplyr::filter(broad != 'Hydroids' & broad != 'Open Water' & broad !='Stony corals' & broad !='Consolidated' & broad !='Unconsolidated' ),
                 aes(x=sample, y=mean, fill=broad)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, cex = 0.5, color = 'blue') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3, cex = 0.8) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(number.points~broad) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(7)) +
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


## 6. Get summary per no. of points analysed ----

habitat.all.mean.img2<-habitat.all.mean.img%>%
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (mean="mean"))%>%
  glimpse()


habitat.all.se.img2<-habitat.all.se.img%>%
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (se="se")) %>%
  glimpse()

head(habitat.all.points)
glimpse(habitat.all.points)


habitat.all.sd.img2<-habitat.all.sd.img%>%
  group_by(number.points)%>%
  summarise_at(c("Consolidated","Hydroids","Macroalgae","Open Water","Seagrasses",  
                 "Sponges", "Stony corals", "Unconsolidated", "Unknown"),
               .funs = (sd="sd"))%>%
  glimpse()


# 7. change to long format for plotting, per no. of points analysed  ----
longmean3<-habitat.all.mean.img2 %>%
  gather(broad, mean, -c(number.points), factor_key=TRUE) %>% glimpse()
longse3<-habitat.all.se.img2 %>%
  gather(broad, se, -c(number.points), factor_key=TRUE) %>% glimpse()
longsd3<-habitat.all.sd.img2 %>%
  gather(broad, sd, -c(number.points), factor_key=TRUE) %>% glimpse()

# 8. merge dataframes with per no. of points analysed  ----
habitat.all.meanse3<-merge(longmean3,longse3, by=c("broad","number.points")) %>% glimpse()
habitat.all.sum3<-merge(habitat.all.meanse3,longsd3, by=c("number.points", "broad"))  %>% glimpse()


# 9. save number of points as factor and re-order 2 ----
habitat.all.sum3$number.points=factor(habitat.all.sum3$number.points, levels = c ("p.20", "p.40","p.100", "p.200"))
glimpse(habitat.all.sum3)



# 10. plot ----
theme_set(theme_bw())
pall2 <-ggplot(data=habitat.all.sum3 %>% 
                 dplyr::filter(broad != 'Hydroids' & broad != 'Open Water' & broad !='Stony corals' ),
                 #dplyr::filter(broad == 'Macroalgae'),
               aes(x=number.points, y=mean, fill=broad)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, cex = 0.5, color = 'blue') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3, cex = 0.8) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_grid(~broad) +
  #facet_wrap(~Zone, ncol = 2, scales = 'free') +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(7)) +
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



SDfrom20points <- longsd3[ which(longsd3$number.points=='p.20'),]
SDfrom40points <- longsd3[ which(longsd3$number.points=='p.40'),]
SDfrom100points <- longsd3[ which(longsd3$number.points=='p.100'),]
SDfrom200points <- longsd3[ which(longsd3$number.points=='p.200'),]




# Create %fov----
fov<-habitat%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, filename))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()


# CREATE catami point score------

broad<-habitat%>%
  dplyr::select(-c(morphology,type))%>%
  # filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  filter(!broad%in%c("",NA,"Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,filename,fieldofview,relief))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse



##PLOT Unknowns against position in image## 

Unknowns <-filter(habitat, broad == "Unknown")
Unconsolidated <-filter(habitat, broad == "Unconsolidated")
Turf <-filter(habitat, type == "Turf mat")



library(ggplot2)
# Basic scatter plot
ggplot(habitat, aes(x=image.row, y=broad)) + geom_point(alpha=0.05)
glimpse(habitat)





library(ggplot2)
# Basic histogram

p<-ggplot(Unknowns, aes(y=image.row)) + 
  geom_histogram(color="black", fill="white") +
  labs(y="Image Row", x = "Number of Unknowns") +
  scale_y_reverse()
p


# Write habitat data----

setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/boss-habitat-swcmp/tidy data")
dir()

habitat.broad <- metadata%>%
  left_join(fov,by="sample")%>%
   left_join(broad,by="sample")


write.csv(habitat.broad,file=paste(study,"_broad.habitat.csv",sep = "."), row.names=FALSE)



#### Subset number of points to 25 ###

glimpse(habitat)
habitat25 <- habitat %>% group_by(sample) %>% sample_n(25)





fov25<-habitat25%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, filename))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()


# CREATE catami point score------


broad25<-habitat25%>%
  dplyr::select(-c(morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,filename,fieldofview,relief))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse



# Write habitat data----

setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/boss-habitat-swcmp/tidy data")
dir()

habitat.broad25 <- metadata%>%
  left_join(fov25,by="sample")%>%
  left_join(broad25,by="sample")


write.csv(habitat.broad25,file=paste(study,"_broad.habitat25.csv",sep = "."), row.names=FALSE)





#### Subset number of points to 50 ###

habitat50 <- habitat %>% group_by(sample) %>% sample_n(50)

fov50<-habitat50%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, filename))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()


# CREATE catami point score------


broad50<-habitat50%>%
  dplyr::select(-c(morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,filename,fieldofview,relief))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse



# Write habitat data----

setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/boss-habitat-swcmp/tidy data")
dir()

habitat.broad50 <- metadata%>%
  left_join(fov50,by="sample")%>%
  left_join(broad50,by="sample")


write.csv(habitat.broad50,file=paste(study,"_broad.habitat50.csv",sep = "."), row.names=FALSE)
#### Subset number of points to 100 ###

habitat100 <- habitat %>% group_by(sample) %>% sample_n(100)



fov100<-habitat100%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, filename))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()


# CREATE catami point score------


broad100<-habitat100%>%
  dplyr::select(-c(morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,filename,fieldofview,relief))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse



# Write habitat data----

setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/boss-habitat-swcmp/tidy data")
dir()

habitat.broad100 <- metadata%>%
  left_join(fov100,by="sample")%>%
  left_join(broad100,by="sample")

write.csv(habitat.broad100,file=paste(study,"_broad.habitat100.csv",sep = "."), row.names=FALSE)





