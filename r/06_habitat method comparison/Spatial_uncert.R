### Check for spatial uncertainty in the predictions ----

# from Ben and Sharyn's script --

# this is done using only the validation data  30% set aside --


## background libraries 
library(caret)
library(raster)
library(rgdal)
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(leaflet)
library(plotGoogleMaps)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(plotrix)
library(sp)
library(caTools)

# Moran's I and spatial dependencies
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(pgirmess) # Data Analysis in Ecology

# Attach libraries for point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic

w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)


###   ###   ###   ###   ###  ###

#### BRUV coarse ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_coarse_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns --
names(df)
#df2 <- df
df2 <- df[,c(3:14)]
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # 6 Classes: consolidated, macroalgae, seagrasses, sponges, turf and unconsolidated
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))
which(is.na(df2))

# remove Nas if needed --
df2 <- na.omit(df2)
any(is.na(df2))
head(df2)
str(df2)

### Get train and test data --
set.seed(777)
sample <- caTools::sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 245  10
dim(test) # [1] 79 10

summary(df2)

# remove 'consolidated', 'sponges'classes --
train <- train[train$Class != "Sponges",]
train <- train[train$Class != "Consolidated",]
train <- droplevels(train)
summary(train)
levels(train$Class)

test <- test[test$Class != "Sponges",]
test <- test[test$Class != "Consolidated",]
test <- droplevels(test)
summary(test)
levels(test$Class)
head(test)
# remove unwanted cols 
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Seagrasses"="Seagrass", "Turf.algae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass", "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae"    ,   "Seagrass" ,   "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~longitude + latitude
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds)

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$GBpred.Coarse.Bruvs, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.Bruvs,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$longitude, preds$latitude)))
maxDist

xy=cbind(preds$longitude, preds$latitude)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("longitude","latitude","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


###   ###   ###   ###   ###  ###

#### AUV coarse ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_auv_coarse_bathy_filtered_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::select(Class, coords.x1, coords.x2, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
df2$Class <- as.factor(df2$Class)
levels(df2$Class) # "total.seagrass" "Turf.algae"     "Unconsolidated"
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
df2 <- na.omit(df2)
any(is.na(df2))
head(df2)
str(df2)

### Get train and test data --
set.seed(777)
sample <- caTools::sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 64 12
dim(test) # [1] 20 12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Consolidated"="Unvegetated", "Seagrasses"="Seagrass", "Macroalgae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass", "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae"    ,   "Seagrass" ,   "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~coords.x1 + coords.x2
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds)

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$classval, preds$GBpred.Coarse.AUV), byClass = T)

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~coords.x1, ~coords.x2, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$coords.x1, preds$coords.x2)))
maxDist

xy=cbind(preds$coords.x1, preds$coords.x2)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("coords.x1","coords.x2","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)



##    ##    ##    ##    ##    ##

#### FTV coarse ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_tv_coarse_bathy_filtered_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
df <- na.omit(df)
str(df) # 309 obs -- check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::select(Class, coords.x1, coords.x2, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Other"          "Seagrasses"     "Unconsolidated"
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
#df2 <- na.omit(df2)
#any(is.na(df2))
#head(df2)
#str(df2)

### Get train and test data ----
levels(df2$Class) # "Other"          "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 232  12
dim(test) # [1] 77 12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Seagrasses, 2 = Algae, 3 = Unconsolidated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Turf.algae"="Algae"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Seagrasses", "Algae",  "Unconsolidated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Seagrasses", "Algae",  "Unconsolidated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~coords.x1 + coords.x2
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 27 obs
preds$GBpred.Coarse.FTV <- as.factor(preds$GBpred.Coarse.FTV)

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))
# when different levels --
u <- union(preds$GBpred.Coarse.FTV, preds$classval)
t <- table(factor(preds$GBpred.Coarse.FTV, u), factor(preds$classval, u))
confusionMatrix(t)
#confusionMatrix(table(preds$GBpred.Coarse.FTV, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(factor(preds$GBpred.Coarse.FTV, u) == factor(preds$classval, u),1,0))
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~coords.x1, ~coords.x2, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$coords.x1, preds$coords.x2)))
maxDist

xy=cbind(preds$coords.x1, preds$coords.x2)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("coords.x1","coords.x2","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


##    ##    ##    ##    ##    ##

#### DTV coarse ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_dtv_coarse_bathy_filtered_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
df <- na.omit(df)
str(df) # 309 obs -- check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::select(Class, coords.x1, coords.x2, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Other"          "Seagrasses"     "Unconsolidated"
summary(df2)
head(df2)


# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
#df2 <- na.omit(df2)
#any(is.na(df2))
#head(df2)
#str(df2)

### Get train and test data ----
levels(df2$Class) # "Other"          "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 74 12
dim(test) # [1]  25 12

summary(test)
levels(test$Class)

# remove class if needed----
#test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
#test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("total.seagrass"= "Seagrass", "Turf.algae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass",  "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae", "Seagrass",  "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~coords.x1 + coords.x2
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 25 obs
preds$GBpred.Coarse.DTV <- as.factor(preds$GBpred.Coarse.DTV)
# There is no class 1 in validation data, so remove from factor ---
preds <- droplevels(preds)
str(preds) # 25 obs


##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$GBpred.Coarse.DTV, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$GBpred.Coarse.DTV == preds$classval,1,0) )
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~coords.x1, ~coords.x2, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$coords.x1, preds$coords.x2)))
maxDist

xy=cbind(preds$coords.x1, preds$coords.x2)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=5)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("coords.x1","coords.x2","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)


##    ##    ##    ##    ##    ##

#### BRUV fine ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::select(Class, latitude, longitude, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Other"          "Seagrasses"     "Unconsolidated"
summary(df2)
head(df2)


# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
#df2 <- na.omit(df2)
#any(is.na(df2))
#head(df2)
#str(df2)

### Get train and test data ----
levels(df2$Class) # "Other"          "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 245  12
dim(test) # [1]  79 12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Sponges",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Seagrasses"= "Seagrass", "Turf.algae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass",  "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae", "Seagrass",  "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~longitude + latitude
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 32 obs
#preds$GBpred.Coarse.DTV <- as.factor(preds$GBpred.Coarse.DTV)
# There is no class 1 in validation data, so remove from factor ---
preds <- droplevels(preds)
str(preds) # 25 obs


##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$GBpred.Fine.Bruvs, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$GBpred.Fine.Bruvs == preds$classval,1,0) )
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$latitude, preds$longitude)))
maxDist

xy=cbind(preds$latitude, preds$longitude)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("longitude","latitude","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)



##    ##    ##    ##    ##    ##

#### AUV Fine ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

df <- read.csv(paste(d.dir, "tidy", "GB_auv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df[,c(3:14)] # remove X, sample, lat, long
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Other"          "Seagrasses"     "Stony.corals"   "Turf.algae"     "Unconsolidated"
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
df2 <- na.omit(df2)
any(is.na(df2))
head(df2)
str(df2)

### Get train and test data --
set.seed(777)
sample <- caTools::sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 64 12
dim(test) # [1] 20 12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Other",]
test <- test[test$Class != "Stony.corals",]
test <- test[test$Class != "Sponges",]
test <- droplevels(test)
summary(test)
levels(test$Class)
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Consolidated"="Unvegetated", "Seagrasses"="Seagrass", "Macroalgae"="Algae", "Turf.algae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass", "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae"    ,   "Seagrass" ,   "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~longitude + latitude
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds)

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
# when different levels --
u <- union(preds$GBpred.Fine.AUV, preds$classval)
t <- table(factor(preds$GBpred.Fine.AUV, u), factor(preds$classval, u))
confusionMatrix(t)

#confusionMatrix(table(preds$GBpred.Fine.AUV, preds$classval), byClass = T) ## predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$GBpred.Fine.AUV == preds$classval,1,0))


# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$longitude, preds$latitude)))
maxDist

xy=cbind(preds$longitude, preds$latitude)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=8)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("longitude","latitude","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)



##    ##    ##    ##    ##    ##

#### FTV Fine ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_tv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df[,c(3:14)] # remove X, sample, lat, long
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2) # 1087 obs
levels(df2$Class) # "Other"          "Seagrasses"     "Sponges"        "Turf.algae"     "Unconsolidated"
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))
which(is.na(df2))

# remove Nas if needed --
df2 <- na.omit(df2)
any(is.na(df2))
head(df2)
#str(df2)

### Get train and test data ----
levels(df2$Class) # "Other"          "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 808  12
dim(test) # [1] 269  12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Seagrasses, 2 = Algae, 3 = Unconsolidated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Turf.algae"="Algae", "Seagrasses"="Seagrass",  "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass",  "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae", "Seagrass",  "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~longitude + latitude
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 268 obs
#preds$GBpred.Coarse.FTV <- as.factor(preds$GBpred.Coarse.FTV)

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))
confusionMatrix(table(preds$GBpred.Fine.FTV, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(factor(preds$GBpred.Fine.FTV, u) == factor(preds$classval, u),1,0))
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$longitude, preds$latitude)))
maxDist

xy=cbind(preds$longitude, preds$latitude)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("coords.x1","coords.x2","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)



##    ##    ##    ##    ##    ##

#### DTV Fine ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --
df <- read.csv(paste(d.dir, "tidy", "GB_dtv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
which(is.na(df))
df <- na.omit(df)
str(df) # 2663 obs

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df[,c(10:22)] # remove X, sample, lat, long
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="dominant"] <- "Class"
names(df2)
str(df2)
df2$Class <- as.factor(df2$Class)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "total.seagrass" "Turf.algae"     "Unconsolidated"  
summary(df2)
head(df2)


# check for NAs
any(is.na(df2))
#which(is.na(df2))

# remove Nas if needed --
#df2 <- na.omit(df2)
#any(is.na(df2))
#head(df2)
#str(df2)

### Get train and test data ----
levels(df2$Class) # "Other"          "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1]  1997   13
dim(test) # [1]  666  13

summary(test)
levels(test$Class)

# remove class if needed----
#test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
#test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:13)]
head(test)

#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("total.seagrass"= "Seagrass", "Turf.algae"="Algae", "Macroalgae"="Algae", "Unconsolidated"="Unvegetated", "Consolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass",  "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae", "Seagrass",  "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~Longitude + Latitude
points(tests)
# extract predicted
preds <- raster::extract(pred, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 666 obs
#preds$GBpred.Coarse.DTV <- as.factor(preds$GBpred.Coarse.DTV)
# There is no class 1 in validation data, so remove from factor ---
preds <- droplevels(preds)
str(preds) # 25 obs


##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$GBpred.Fine.DTV, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$GBpred.Fine.DTV == preds$classval,1,0) )
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


##    ##    ##    ##    ##    ##

#### 3. Calcuate maximum distance between validatio points ----

maxDist<-max(dist(cbind(preds$Longitude, preds$Latitude)))
maxDist

xy=cbind(preds$Longitude, preds$Latitude)
pgi.cor <- correlog(coords=xy, z=preds$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

##    ##    ##    ##    ##    ##
#### 4. Spatial autocorrlelation p stats ----
pgi.cor

dat_geo<-as.geodata(preds[,c("Longitude","Latitude","correct")])


##    ##    ##    ##    ##    ##

#### 5. Generate and plot a binned variogram (10 bins so maxdist /10) ----
Vario <- variog(dat_geo,max.dist=maxDist,uvec=seq(maxDist/10,maxDist,l=10))
## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)
