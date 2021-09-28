### Testing SpeciesMix script ####
# Fit Mixtures of Archetype Species # 

# Load libraries ----

#library(devtools)
#devtools::install_github('skiptoniam/ecomix')
library(ecomix)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(reshape2)
library(tidyr)
library(devtools)
library(BBmisc) # to normalize
library(corrplot)
library(tibble)


# clear workspace ----
rm(list = ls())


# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


# 1. Load data ----
df <- read.csv(paste(dt.dir, "2020_sw_maxn.env-cov.csv", sep = '/'))%>%
  mutate_at(vars(sample, family, genus, species, dataset, unique.name, full.name, location, status, cluster, cluster.new, number, n, class), list(as.factor)) %>% # make these columns as factors
  # At some point filter for successful count
  glimpse()
head(df)
str(df)
names(df) # 278 BRUV deployments
summary(df)



# 2. Remove sp that are encountered less than 2.5% of the time ----
# as per Foster et al 2015 ----
# 278 BRUV drops so far, going to work with 2.5% which is more than 2 BRUVs
head(df)
names(df)

no.bruvs <- length(levels(df$sample))
threshold <- round(no.bruvs*0.025)

# Species from wide to long --
sp.to.remove <- df %>% 
  dplyr::mutate(count = ifelse(maxn >= 1, '1', '0')) %>% # create new column with count = 1 
  #tidyr::pivot_wider(names_from = sample, values_from = count, values_fill = 0) %>% # spread and if NA then = 0
  dplyr::group_by(full.name) %>%
  #dplyr::summarise_at(vars(37:75), funs(sum)) %>%
  #dplyr::summarise_at(vars(33:42), list(~ sum(.))) %>%
  dplyr::summarise_at(vars(5), list(~ sum(.))) %>% # sumarize by max n
  dplyr::mutate(total.counts=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% # get occurrence no of BRUVS at which each sp. occurred
  dplyr::ungroup() %>%
  dplyr::arrange(total.counts) %>% # arrange ascending to see if any species found in less than 2 BRUVs
  dplyr::filter(total.counts < threshold) %>%
  dplyr::select(full.name) %>%
  glimpse() 

glimpse(sp.to.remove)
names(sp.to.remove)
to.remove <- sp.to.remove$full.name
length(sp.to.remove$full.name) # 69 species removed

# Remove species from df --
df2 <- df %>% dplyr::filter(!full.name %in% to.remove) # remove rare sp
df2 %>% count(full.name)
df2 <- df2 %>% dplyr::filter(full.name != "Unknown spp") # remove unknowns, there are 10 unknowns

df2 <- droplevels(df2) 
str(df2)  # 19035 obs, 83 species remaining

names(df2)

# 3. Make covariates in long formate using reshape2 package ----
dfl <- melt(df2,
            id.vars = names(df)[c(3:33)],
            measure.vars = names(df)[c(34:40)],
            variable.name = "covariate",
            value.name = "value"
)
head(dfl)
str(dfl)

# check for NAs in covariates
any(is.na(dfl$value))
which(is.na(dfl$value))

# check for NAs in cluster
any(is.na(dfl$cluster.new))
which(is.na(dfl$cluster.new))

# to check which BRUVs have NA cluster ----
str(dfl)
levels(dfl$sample)
ndf <- as.data.frame(cbind(as.character(dfl$sample), as.character(dfl$cluster.new)))
str(ndf)
head(ndf)
bruv.cluster <- unique(ndf[,c(1,2)])
any(is.na(bruv.cluster$V2))
which(is.na(bruv.cluster$V2)) # which ones
length(which(is.na(bruv.cluster$V2))) # how many

# Remove covars if needed ----
#levels(dfl$covariate)

#dfl <- dfl %>%
#  dplyr::filter(covariate != "SSTmean_SSTARRS" & covariate != "SSTsterr_SSTARRS" & covariate != "SSTtrend_SSTARRS") %>%
 # droplevels

#str(dfl)
#levels(dfl$covariate) #check that levels were droped
#class(dfl)

# check for NAs again --
#any(is.na(dfl$value))
#which(is.na(dfl$value))




# 4. Species data into matrix ----
pd <- table_to_species_data(
  dfl,
  site_id = "sample", # use cluster? or status?
  species_id = "full.name",
  measurement_id = "maxn"
)

pd
class(pd)
dim(pd)
# change column names for fomula because long names don't work with function --
sp.names <- colnames(pd) # col names as species names
new.names <- paste0('spp',1:83)
colnames(pd) <- new.names
pd

# save names for later
sp.names.no <- as.data.frame(cbind(sp.names, new.names))
head(sp.names.no)

# 5. Covariate data into wide df ----
names(dfl)

cd <- reshape(dfl[,c(1,32:33)], idvar = "sample", timevar = "covariate", direction = "wide")

cd
class(cd)
colnames(cd)
dim(cd)

cdm <- as.matrix(cd)

# 6. Standarize the covariates ----
cd.stand <- BBmisc::normalize(cd, method = "standardize", range = c(0,1))
colnames(cd.stand) <- colnames(cd)
colnames(cd.stand) <- c("sample"  , "bathy", "detrended.bathy" , "slope"  ,  "flowdir"  ,   "tri" ,       
                        "tpi"  ,       "aspect"  )
dim(cd.stand)

# use only a set of covariates for the moment --
#cd.stand <- cd.stand[,c(2:13)]
#colnames(cd.stand) 



# 7. Calculate correlation coeficient between covariates ----

# Plot predictors correlations by class --

### Check Predicitor correlations ---

# compute correlation matrix --
C <- cor(cd[,c(2:8)], use = 'complete.obs') # remove NAs because they mess up the corr matrix
head(round(C,2))

# correlogram : visualizing the correlation matrix --
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram#:~:text=Correlogram%20is%20a%20graph%20of%20correlation%20matrix.&text=In%20this%20plot%2C%20correlation%20coefficients,corrplot%20package%20is%20used%20here.
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients
corrplot(C, method="circle")
corrplot(C, method="pie")
corrplot(C, method="color")
corrplot(C, method="number", type = "upper")
corrplot(C, method="color", type = "lower", order="hclust") #  “hclust” for hierarchical clustering order is used in the following examples

# compute the p-value of correlations --
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(cd[,c(2:8)])
head(p.mat[, 1:5])

# customize correlogram --
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(C, method="color", col=col(100),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


# define function mosthighlycorrelated --
# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html

# linear correlation coefficients for each pair of variables in your data set, 
# in order of the correlation coefficient. This lets you see very easily which pair of variables are most highly correlated.

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe, use = 'complete.obs')
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


mosthighlycorrelated(cd[,c(2:8)], 30) # This results in only depth, rough and slope 4 not being correlated above 0.95


# 8. Make matrix of species and covariates ----
dd <- make_mixture_data(species_data = pd,
                        covariate_data = cd.stand) # use standarlized covariates
dd # I think this is what I need to use for the models


# 9. Optimize number of archetypes ----
# Fit species mix model using diffent number of archetypes and checking BIC --

colnames(cd.stand)
class(cd.stand)
head(cd.stand)
cd.df <- as.data.frame(cd.stand)
colnames(pd)

#sam_form <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:59), collapse = ','),") ~ bathy + slope"))
                                                    
sam_form_full <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:83),
                                                    collapse = ','),
                                                    ") ~ poly(bathy, 2, raw = TRUE) + poly(tpi, 2, raw = TRUE) + poly(flowdir, 2, raw = TRUE) + poly(aspect, 2, raw = TRUE) + poly(slope, 2, raw = TRUE)")) # raw = T will stop you from using orthogonal polynomials, which are not working yet
                                                

                                                  
sp_form <- ~1


test_model <- species_mix(
  archetype_formula = sam_form_full,
    #poly(slope, degree = 2, raw = TRUE) + poly(tpi, degree = 2, raw = TRUE) + poly(aspect, degree = 2, raw = TRUE) +
    #poly(temp_mean, degree = 2, raw = TRUE) + poly(temp_trend, degree = 2, raw = TRUE)),
  species_formula = sp_form, #stats::as.formula(~1),
  all_formula = NULL,
  data=dd,
  nArchetypes = 13,
  family = "negative.binomial",
  #offset = NULL,
  #weights = NULL,
  #bb_weights = NULL,
  #size = NULL, # for presence absence - benthic point data
  #power = NULL, # for tweedie : eg. biomass data
  control = list(), # for tuning the model if needed
  #inits = NULL, # if you have fitted the model previously: use the same values
  #standardise = FALSE, # has been removed in new update it scales
  #titbits = TRUE # could turn this off
)

BIC(test_model) # this gives a valie of BIC
print(test_model)

summary.species_mix(test_model, digits = 4)# this is not working

coef(test_model)
class(test_model)
test_model$taus


## 10. Optimize number of covariates ----
# now that you know the no. of archetypes, start removing convariates and test AIC to find the most parismonious model--

# remove one covariate at a time ----
sam_form_b <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:83),
                                                         collapse = ','),
                                          ") ~ poly(bathy, 2, raw = TRUE) + poly(aspect, 2, raw = TRUE) + poly(slope, 2, raw = TRUE)")) # raw = T will stop you from using orthogonal polynomials, which are not working yet


test_model_b <- species_mix(
  archetype_formula = sam_form_b,
  #poly(slope, degree = 2, raw = TRUE) + poly(tpi, degree = 2, raw = TRUE) + poly(aspect, degree = 2, raw = TRUE) +
  #poly(temp_mean, degree = 2, raw = TRUE) + poly(temp_trend, degree = 2, raw = TRUE)),
  species_formula = sp_form, #stats::as.formula(~1),
  all_formula = NULL,
  data=dd,
  nArchetypes = 4,
  family = "negative.binomial",
  #offset = NULL,
  #weights = NULL,
  #bb_weights = NULL,
  #size = NULL, # for presence absence - benthic point data
  #power = NULL, # for tweedie : eg. biomass data
  control = list(), # for tuning the model if needed
  #inits = NULL, # if you have fitted the model previously: use the same values
  #standardise = FALSE, # has been removed in new update it scales
  #titbits = TRUE # could turn this off
)

BIC(test_model_b) # this gives a valie of BIC
print(test_model_b)


# 11. Final model ----

sp_form <- ~1

sam_form <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:83),
                                                      collapse = ','),
                                       ") ~ poly(bathy, 2, raw = TRUE) + poly(aspect, 2, raw = TRUE) + poly(slope, 2, raw = TRUE) + poly(slope, 2, raw = TRUE)")) # raw = T will stop you from using orthogonal polynomials, which are not working yet


A_model <- species_mix(
  archetype_formula = sam_form,
  #poly(slope, degree = 2, raw = TRUE) + poly(tpi, degree = 2, raw = TRUE) + poly(aspect, degree = 2, raw = TRUE) +
  #poly(temp_mean, degree = 2, raw = TRUE) + poly(temp_trend, degree = 2, raw = TRUE)),
  species_formula = sp_form, #stats::as.formula(~1),
  all_formula = NULL,
  data=dd,
  nArchetypes = 4,
  family = "negative.binomial",
  #offset = NULL,
  #weights = NULL,
  #bb_weights = NULL,
  #size = NULL, # for presence absence - benthic point data
  #power = NULL, # for tweedie : eg. biomass data
  control = list(), # for tuning the model if needed
  #inits = NULL, # if you have fitted the model previously: use the same values
  #standardise = FALSE, # has been removed in new update it scales
  #titbits = TRUE # could turn this off
)

## Check model fit ----

BIC(A_model) # this gives a valie of BIC
print(A_model)
A_model$coefs
A_model$alpha
A_model$beta
A_model$lofl
A_model$gamma
A_model$delta
A_model$theta


# look at the partial response of each covariate using:
par(mfrow=c(1,2))
eff.df <- effectPlotData(focal.predictors = c("bathy","slope","aspect"), mod = A_model)
plot(x = eff.df, object = A_model)


# 12. Probability of each sp. belonging to each archetype ----
arch_prob <- as.data.frame(A_model$taus)
head(arch_prob)
names(arch_prob)
head(sp.names.no)

arch <- cbind(arch_prob, sp.names.no)
head(arch)

# Get archetype with maximum probability for each sp --
arch2 <- arch_prob %>%
  tibble::rownames_to_column() %>% # 1. add row ids as a column
  gather(column, value, -rowname) %>%
  dplyr::group_by(rowname) %>%
  dplyr::filter(rank(-value) == 1) %>%
  glimpse()

head(arch2)
str(arch2)
arch2 <- as.data.frame(arch2)
names(arch2) <- c('new.names', 'archetype', 'prob')

arch3 <- arch2 %>%
  dplyr::left_join(sp.names.no) %>%
  dplyr::mutate_at(vars(archetype), list(as.factor)) %>%
  glimpse()

str(arch3)
summary(arch3)

# to save this list --
#write.csv(arch3, paste(o.dir, "species_archetypes.csv", sep ='/'))




# 13. Plots ----

plot(A_model)

preds <- c("bathy", "slope", "aspect")


ef.plot <- effectPlotData(preds, A_model)
head(ef.plot)

ef.plot$bathy
plot(ef.plot, A_model)


sp.boot <- species_mix.bootstrap(
  A_model,
  nboot = 100,
  type = "BayesBoot",
  mc.cores = 2,
  quiet = FALSE
)


plot(ef.plot,
     A_model, 
     sp.boot,
     )


# still not sure what to use this for --
sp.var <- vcov(
  A_model,
  sp.boot = sp.boot,
  method = "BayesBoot",
  nboot = 10,
  mc.cores = 2
)




## 14. Predict ----

# load predictors --

# bathy --
b <- raster(paste(r.dir, "SW_bathy-to-260m.tif", sep='/'))
plot(b)

# derivatives --
d <- stack(paste(r.dir, "SW_detrendend.derivatives-to-260m.tif", sep='/'))
plot(d)
names(d)
n <- read.csv(paste(r.dir, "names.det.bath.csv", sep='/'))
n
n$covs <- c("detrended.bathy", "slope", "flowdir", "tri", "tpi", "aspect")
names(d) <- n[,3]

# crop bathy to stack --
b2 <- crop(b, d)

# stack preds --
d2 <- stack(d$slope, d$aspect, b2)
plot(d2)

d3 <- as.data.frame(d2, xy = TRUE)
dim(d3)
head(d3)
names(d3) <- c('x', 'y', 'slope', 'aspect', 'bathy')
str(d3)
any(is.na(d3$slope))
length(which(is.na(d3$slope)))
d3 <- na.omit(d3)
str(d3)


# predict ##
ptest <- predict(
  A_model,
  sp.boot,
  d3[,c(3:5)],
  mc.cores = 2,
  prediction.type = "archetype"
)


class(ptest)
str(ptest$ptPreds)
head(ptest$ptPreds)
ptest$

Apreds <- ptest$ptPreds
head(Apreds)

SAMpreds <- cbind(d3, Apreds)
head(SAMpreds)


coordinates(SAMpreds) <- ~x+y
A1 <- SAMpreds[,4]
A2 <- SAMpreds[,5]
A3 <- SAMpreds[,6]
A4 <- SAMpreds[,7]

gridded(A1) <- TRUE
gridded(A2) <- TRUE
gridded(A3) <- TRUE
gridded(A4) <- TRUE

A1preds <- raster(A1)
A2preds <- raster(A2)
A3preds <- raster(A3)
A4preds <- raster(A4)

Allpreds <- stack(A1preds, A2preds, A3preds, A4preds)
plot(Allpreds)

# PREDICT USING STAND COVS ----

## Standarize the covariates --
d3.stand <- BBmisc::normalize(d3[,c(3:5)], method = "standardize", range = c(0,1))
colnames(d3.stand) <- colnames(d3[,c(3:5)])
dim(d3.stand)
head(d3.stand)


ptest2 <- predict(
  A_model,
  sp.boot,
  d3.stand,
  prediction.type = "archetype"
)

class(ptest2)
str(ptest2$ptPreds)
head(ptest2$ptPreds)
  
Bpreds <- ptest2$ptPreds
head(Bpreds)

SAMpreds <- cbind(d3, Bpreds)
head(SAMpreds)


coordinates(SAMpreds) <- ~x+y
A1 <- SAMpreds[,4]
A2 <- SAMpreds[,5]
A3 <- SAMpreds[,6]
A4 <- SAMpreds[,7]

gridded(A1) <- TRUE
gridded(A2) <- TRUE
gridded(A3) <- TRUE
gridded(A4) <- TRUE

A1preds <- raster(A1)
A2preds <- raster(A2)
A3preds <- raster(A3)
A4preds <- raster(A4)

AllpredsB <- stack(A1preds, A2preds, A3preds, A4preds)
plot(AllpredsB)

#####################

