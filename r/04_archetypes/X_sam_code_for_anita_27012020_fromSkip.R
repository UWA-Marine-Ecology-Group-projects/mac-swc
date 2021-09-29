## Code of Anita.
## Skipton Woolley 27/1/2021
## load the ecomix package
library(ecomix)
library(raster)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(RandomFields)
set.seed(007)
lenny <- 250
xSeq <- seq( from=0, to=1, length=lenny)
ySeq <- seq( from=0, to=1, length=lenny)
X <- expand.grid( x=xSeq, y=ySeq)

#define four covariates within the study area
Mod1 <- RMgauss( var=1, scale=0.2) + RMnugget( var=0.01)
Mod2 <- RMgauss( var=1, scale=0.5) + RMnugget( var=0.01)
Mod3 <- RMgauss( var=1, scale=1)# + RMnugget( var=0.01)
Mod4 <- RMgauss( var=2, scale=0.1) + RMnugget( var=1)
simmy1 <- RFsimulate( Mod1, x=xSeq, y=ySeq)
simmy2 <- RFsimulate( Mod2, x=xSeq, y=ySeq)
simmy3 <- RFsimulate( Mod3, x=xSeq, y=ySeq)
simmy4 <- RFsimulate( Mod4, x=xSeq, y=ySeq)
X <- cbind( X, as.numeric( as.matrix( simmy1)), as.numeric( as.matrix( simmy2)),
            as.numeric( as.matrix( simmy3)), as.numeric( as.matrix( simmy4)))
X[,-(1:2)] <- apply( X[,-(1:2)], 2, scale)
colnames( X) <- c("x","y","covar1","covar2","covar3","covar4")

#plot the covariates
env <- rasterFromXYZ( X)
#plot(env, zlim=range( values( Xbrick)))
plot(env, zlim=range( values(env)))
names(env) <- c("Temperature", "Oxygen", "Depth", "Productivity")
env.df <- as.data.frame(env,xy=TRUE)
env_dat<-rasterToPoints(env)

## simulated data for model fitting
set.seed(42)
nsp <- 100
betamean <- 0.3
betabeta <- 2
betaalpha <- betamean/(1-betamean) 
alpha <- rbeta( nsp, betaalpha, betabeta) #prevalences with mean of betaalpha/(betaalpha+betabeta)
# curve( dbeta( x, betaalpha, betabeta), from=0.001, to=0.999) #the distribution that it is drawn from
alphas <- log( alpha / ( 1-alpha))  #put them on the right scale-- but note that this is conditional on all covars being zero

# now set
betas <- as.matrix(data.frame(Temperature=c(0.75,0,-0.5), Oxygen=c(0,-0.5,0),
                              Depth= c(0,0.5,-1), Productivity=c(1,0,-.5)))

# generate realisation of data for entire survey region
sim_dat <- data.frame(intercept=1,env_dat[,3:ncol(env_dat)])
sim_dat[,-1] <-scale(sim_dat[,-1])
sites<-sample(1:nrow(sim_dat), 200, replace=FALSE)
env_200<-sim_dat[sites,]
head(env_200)

## Set up model for fitting
sam_form <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:100),
                                                    collapse = ','),")~Temperature+Oxygen+Depth+Productivity"))
sp_form <- ~1
simulated_data200 <- species_mix.simulate(archetype_formula=sam_form,
                                          species_formula=sp_form,
                                          alpha = alphas,
                                          beta = betas,
                                          data = env_200,
                                          nArchetypes = 3,
                                          family = "bernoulli")


## Select species with greater than ten occurrences across all sites.
samdat_10p <- simulated_data200[,-which(colSums(simulated_data200[,1:100])<10)] 

## Archetype formula
sam_form <- as.formula(paste0(paste0('cbind(',paste(colnames(samdat_10p)[grep("spp",colnames(samdat_10p))],collapse = ", "),") ~Temperature+Oxygen+Depth+Productivity")))

## Species formula
sp_form <- ~ 1

## Fit a single model
sam_fit <- species_mix(archetype_formula = sam_form, # Archetype formula
                       species_formula = sp_form,    # Species formula
                       data = samdat_10p,            # Data
                       nArchetypes = 3,              # Number of groups (mixtures) to fit
                       family = 'bernoulli',         # Which probability distribution to use
                       control = list(quiet = TRUE))   

## Fit multiple starts over 1 to 6 groups
## This will do the multiple fits nstarts per each group
fms <- list()
grps <- 1:6
for(i in seq_along(grps)){
  fms[[i]] <- species_mix.multifit(archetype_formula = sam_form, # Archetype formula
                                   species_formula = sp_form,    # Species formula
                                   data = samdat_10p,            # Data
                                   nArchetypes = grps[i],        # Number of groups (mixtures) to fit
                                   nstart = 5,                   # Starts
                                   mc.cores = 3,                 # Number of cpu cores to use
                                   family = 'bernoulli',         # Which probability distribution to use
                                   control = list(quiet = TRUE))  # Controls   )
}

nSAMs_fm <- fms 
SAMsamp_ll <- sapply( nSAMs_fm, function(x) sapply( x, function(y) y$logl))
SAMsamp_BICs <- sapply( nSAMs_fm, function(x) sapply( x, function(y) ifelse(attributes(y)$class=="try-error",NA,ecomix:::BIC.species_mix(y))))
SAMsamp_Gs <- sapply( nSAMs_fm, function(x) sapply( x, function(y) y$G))
SAMsamp_minPosteriorSites <- cbind(39, sapply( nSAMs_fm, function(y) sapply( y, function(x) min( colSums( x$taus)))))
SAMsamp_ObviouslyBad <- which(SAMsamp_minPosteriorSites < 2)
SAMsamp_BICs[SAMsamp_ObviouslyBad] <- NA
SAMsamp_BICs <- matrix(SAMsamp_BICs,nrow = 5)
SAMsamp_minBICs <- apply( SAMsamp_BICs, 2, min, na.rm=TRUE)
df2a <- data.frame(grps=grps,bic=SAMsamp_minBICs)
df2b <- data.frame(grps=rep( grps, each=nrow( SAMsamp_BICs)),bic=as.numeric(SAMsamp_BICs))

## plot the BIC for each group
p2a <- ggplot(df2a,aes(x=grps,y=bic))+
  geom_point()+
  geom_line()+
  geom_point(data=df2b,aes(x=grps,y=bic))+
  scale_x_continuous("Number of Groups", labels = as.character(grps), breaks = grps)+
  ylab("BIC")+
  ggtitle("a)")



