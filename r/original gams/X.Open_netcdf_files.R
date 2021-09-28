###  extract data from netcdf files ----

# CARS 2009 info page : http://www.marine.csiro.au/~dunn/cars2009/

# open library to work with cdf
library(ncdf4)
library(raster)
library(chron)
library(data.table) # for as.data.frame.table function
library(plyr) # for adply function to turn array into df
library(readr) # for Rds files
library(gapfill) # to convert arrays to raster
library(raster)

# set working directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "H:/Github/GB_2015_Survey"
# Set data directory - to read the data from
dt.dir <- paste(w.dir, "Data/Tidy", sep='/')
s.dir <- (paste(w.dir, "shapefiles", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
x.dir <- "G:/My Drive/Anita/NESP_D3/SST_data"

## 1. Acquire Mean SST ----

file <- nc_open(paste(x.dir,'IMOS_aggregation_20210114T033003Z_SSTAARS/IMOS_aggregation_20210114T033003Z.nc', sep='/'))
class(file)
attributes(file$var)
file$var
attributes(file$id)
nt <- file$var[[4]] # mean nitrates
nt
class(nt)
nt$varsize # 106  88
nt$ndims # 2 dimensions, lat, long 

# Get Lat and Lon ----

#lats <- ncvar_get(file,'lat')
lats <- nt$dim[[2]]
lats
class(lats) # ncdim4
lat <- lats$vals
class(lat) # array
#lat <- as.data.frame.table(lats)

#lon <- ncvar_get(file,'lon')
long <- nt$dim[[1]]
long
class(long)
lon <- long$vals
class(lon)
dim(lon)
print(lon)
lon[1]
#lon <- as.data.frame.table(lon)


### Get variable data ----

# choose the time frame you want to download
start <- c(1, 1)
count <- c(106, 88) ## gonna start with only 5 time steps
mean_var <- ncvar_get(file, nt, start, count)
class(mean_var)

nc_close(file)

# flip the dimensions of the Temp array for correct plotting --
mean_var2 <- apply(mean_var,1,rev)

# Create raster from arrays ----
r <- raster(
  mean_var2,
  xmn=range(lon)[1], xmx=range(lon)[2],
  ymn=range(lat)[1], ymx=range(lat)[2],
  crs=CRS("+init=epsg:4939")
)
r
plot(r)

writeRaster(r, paste(r.dir, "SSTmean_SSTARRS.tif", sep='/'))


## 2. Acquire Std. Err. SST ----

file <- nc_open(paste(x.dir,'IMOS_aggregation_20210114T033003Z_SSTAARS/IMOS_aggregation_20210114T033003Z.nc', sep='/'))
class(file)
attributes(file$var)
file$var
attributes(file$id)
nt <- file$var[[5]] # mean nitrates
nt
class(nt)
nt$varsize # 106  88
nt$ndims # 2 dimensions, lat, long 

# Get Lat and Lon ----

#lats <- ncvar_get(file,'lat')
lats <- nt$dim[[2]]
lats
class(lats) # ncdim4
lat <- lats$vals
class(lat) # array
#lat <- as.data.frame.table(lats)

#lon <- ncvar_get(file,'lon')
long <- nt$dim[[1]]
long
class(long)
lon <- long$vals
class(lon)
dim(lon)
print(lon)
lon[1]
#lon <- as.data.frame.table(lon)


### Get variable data ----

# choose the time frame you want to download
start <- c(1, 1)
count <- c(106, 88) ## gonna start with only 5 time steps
mean_var <- ncvar_get(file, nt, start, count)
class(mean_var)

nc_close(file)

# flip the dimensions of the Temp array for correct plotting --
mean_var2 <- apply(mean_var,1,rev)

# Create raster from arrays ----
r <- raster(
  mean_var2,
  xmn=range(lon)[1], xmx=range(lon)[2],
  ymn=range(lat)[1], ymx=range(lat)[2],
  crs=CRS("+init=epsg:4939")
)
r
plot(r)

writeRaster(r, paste(r.dir, "SSTsterr_SSTARRS.tif", sep='/'))

## 3. Acquire Std. Err. SST ----

file <- nc_open(paste(x.dir,'IMOS_aggregation_20210114T033003Z_SSTAARS/IMOS_aggregation_20210114T033003Z.nc', sep='/'))
class(file)
attributes(file$var)
file$var
attributes(file$id)
nt <- file$var[[6]] # mean nitrates
nt
class(nt)
nt$varsize # 106  88
nt$ndims # 2 dimensions, lat, long 

# Get Lat and Lon ----

#lats <- ncvar_get(file,'lat')
lats <- nt$dim[[2]]
lats
class(lats) # ncdim4
lat <- lats$vals
class(lat) # array
#lat <- as.data.frame.table(lats)

#lon <- ncvar_get(file,'lon')
long <- nt$dim[[1]]
long
class(long)
lon <- long$vals
class(lon)
dim(lon)
print(lon)
lon[1]
#lon <- as.data.frame.table(lon)


### Get variable data ----

# choose the time frame you want to download
start <- c(1, 1)
count <- c(106, 88) ## gonna start with only 5 time steps
mean_var <- ncvar_get(file, nt, start, count)
class(mean_var)

nc_close(file)

# flip the dimensions of the Temp array for correct plotting --
mean_var2 <- apply(mean_var,1,rev)

# Create raster from arrays ----
r <- raster(
  mean_var2,
  xmn=range(lon)[1], xmx=range(lon)[2],
  ymn=range(lat)[1], ymx=range(lat)[2],
  crs=CRS("+init=epsg:4939")
)
r
plot(r)

writeRaster(r, paste(r.dir, "SSTtrend_SSTARRS.tif", sep='/'))




### UP TO HERE -----




test <- as.data.frame(mean_nit)

## check these the first time
dlname <- ncatt_get(file, nt, "long_name")
dunits <- ncatt_get(file, nt, "units")  
fillvalue <- ncatt_get(file, nt, "_FillValue")  
class(mean_nit) #array


### CLOSE NETCFD FILE!
nc_close(file)


# Replace NetCDF fillvalues with R NAs in the ARRAY
mean_nit[mean_nit == fillvalue$value] <- NA
length(na.omit(as.vector(mean_nit[, , 1]))) # check how many NAs # 177687

r <- raster(
  
)



# rearrange long and lat if needed
#input_array <- aperm(mean_nit, c(2,1,3))

# transfor into data frame using adply
#create Raster stack: need to provide n for lats, longs and time
output_stack <- stack(brick(array(mean_nit, c(106, 88, 1))))
plot(output_stack)
# Save as Raster stack
writeRaster(output_stack, paste(r.dir, "test.tif", sep='/')) 
