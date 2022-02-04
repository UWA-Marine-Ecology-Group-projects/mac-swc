###
# Project: SWC
# Data: Multibeam
# Task: crop small areas for 3d visualisation
# Name: Kingsley
# Date: Dec 2021
##
library(raster)
test <- raster("data/spatial/rasters/SWC_Multibeam.tif")
plot(test)

a_area <- extent(306000, 310000, 6220000, 6223000)
a_rast <- crop(test, a_area)

plot(a_rast)
writeRaster(a_rast, format = "GTiff", overwrite = TRUE,
            "output/spatial/raster/area_a_multibeam_clip.tif")
writeRaster(a_rast, format = "ascii", overwrite = TRUE,
            "output/spatial/raster/area_a_multibeam_clip.asc")


b_area <- extent(293000, 297000, 6224000, 6227000)

plot(test)
plot(b_area, add=T)

b_rast <- crop(test, b_area)
plot(b_rast)
writeRaster(b_rast, format = "GTiff", overwrite = TRUE,
            "output/spatial/raster/area_b_multibeam_clip.tif")
writeRaster(b_rast, format = "ascii", overwrite = TRUE,
            "output/spatial/raster/area_b_multibeam_clip.asc")
