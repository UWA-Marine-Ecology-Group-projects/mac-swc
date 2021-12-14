###
# Project: SWC
# Data: Multibeam
# Task: crop small areas for 3d visualisation
# Name: Kingsley
# Date: Dec 2021
##

test <- raster("data/spatial/rasters/SWC_Multibeam.tif")
plot(test)

a_area <- extent(306000, 310000, 6220000, 6223000)
a_rast <- crop(test, a_area)

plot(a_rast)
writeRaster(a_rast, "output/spatial/raster/area_a_multibeam_clip.tif")

b_area <- extent(293000, 297000, 6224000, 6227000)

plot(test)
plot(b_area, add=T)

b_rast <- crop(test, b_area)
plot(b_rast)
writeRaster(b_rast, "output/spatial/raster/area_b_multimeam_clip.tif")
