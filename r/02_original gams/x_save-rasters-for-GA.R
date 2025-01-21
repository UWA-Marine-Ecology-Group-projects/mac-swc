library(terra)
library(tidyverse)

broad_fish_predictions <- readRDS("output/fish gamms/broad_fish_predictions.rds") %>%
  dplyr::select(x, y, p_legal.fit, p_legal.se.fit) %>%
  rast(crs = "EPSG:32750") 
plot(broad_fish_predictions)

writeRaster(broad_fish_predictions, paste0("output/fish gamms/swc_", names(broad_fish_predictions), ".tif"), overwrite=TRUE)
