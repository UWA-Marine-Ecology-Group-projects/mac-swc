library(terra)
library(tidyverse)

broad_fish_predictions <- readRDS("output/fish gamms/broad_fish_predictions.rds") %>%
  dplyr::select(x, y, p_legal.fit, p_legal.se.fit) %>%
  rast(crs = "EPSG:32750") 
plot(broad_fish_predictions)

writeRaster(broad_fish_predictions, paste0("output/fish gamms/swc_", names(broad_fish_predictions), ".tif"), overwrite=TRUE)

broad_habitat_predictions <- readRDS("output/habitat_fssgam/site_habitat_predictions.rds") %>%
  dplyr::select(x, y, preef.fit, preef.se.fit) %>%
  rast(crs = "EPSG:32750")
plot(broad_habitat_predictions)

writeRaster(broad_habitat_predictions, paste0("output/habitat_fssgam/swc_", names(broad_habitat_predictions), ".tif"), overwrite = TRUE)
