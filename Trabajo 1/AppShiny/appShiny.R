library(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, broom)

shp <- shapefile('ZONASSIT.shp')
