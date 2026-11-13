
library(terra)
library(sf)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(landscapemetrics)
library(raster)
library(scales)
##------------------------------------------
## create binary files

getwd()
setwd("/home/verena/Documents/Project/WorkSpace")

files <- list.files(
  path = "Data/LULC_11",
  pattern = "_atlantic_forest.*\\.tif$",
  full.names = TRUE
)

files

forest_classes <- c(3, 4, 5, 6, 7, 49) ##create forest classes
for (f in files) {
  # read raster
  r <- rast(f)
  # create binary raster
  forest <- ifel(r %in% forest_classes, 1, 0)
  # extract year from filename
  year <- sub(".*([0-9]{4})\\.tif$", "\\1", basename(f))
  # output filename
  outfile <- file.path(
    "Data/Forest_Binary",
    paste0("forest", year, "_binary.tif")
  )
  # save
  writeRaster(
    forest,
    outfile,
    overwrite = TRUE
  )
  cat("Finished:", year, "
")
}

length(files)

length(list.files("BinaryRaster",
                  pattern = "_binary\.tif$")) #check number is the same and all files were saved (or look into compressing them?)
