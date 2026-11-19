library(terra) 
library(sf)  

library(tidyverse)
library(tidyr)  
library(dplyr)  
library(ggplot2)  
library(landscapemetrics)

library(raster)
library(scales)

getwd()

muni_rast <- rast("AFmunicipalities_raster.tif")
names(muni_rast)

rasters <- list.files(
  path = "BinaryRaster",
  pattern = "_binary\\.tif$",
  full.names = TRUE
)

rasters

r0 <- rast(rasters[1])
r0

crs(r0)
crs(muni_rast)
same.crs(r0,muni_rast) ##if it wasn't TRUE follow steps below
##muni_rast <- project(muni_rast, crs(r0))
##same.crs(r0,muni_rast)

r <- rast(rasters[1]) ##test??

z <- zonal(
  r,
  muni_rast,
  fun = "sum",
  na.rm = TRUE
)

head(z)


results <- list()

for (f in rasters) {
  r <- rast(f)
  year <- sub(
    "^forest([0-9]{4}).*",
    "\\1",
    basename(f)
  )
  z <- zonal(
    r,
    muni_rast,
    fun = "sum",
    na.rm = TRUE
  )
  names(z) <- c("CD_MUN", "forest_pixels")
  z$forest_ha <- z$forest_pixels * 0.09
  z$year <- year
  results[[year]] <- z
  cat("Finished:", year, "\n")
}

##--------------------------fixing 2016 and 2022
r2015 <- rast("BinaryRaster/forest2015_binary.tif")
r2016 <- rast("BinaryRaster/forest2016_binary.tif")
r2022 <- rast("BinaryRaster/forest2022_binary.tif")

ext(r2015)
ext(r2016)

res(r2015)
res(r2016)

crs(r2015)
crs(r2016)

compareGeom(r2015, r2016) ##extends do not match
compareGeom(r2016, muni_rast)

r2016_aligned <- resample(
  r2016,
  muni_rast,
  method = "near"
)

compareGeom(r2016_aligned, muni_rast)

writeRaster(
  r2016_aligned,
  "BinaryRaster/forest2016_binary_aligned.tif",
  overwrite = TRUE
)


r2022_aligned <- resample(
  r2022,
  muni_rast,
  method = "near"
)

compareGeom(r2022_aligned, muni_rast)

writeRaster(
  r2022_aligned,
  "BinaryRaster/forest2022_binary_aligned.tif",
  overwrite = TRUE
)

##load list again further up!!

completed_years <- names(results)
completed_years


for (f in rasters) {
  year <- sub(
    "^forest([0-9]{4}).*",
    "\\1",
    basename(f)
  )
  if (year %in% completed_years) {
    cat("Skipping:", year, "\n")
    next
  }
  r <- rast(f)
  z <- zonal(
    r,
    muni_rast,
    fun = "sum",
    na.rm = TRUE
  )
  names(z) <- c("CD_MUN", "forest_pixels")
  z$forest_ha <- z$forest_pixels * 0.09
  z$year <- year
  results[[year]] <- z
  cat("Finished:", year, "\n")
}

##----------------------------------



forest_by_year <- do.call(rbind, results)

write.csv(
  forest_by_year,
  "forest_area_by_municipality_new.csv",
  row.names = FALSE
)


forest_wide_pixel <- pivot_wider(
  forest_by_year,
  id_cols = CD_MUN,
  names_from = year,
  values_from = forest_pixels
)

print(forest_wide_pixel)    ##points behind values mean that these are numeric values that are whole numbers

write.csv(forest_wide_pixel, "forest_area_by_municipality_wide_new_pixel.csv", row.names = FALSE)

names(forest_by_year)
print(forest_by_year)
str(forest_by_year)
head(z)
