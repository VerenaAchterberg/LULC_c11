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
setwd("/home/verena/Documents/Project/WorkSpace/Data")

municipality_raster <- rast("Municipalities/IBGEmun_clipped_LULCMB_raster.tif")
names(municipality_raster)

BinaryFiles_forest <- list.files(
  "Forest_Binary",
  pattern = "forest.*_binary\\.tif$",
  full.names = TRUE
)

BinaryFiles_forest
r0 <- rast(BinaryFiles_forest[1]) #loads first raster
r0                                #check file

crs(r0)
same.crs(r0,municipality_raster) #if it wasn't TRUE follow steps below
crs(municipality_raster)
      #muni_rast <- project(muni_rast, crs(r0))
      #same.crs(r0,muni_rast)


      #sum forest pixel for each municipality

r <- rast(BinaryFiles_forest[1])

z <- zonal(
  r,
  municipality_raster,
  fun = "sum",
  na.rm = TRUE
)

head(z)
print(z)


results_mun_pixel <- list()     #create empty list

  # loop all binary files (results in ha, but pixel column should be there too)
for (f in BinaryFiles_forest) {
  r <- rast(f)
  year <- sub(
    "^forest([0-9]{4}).*",
    "\\1",
    basename(f)
  )
  z <- zonal(
    r,
    municipality_raster,
    fun = "sum",
    na.rm = TRUE
  )
  names(z) <- c("CD_MUN", "forest_pixels")
  z$forest_ha <- z$forest_pixels * 0.09
  z$year <- year
  results_mun_pixel[[year]] <- z
  cat("Finished:", year, "\n")
}

completed_years <- names(results_mun_pixel) #check results
completed_years

#--------------------- only run this is something failed
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
#----------------------------------

forest_by_year_ha <- do.call(rbind, results_mun_pixel)
print(forest_by_year_ha)


getwd()
setwd("/home/verena/Documents/Project/WorkSpace/Results")

write_xlsx(
  forest_by_year_ha,
  "Script3_forest_municipality.xlsx")

##here (also plot all municipalities here?), but also first push
#wide table with pixel value
forest_by_year_pixel_wide <- pivot_wider(
  forest_by_year_ha,
  id_cols = CD_MUN,
  names_from = year,
  values_from = forest_pixels
)

#wide table with ha value
forest_by_year_ha_wide <- pivot_wider(
  forest_by_year_ha,
  id_cols = CD_MUN,
  names_from = year,
  values_from = forest_ha
)


print(forest_by_year_pixel_wide)    ##points behind values mean that these are numeric values that are whole numbers
print(forest_by_year_ha_wide)

write_xlsx(forest_by_year_pixel_wide,
          "Script3_forest_municipality_pixel_wide.xlsx")
write_xlsx(forest_by_year_ha_wide,
           "Script3_forest_municipality_ha_wide.xlsx")


# plot all municipalities separately

ggplot(forest_by_year_ha,
       aes(x = year,
           y = forest_ha,
           group = CD_MUN)) +
  geom_line(alpha = 0.6, linewidth = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Forest cover (ha)",
    title = "Forest cover change of all municipalities"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma) +


#relative forest cover change of each municipality compared to first year

forest_long <- forest_by_year_ha %>%
  group_by(CD_MUN) %>%
  mutate(
    forest_rel = forest_ha / first(forest_ha)
  )

ggplot(forest_long,
       aes(year, forest_rel, group = CD_MUN)) +
  geom_line(alpha = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 







