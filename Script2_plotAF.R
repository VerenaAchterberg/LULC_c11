library(terra) 
library(sf)  

library(tidyverse)
library(tidyr)  
library(dplyr)  
library(ggplot2)  
library(landscapemetrics)

library(raster)
library(scales)

install.packages("writexl") # only once
library(writexl)
install.packages("readxl")  # only once
library(readxl)

getwd()
## setwd("/home/verena/Documents/Project/WorkSpace/Data")
## setwd("/home/verena/Documents/Project/WorkSpace")

BinaryFiles_forest <- list.files(
  "LULC_11",
  pattern = "_atlantic_forest_.*\\.tif$",
  full.names = TRUE
)

BinaryFiles_forest
print(BinaryFiles_forest)

ForestCount_pixel <- data.frame(
  year = integer(),
  forest_pixels = numeric()
)

for (f in BinaryFiles_forest) {
  r <- rast(f)
  # count forest pixels (value = 1)
  n_forest <- global(r, "sum", na.rm = TRUE)[1,1]
  # extract year from filename
  year <- as.numeric(sub(".*_(\\d{4})\\.tif$", "\\1", basename(f)))
  ForestCount_pixel <- rbind(
    ForestCount_pixel,
    data.frame(
      year = year,
      forest_pixels = n_forest
    )
  )
}


setwd("/home/verena/Documents/Project/WorkSpace/Results")

write_xlsx(
  ForestCount_pixel,
  "Script2_ForestCount_pixel.xlsx"
)

## ForestCount_pixel <- read_xlsx("Script2_ForestCount_pixel.xlsx")

ForestCount_pixel <- ForestCount_pixel[order(ForestCount_pixel$year), ]

ForestCount_pixel


ggplot(ForestCount_pixel,
       aes(x = year, y = forest_pixels)) +
  geom_line() +
  geom_point() +
  labs(
  title = "Forest cover over time",
  x = "Year",
  y = "Number of forest pixels"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = 2001, color = "deeppink") +
  geom_vline(xintercept = 2015, color = "lightsalmon") +
  scale_x_continuous(breaks = seq(min(ForestCount_pixel$year), max(ForestCount_pixel$year), by = 1)) +
  scale_y_continuous(labels = comma)

##mha instead of pixel number

ggplot(ForestCount_pixel,
       aes(x = year, y = forest_pixels)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Forest cover over time",
    x = "Year",
    y = "Forest cover (mha)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = 2001, color = "deeppink") +
  geom_vline(xintercept = 2015, color = "lightsalmon") +
  scale_x_continuous(breaks = seq(min(ForestCount_pixel$year), max(ForestCount_pixel$year), by = 1)) +
  scale_y_continuous(
    labels = function(x) round(x * 0.09 / 1e6, 1)
  )
