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

FullTable <- read.csv("forest_area_by_municipality_wide_new.csv", check.names = FALSE)
head(FullTable)

nrow(FullTable)

## 1) Filter municipalities with forest loss between 1987 and 2001

# Keep only rows where 2001 < 1987
FullTable_filtered <- FullTable[FullTable$'2001' < FullTable$'1987', ]

nrow(FullTable_filtered)

# Remove 1985 and 1986 columns
FullTable_filtered <- FullTable_filtered[, !names(FullTable_filtered) %in% c("1985", "1986")]

# Check result
head(FullTable_filtered)

# Save
write.csv(
  FullTable_filtered,
  "Results_Tables/ForestLoss1987_2001_new.csv",  ##HERE!!!!!!!
  row.names = FALSE
)

##-----plot this:
year_allfrom1987 <- paste0(1987:2024)

ALL_Loss1987to2001 <- read.csv("Results_Tables/ForestLoss1987_2001_new.csv",check.names = FALSE)

ALL_Loss1987to2001_sum <- colSums(ALL_Loss1987to2001[, year_allfrom1987], na.rm = TRUE)

ALL_Loss1987to2001_sum

df_ALL_Loss1987to2001_sum <- data.frame(
  year = 1987:2024,
  forest_sum = as.numeric(ALL_Loss1987to2001_sum)
)


ggplot(df_ALL_Loss1987to2001_sum,
       aes(x = year, y = forest_sum)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Municipalities with loss between 1987 and 2001",
    x = "Year",
    y = "Forest Cover (mha)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = 2001, color = "deeppink") +
  geom_vline(xintercept = 2015, color = "lightsalmon") +
  scale_y_continuous(
    labels = function(x) x / 1e6) +
  scale_x_continuous(breaks = seq(min(df_ALL_Loss1987to2001_sum$year), max(df_ALL_Loss1987to2001_sum$year), by = 1))


## 2) split 2001 to 2015 into increase and decrease

Loss1987to2001 <- read.csv("Results_Tables/ForestLoss1987_2001_new.csv", check.names = FALSE)

#only keep years 1987 to 2015
Loss1987to2001_filtered <- Loss1987to2001[
  ,
  c("CD_MUN", paste0(1987:2015))
]

head(Loss1987to2001_filtered)

Loss2001to2015 <- Loss1987to2001_filtered[Loss1987to2001_filtered$'2001' > Loss1987to2001_filtered$'2015', ]
Gain2001to2015 <- Loss1987to2001_filtered[Loss1987to2001_filtered$'2001' < Loss1987to2001_filtered$'2015', ]

nrow(Loss2001to2015)
nrow(Gain2001to2015)

year_1987to2015 <- paste0(1987:2015)

Loss2001to2015_sum <- colSums(Loss2001to2015[, year_1987to2015], na.rm = TRUE)
Gain2001to2015_sum <- colSums(Gain2001to2015[, year_1987to2015], na.rm = TRUE)

plot_2001to2015 <- data.frame(
  year = rep(1987:2015, 2),
  forest_area = c(Loss2001to2015_sum, Gain2001to2015_sum),
  group = c(
    rep("Loss", length(Loss2001to2015_sum)),
    rep("Gain", length(Gain2001to2015_sum))
  )
)


ggplot(plot_2001to2015,
       aes(x = year, y = forest_area, color = group)) +
  geom_line(linewidth = 1) +
  labs(
  title = "Municipalities by Loss / Gain from 2001 - 2015",
  x = "Year",
  y = "Total forest area (mha)",
  color = "Group"
  ) +
  scale_color_manual(
  values = c("Loss" = "red",
             "Gain" = "darkgreen")
  ) +
  theme_minimal() +
  geom_vline(xintercept = 2001, color = "plum4", linewidth = 1) +
  scale_y_continuous(
    labels = function(x) x / 1e6) +
  scale_x_continuous(breaks = seq(min(plot_2001to2015$year), max(plot_2001to2015$year), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



## 3) split 2001 to 2024 into increase and decrease (SPLIT THESE INTO 2001 and later change!!)

Loss1987to2001 <- read.csv("Results_Tables/ForestLoss1987_2001_new.csv", check.names = FALSE)
    ##is already 1987 to 2024, no filter necessary
head(Loss1987to2001)

Loss2001to2024 <- Loss1987to2001[Loss1987to2001$'2001' > Loss1987to2001$'2024', ]
Gain2001to2024 <- Loss1987to2001[Loss1987to2001$'2001' < Loss1987to2001$'2024', ]

nrow(Loss2001to2024)
nrow(Gain2001to2024)


year_1987to2024 <- paste0(1987:2024)

Loss2001to2024_sum <- colSums(Loss2001to2024[, year_1987to2024], na.rm = TRUE)
Gain2001to2024_sum <- colSums(Gain2001to2024[, year_1987to2024], na.rm = TRUE)

plot_2001to2024 <- data.frame(
  year = rep(1987:2024, 2),
  forest_area = c(Loss2001to2024_sum, Gain2001to2024_sum),
  group = c(
    rep("Loss", length(Loss2001to2024_sum)),
    rep("Gain", length(Gain2001to2024_sum))
  )
)


ggplot(plot_2001to2024,
       aes(x = year, y = forest_area, color = group)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Municipalities by Loss / Gain from 2001 - 2024",
    x = "Year",
    y = "Total forest area (mha)",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("Loss" = "red",
               "Gain" = "darkgreen")
  ) +
  theme_minimal() +
  geom_vline(xintercept = 2001, color = "plum4", linewidth = 1) +
  geom_vline(xintercept = 2015, color = "slategrey", linewidth = 1) +
  scale_y_continuous(
    labels = function(x) x / 1e6) +
  scale_x_continuous(breaks = seq(min(plot_2001to2024$year), max(plot_2001to2024$year), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


##4) split into 2015>2001, 2024>2001 and rest

Loss1987to2001 <- read.csv("Results_Tables/ForestLoss1987_2001_new.csv", check.names = FALSE)

# Group 1
Group1 <- Loss1987to2001[Loss1987to2001$'2015' > Loss1987to2001$'2001', ]

# Remaining municipalities
Remaining <- Loss1987to2001[!(Loss1987to2001$'2015' > Loss1987to2001$'2001'), ]

# Group 2 (only among remaining)
Group2 <- Remaining[Remaining$'2024' > Remaining$'2001', ]

# Group 3 (everything left)
Group3 <- Remaining[!(Remaining$'2024' > Remaining$'2001'), ]

nrow(Group1)
nrow(Remaining)
nrow(Group2)
nrow(Group3)

year_1987to2024 <- paste0(1987:2024)

Group1_sum <- colSums(Group1[, year_1987to2024], na.rm = TRUE)
Group2_sum <- colSums(Group2[, year_1987to2024], na.rm = TRUE)
Group3_sum <- colSums(Group3[, year_1987to2024], na.rm = TRUE)

df_Groups <- data.frame(
  year = 1987:2024,
  Group1 = as.numeric(Group1_sum),
  Group2 = as.numeric(Group2_sum),
  Group3 = as.numeric(Group3_sum)
)


plot_long <- rbind(
  data.frame(year = 1987:2024,
             forest = as.numeric(Group1_sum),
             group = "2015 > 2001"),
  data.frame(year = 1987:2024,
             forest = as.numeric(Group2_sum),
             group = "2024 > 2001"),
  data.frame(year = 1987:2024,
             forest = as.numeric(Group3_sum),
             group = "Remaining")
)

ggplot(plot_long,
       aes(x = year, y = forest, color = group)) +
  geom_line(linewidth = 1) +
  labs(
  x = "Year",
  y = "Total forest area (mha)",
  color = "Group") +
  theme_minimal() +
  geom_vline(xintercept = 2001, color = "violetred1", linewidth = 1) +
  geom_vline(xintercept = 2015, color = "violetred3", linewidth = 1) +
  scale_color_manual(
    values = c("2015 > 2001" = "darkgreen",
               "2024 > 2001" = "green",
               "Remaining" = "red")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(
    labels = function(x) x / 1e6) +
  scale_x_continuous(breaks = seq(min(plot_long$year), max(plot_long$year), by = 1))

    ##just light green

df_Group2 <- data.frame(
  year = 1987:2024,
  Group2 = as.numeric(Group2_sum)
)

ggplot(df_Group2,
       aes(x = year, y = Group2, color = Group2)) +
  geom_line(linewidth = 1, color = "green") +
  labs(
    x = "Year",
    y = "Total forest area (mha)") +
  theme_minimal() +
  geom_vline(xintercept = 2001, color = "violetred1", linewidth = 1) +
  geom_vline(xintercept = 2015, color = "violetred3", linewidth = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(
    labels = function(x) x / 1e6) +
  scale_x_continuous(breaks = seq(min(df_Group2$year), max(df_Group2$year), by = 1))

head(df_Group2)

##5) compare Loss and Gain tables from 2001-2015 and 2001-2024 and see how many municipalities move site

FullTable <- read.csv("forest_area_by_municipality_wide_new.csv", check.names = FALSE)
head(FullTable)
nrow(FullTable)

Loss2001to2024 <- Loss1987to2001[Loss1987to2001$'2001' > Loss1987to2001$'2024', ]
Gain2001to2024 <- Loss1987to2001[Loss1987to2001$'2001' < Loss1987to2001$'2024', ]

Loss2001to2015 <- Loss1987to2001_filtered[Loss1987to2001_filtered$'2001' > Loss1987to2001_filtered$'2015', ]
Gain2001to2015 <- Loss1987to2001_filtered[Loss1987to2001_filtered$'2001' < Loss1987to2001_filtered$'2015', ]

status_2015 <- bind_rows(
  Loss2001to2015 %>% mutate(status_2015 = "loss"),
  Gain2001to2015 %>% mutate(status_2015 = "gain")
  ) %>%
  dplyr::select(CD_MUN, status_2015)     ##need to add dplyr:: because otherwise it tries to use different 'select'

status_2024 <- bind_rows(
  Loss2001to2024 %>% mutate(status_2024 = "loss"),
  Gain2001to2024 %>% mutate(status_2024 = "gain")
  ) %>%
  dplyr::select(CD_MUN, status_2024)

comparison <- full_join(
  status_2015,
  status_2024,
  by = "CD_MUN"
  )

  #head(comparison)

comparison %>%
  filter(status_2015 != status_2024)

comparison %>%
  filter(status_2015 == "gain",
       status_2024 == "loss") %>%
  nrow()


comparison %>%
  filter(status_2015 == "loss",
         status_2024 == "gain") %>%
  nrow()

table(comparison$status_2015,
      comparison$status_2024,
      useNA = "ifany")



##6)Map the above table

comparison_map <- comparison %>%
mutate(change = case_when(
  status_2015 == "gain" & status_2024 == "gain" ~ "Stayed gain",
  status_2015 == "loss" & status_2024 == "loss" ~ "Stayed loss",
  status_2015 == "gain" & status_2024 == "loss" ~ "Gain → Loss",
  status_2015 == "loss" & status_2024 == "gain" ~ "Loss → Gain",
  TRUE ~ NA_character_
))

comparison_map <- comparison_map %>%
  mutate(CD_MUN = as.character(CD_MUN))  #need to change before joining because one is character and one integer

AFmunicipalities_shp <- st_read("AFmunicipalities_v3.shp")
plot(st_geometry(AFmunicipalities_shp))

class(AFmunicipalities_shp$CD_MUN)
class(comparison_map$CD_MUN)


muni_comparison_map <- AFmunicipalities_shp %>%
  left_join(comparison_map, by = "CD_MUN")

ggplot(muni_comparison_map) +
geom_sf(aes(fill = change), colour = NA) +
scale_fill_manual(
  values = c(
    "Stayed gain" = "darkgreen",
    "Stayed loss" = "darkred",
    "Gain → Loss" = "lightcoral",
    "Loss → Gain" = "lightgreen"
  ),
  na.value = "grey90"
) +
labs(fill = "Forest cover change") +
theme_minimal()

st_write(muni_comparison_map, "NewShapefiles/2015vs2024/MuniComparison2015vs2024.shp")

##check coordinates
##try save and put in QGIS


colours()
