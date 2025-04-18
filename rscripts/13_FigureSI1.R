rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(cowplot)
library(dplyr)
library(ggbeeswarm)
library(ggbreak)
library(ggh4x)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(kableExtra)
library(lemon)
library(purrr)
library(rnaturalearth)
library(rstatix)
library(sf)
library(stringr)
library(tidyr)
library(tidyverse)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/theme_map.R")


##--------------
## LOAD DATASETS
##--------------
myload(dataset_9BSCBenthicPelagicGillnetSelectivity,
       dir = "outputs")
lake.type <- read.csv("./data/Lake_type.txt", sep="")


##----------------
## PREPARE DATASET
##----------------
summary(dataset_9BSCBenthicPelagicGillnetSelectivity)
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity
count.per.lake <- dataset.thermal.trajectories %>% dplyr::group_by(lake.code) %>% 
  dplyr::summarise(total_count = n(),.groups = 'drop') %>%
  as.data.frame()


##------------------------------
## SPATIAL DISTRIBUTION OF LAKES
##------------------------------
worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
fr <- data.frame(Country = "France", Focus = "YES") 
world_joined <- left_join(worldmap, fr, by = c("name" = "Country"))

francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')

lakes <- ne_download(scale = 10, type = 'lakes', category = 'physical')

rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')

sf::sf_use_s2(FALSE)
francelakes <- st_intersection(st_as_sf(lakes), st_as_sf(francemap))
francerivers <- st_intersection(st_as_sf(rivers), st_as_sf(francemap))

coord <- dataset.thermal.trajectories %>% dplyr::select(lake.code, lat, long) %>% unique(.)
coord <- left_join(coord, count.per.lake, by = 'lake.code')
coord <- left_join(coord, lake.type, by = 'lake.code')
coord <- coord %>%
  mutate(typo_pla = ifelse(typo_pla == "C", "A", typo_pla)) %>%
  mutate(typo_pla = ifelse(typo_pla == "E", "A", typo_pla))
str(coord)
coord$total_count <- as.factor(coord$total_count)
coord$typo_pla <- as.factor(coord$typo_pla)
 
map <- ggplot() +
  geom_sf(data = world_joined, fill = "white", color = "black", size = 0.05) +
  geom_sf(data = francemap, fill = gray(0.9), color = "black", size = 0.25) +
  geom_sf(data = francerivers, col = '#6baed6', size = 0.25) +  
  geom_sf(data = francelakes, col = '#6baed6', fill = '#6baed6', size = 0.05) +  
  geom_point(data = coord, aes(x = long, y = lat, fill = total_count, shape = typo_pla), colour = "black", size = 1.75) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(which_north = "true", location = "tr", height = unit(0.5, "cm"), width = unit(0.5, "cm"), style = north_arrow_orienteering(fill = c("black", "black"), text_size = 6)) +           
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")) +
  scale_shape_manual(values = c(21, 22, 24, 23))  +
  map_theme +
  theme(strip.background = element_rect(color = "black", size = 1, linetype = "solid"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + theme(legend.position = "none")
map
#5 x 5
