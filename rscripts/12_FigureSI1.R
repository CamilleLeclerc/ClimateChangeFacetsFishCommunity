rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(dplyr)
library(forcats)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/theme_map.R")


##--------------
## LOAD DATASETS
##--------------
myload(ind_size, dir = "./outputs/IndividualSize")
dfspeciestype <- read.delim("./data/Species_type.txt")
myload(dataset_9BSCBenthicPelagicGillnetSelectivity,
       dir = "outputs")


##----------------
## PREPARE DATASET
##----------------
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity

colnames(dfspeciestype)[1] <- "species"

length(unique(ind_size$code_lac)) ; length(unique(dataset.thermal.trajectories$lake.code))
ind_size <- ind_size %>% dplyr::filter(id_campagne %in% dataset.thermal.trajectories$id.samp)
nrow(ind_size %>% dplyr::select(code_lac, camp_annee) %>% unique(.)) ; nrow(dataset.thermal.trajectories %>% dplyr::select(lake.code, year.samp) %>% unique(.))
ind_size$species <- sub("_", " ", ind_size$species)
ind_size$species[ind_size$species == "Salmo trutta_fario"] <- "Salmo trutta"
ind_size$species[ind_size$species == "Salmo trutta_lacustris"] <- "Salmo trutta"
ind_size$species[ind_size$species == "Abramis"] <- "Abramis brama"
ind_size <- ind_size %>% filter(!(species %in% c("Hybride br�me-gardon","Hybrides de_cyprinid�s", "Percidae", "Cyprinidae", "Mugilidae")))
ind_size <- ind_size %>% filter(!(id_campagne %in% c(44, 515)))
ind_size <- ind_size %>% dplyr::filter(fish >= 25 & fish <= 965)
summary(ind_size)
ind_size <- ind_size %>% drop_na(.)
ind_size <- as.data.frame(ind_size)


species_mean_size <- ind_size %>%
  select(species, fish) %>%
  group_by(species) %>%
  summarise(mean_size = mean(fish)/10)
quantile(species_mean_size$mean_size, na.rm = T, probs = c(0.33333, 0.6666))
species_mean_size$size_category <- NA
species_mean_size$size_category[species_mean_size$mean_size <= 10] <- "< 10 cm"
species_mean_size$size_category[species_mean_size$mean_size >= 25] <- "> 25 cm"
species_mean_size$size_category[species_mean_size$mean_size > 10 & species_mean_size$mean_size <= 15] <- "10-15 cm"
species_mean_size$size_category[species_mean_size$mean_size > 15 & species_mean_size$mean_size <= 20] <- "15-20 cm"
species_mean_size$size_category[species_mean_size$mean_size > 20 & species_mean_size$mean_size <= 25] <- "20-25 cm"
species_mean_size$size_category[is.na(species_mean_size$size_category)] <- "10-25 cm"


species_occurrence <- ind_size %>% select(code_lac, species) %>% unique(.)
species_occurrence$presence <- 1
species_occurrence <- species_occurrence %>%
  select(species, presence) %>%
  group_by(species) %>%
  summarise(occurrence = sum(presence))
species_occurrence$percentage <- (species_occurrence$occurrence / nrow(ind_size %>% select(code_lac) %>% unique(.)))*100


df <- left_join(dfspeciestype, species_mean_size, by = 'species')
df <- left_join(df, species_occurrence, by = 'species')
df <- df %>% drop_na()
head(df)

# Filter data for native species only
df.native <- df %>% filter(type == "native")

# Reverse the order of species
df.native <- df.native %>% mutate(species = fct_rev(factor(species)))

# Create the plot for native species only
ggplot(df.native, aes(y = species, x = percentage, fill = size_category)) +
  # Add bars for the percentages
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Add rectangles to represent 100% height
  geom_rect(data = df.native %>% distinct(species, type),
            aes(ymin = as.numeric(factor(species)) - 0.35, 
                ymax = as.numeric(factor(species)) + 0.35,
                xmin = 0, xmax = 100),
            inherit.aes = FALSE,
            color = "black", fill = NA, size = 0.5) +
  # Customize colors for size categories
  scale_fill_manual(values = c("< 10 cm" = "#ececec", "10-15 cm" = "#717575", "15-20 cm" = "#ffc180", "20-25 cm" = "#ea7600", "> 25 cm" = "#b35c00")) +
  # Customize labels and theme
  labs(y = NULL, x = "Percentage", fill = "Mean size") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, colour = "#000000", face = "italic"),
        axis.text.x = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



