rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES
library(cowplot)
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
myload(network_lake_metrics, dir = "./outputs/FoodWebs")
dfspeciestype <- read.delim("./data/Species_type.txt")
code_species_river_lake <- read.delim("data/code_species_river_lake.txt")
myload(dataset_9BSCBenthicPelagicGillnetSelectivity, dir = "outputs")


##----------------
## PREPARE DATASET
##----------------
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity

code_species_river_lake <- code_species_river_lake %>% dplyr::select(sp_code, sp_lake) %>% drop_na()
colnames(code_species_river_lake) <- c("code", "species")

colnames(dfspeciestype)[1] <- "species"
dfspeciestype <- left_join(dfspeciestype, code_species_river_lake, by = 'species')
dfspeciestype[42, 3] <- "TRF"

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


##MEAN SIZE OF SPECIES
species_mean_size <- ind_size %>%
  select(species, fish) %>%
  group_by(species) %>%
  summarise(mean_size = mean(fish)/10)
quantile(species_mean_size$mean_size, na.rm = T, probs = c(0.33333, 0.6666))
species_mean_size$size_category <- NA
species_mean_size$size_category[species_mean_size$mean_size <= 12.5] <- "< 12.5 cm"
species_mean_size$size_category[species_mean_size$mean_size >= 25] <- "> 25 cm"
#species_mean_size$size_category[species_mean_size$mean_size > 10 & species_mean_size$mean_size <= 15] <- "10-15 cm"
#species_mean_size$size_category[species_mean_size$mean_size > 15 & species_mean_size$mean_size <= 20] <- "15-20 cm"
#species_mean_size$size_category[species_mean_size$mean_size > 20 & species_mean_size$mean_size <= 25] <- "20-25 cm"
species_mean_size$size_category[is.na(species_mean_size$size_category)] <- "12.5-25 cm"


##OCCURENCE OF SPECIES
species_occurrence <- ind_size %>% select(code_lac, species) %>% unique(.)
species_occurrence$presence <- 1
species_occurrence <- species_occurrence %>%
  select(species, presence) %>%
  group_by(species) %>%
  summarise(occurrence = sum(presence))
species_occurrence$percentage <- (species_occurrence$occurrence / nrow(ind_size %>% select(code_lac) %>% unique(.)))*100


##NUMBER OF TIMES SPECIES REACH THE MAXIMUM TROPHIC LEVEL 
length(unique(network_lake_metrics$id_campagne)) ; length(unique(dataset.thermal.trajectories$id.samp))
network_lake_metrics <- network_lake_metrics %>% dplyr::filter(id_campagne %in% dataset.thermal.trajectories$id.samp)

species_maxTL <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(species_maxTL) <- c("id_campagne", "trophic_species", "code_species", "species", "TL")

for(i in 361:nrow(network_lake_metrics)){
#issue i = 18 ; 269 ; 360

sub <- network_lake_metrics[[6]][[i]]
sub <- sub %>% top_n(1, obs_troph_level)

sub_species_maxTL <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(sub_species_maxTL) <- c("id_campagne", "trophic_species", "code_species", "species", "TL")
sub_species_maxTL[1, 1] <- network_lake_metrics[[1]][[i]]
sub_species_maxTL[1, 2] <- sub$species_name[2]
sub_species_maxTL[1, 3] <- substr(sub$species_name[2], 0, 3)
sub_species_maxTL[1, 4] <- dfspeciestype %>% dplyr::filter(code == substr(sub$species_name[2], 0, 3)) %>% dplyr::select(species)
sub_species_maxTL[1, 5] <- sub$obs_troph_level[2]

species_maxTL <- rbind(species_maxTL, sub_species_maxTL)

rm(sub, sub_species_maxTL)

}
rm(i)

species_maxTL$count <- 1

maxTL <- species_maxTL %>%
  select(species, count) %>%
  group_by(species) %>%
  summarise(maxTL = sum(count))
maxTL$maxTL.percentage <- (maxTL$maxTL/length(unique(dataset.thermal.trajectories$id.samp)))*100


##COMBINE ALL INFORMATIONS
df <- left_join(dfspeciestype, species_mean_size, by = 'species')
df <- left_join(df, species_occurrence, by = 'species')
df <- df %>% drop_na()
head(df)


## PLOT NATIVE SPECIES
# Filter data for native species only
df.native <- df %>% filter(type == "native")

# Reorder species by their percentage values
df.native <- df.native %>% mutate(species = fct_reorder(species, percentage, .desc = FALSE))

# Create the plot for native species only
native <- ggplot(df.native , aes(y = species, x = percentage, fill = size_category)) +
  # Add bars for the percentages
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Add rectangles to represent 100% height
  geom_rect(data = df.native  %>% distinct(species, type),
            aes(ymin = as.numeric(factor(species)) - 0.35, 
                ymax = as.numeric(factor(species)) + 0.35,
                xmin = 0, xmax = 100),
            inherit.aes = FALSE,
            color = "black", fill = NA, size = 0.5) +
  # Customize colors for size categories
  scale_fill_manual(values = c("> 25 cm" = "#979a9b", "12.5-25 cm" = "#c1a59d", "< 12.5 cm" = "#f7c8a4")) +
  # Customize labels and theme
  labs(y = NULL, x = "Percentage", fill = "Mean size") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, colour = "#000000", face = "italic"),
        axis.text.x = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
native 


## PLOT EXOTIC SPECIES
# Filter data for exotic species only
df.exotic <- df %>% filter(type == "non-indigenous")

# Reorder species by their percentage values
df.exotic <- df.exotic %>% mutate(species = fct_reorder(species, percentage, .desc = FALSE))

# Create the plot for exotic species only
exotic <- ggplot(df.exotic , aes(y = species, x = percentage, fill = size_category)) +
  # Add bars for the percentages
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Add rectangles to represent 100% height
  geom_rect(data = df.exotic  %>% distinct(species, type),
            aes(ymin = as.numeric(factor(species)) - 0.35, 
                ymax = as.numeric(factor(species)) + 0.35,
                xmin = 0, xmax = 100),
            inherit.aes = FALSE,
            color = "black", fill = NA, size = 0.5) +
  # Customize colors for size categories
  scale_fill_manual(values = c("> 25 cm" = "#979a9b", "12.5-25 cm" = "#c1a59d", "< 12.5 cm" = "#f7c8a4")) +
  # Customize labels and theme
  labs(y = NULL, x = "Percentage", fill = "Mean size") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, colour = "#000000", face = "italic"),
        axis.text.x = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
exotic 


plot_grid(native, exotic,
          labels = c("A", "B"),
          ncol = 2, nrow = 1, align = "v")
#10 x 8; portrait
