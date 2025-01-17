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
library(lemon)
library(stringr)
library(tidyr)
library(tidyverse)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/theme_map.R")


##--------------
## LOAD DATASETS
##--------------
myload(dataset_9BSCBenthicPelagicGillnetSelectivity, dir = "outputs")
myload(ind_size, dir = "./outputs/IndividualSize")


##----------------
## PREPARE DATASET
##----------------
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity
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
dfspeciestype <- read.delim("./data/Species_type.txt")
colnames(dfspeciestype)[1] <- "species"


ind_size <- left_join(ind_size, dfspeciestype, by ='species')
summary(ind_size)
ind_size <- ind_size %>% drop_na(.)
ind_size <- as.data.frame(ind_size)
nis <- ind_size %>% dplyr::filter(type == "non-indigenous") %>% dplyr::select(fish) 
native <- ind_size %>% dplyr::filter(type == "native") %>% dplyr::select(fish) 
mean(nis[,1])/mean(native[,1])

summary_ind_size <- ind_size %>%
  dplyr::select(id_campagne, type, fish) %>%
  dplyr::group_by(id_campagne, type) %>%
  dplyr::summarise(mean_size = mean(fish),
            median_size = median(fish),
            max_size = max(fish))
summary_ind_size_nis <- summary_ind_size %>% dplyr::filter(type == "non-indigenous") %>% as.data.frame(.)
summary_ind_size_nis <- summary_ind_size_nis %>% dplyr::select(id_campagne, mean_size, median_size, max_size)
colnames(summary_ind_size_nis) <- c("id_campagne", "mean_size_nis", "median_size_nis", "max_size_nis")
summary_ind_size_ns <- summary_ind_size %>% dplyr::filter(type == "native") %>% as.data.frame(.)
summary_ind_size_ns <- summary_ind_size_ns %>% dplyr::select(id_campagne, mean_size, median_size, max_size)
colnames(summary_ind_size_ns) <- c("id_campagne", "mean_size_ns", "median_size_ns", "max_size_ns")
summary_ind_size <- left_join(summary_ind_size_ns, summary_ind_size_nis, by = 'id_campagne')
#summary_ind_size <- summary_ind_size %>% replace(is.na(.), 0)
summary_ind_size$ratio.mean <- summary_ind_size$mean_size_nis / summary_ind_size$mean_size_ns
summary_ind_size$ratio.median <- summary_ind_size$median_size_nis / summary_ind_size$median_size_ns
summary_ind_size$ratio.max <- summary_ind_size$max_size_nis / summary_ind_size$max_size_ns

colnames(summary_ind_size)[1] <- "id.samp"




##----------------------------------------------------
## HISTOGRAM SIZE OF NON-INDIGENOUS/INDIGENOUS SPECIES
##----------------------------------------------------
pratio.mean <- summary_ind_size %>%
  ggplot(aes(x = ratio.mean), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
  geom_density(size = 1, color = "#66a182", adjust = 2) +
  geom_segment(x = mean(summary_ind_size$ratio.mean, na.rm=TRUE), xend = mean(summary_ind_size$ratio.mean, na.rm=TRUE), y = -Inf, yend = 0.6, linetype = "dashed", size = 1) +
  geom_segment(x = 1, xend = 1, y = -Inf, yend = 0.6, linetype = "solid", size = 0.5, color = "grey") +
  labs(y = "Density", x = "Ratio of mean body size of non-indigenous/native fish species per sampling event") +
  #scale_x_continuous(breaks = c(3.5, 4, 4.5, 5), limits = c(3.5, 5)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.6)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(summary_ind_size$ratio.mean, na.rm=TRUE) %>% round(digits = 3),
                         "\nmedian = ", median(summary_ind_size$ratio.mean, na.rm=TRUE) %>% round(digits = 3),
                         "\nmin = ", min(summary_ind_size$ratio.mean, na.rm=TRUE) %>% round(digits = 3),
                         "\nmax = ", max(summary_ind_size$ratio.mean, na.rm=TRUE) %>% round(digits = 3),
                         "\ns.d. = ", sd(summary_ind_size$ratio.mean, na.rm=TRUE) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pratio.mean


pratio.max <- summary_ind_size %>%
  ggplot(aes(x = ratio.max), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
  geom_density(size = 1, color = "#66a182", adjust = 2) +
  geom_segment(x = mean(summary_ind_size$ratio.max, na.rm=TRUE), xend = mean(summary_ind_size$ratio.max, na.rm=TRUE), y = -Inf, yend = 1.2, linetype = "dashed", size = 1) +
  geom_segment(x = 1, xend = 1, y = -Inf, yend = 1.2, linetype = "solid", size = 0.5, color = "grey") +
  labs(y = "Density", x = "Ratio of maximum body size of non-indigenous/native fish species per sampling event") +
  scale_x_continuous(breaks = c(0, 1, 2, 3), limits = c(0, 3)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2), limits = c(0, 1.3)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(summary_ind_size$ratio.max, na.rm=TRUE) %>% round(digits = 3),
                         "\nmedian = ", median(summary_ind_size$ratio.max, na.rm=TRUE) %>% round(digits = 3),
                         "\nmin = ", min(summary_ind_size$ratio.max, na.rm=TRUE) %>% round(digits = 3),
                         "\nmax = ", max(summary_ind_size$ratio.max, na.rm=TRUE) %>% round(digits = 3),
                         "\ns.d. = ", sd(summary_ind_size$ratio.max, na.rm=TRUE) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pratio.max


#pratio.median <- summary_ind_size %>%
#ggplot(aes(x = ratio.median), position = "identity") +
#geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
#geom_density(size = 1, color = "#66a182", adjust = 2) +
#geom_segment(x = mean(summary_ind_size$ratio.median, na.rm=TRUE), xend = mean(summary_ind_size$ratio.median, na.rm=TRUE), y = -Inf, yend = 0.5, linetype = "dashed", size = 1) +
#geom_segment(x = 1, xend = 1, y = -Inf, yend = 0.5, linetype = "solid", size = 0.5, color = "grey") +
#labs(y = "Density", x = "Ratio of median body size of non-indigenous/native fish species per sampling event") +
#scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.5)) +
#coord_capped_cart(bottom = 'both', left = 'both') +
#theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
#theme_classic() + 
#  theme(legend.position = "none",
#        axis.text = element_text(size = 14, colour = "#000000"),
#        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
#  annotate("text", x = Inf, y = Inf, 
#           label = paste("mean = ", mean(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
#                         "\nmedian = ", median(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
#                         "\nmin = ", min(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
#                         "\nmax = ", max(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
#                         "\ns.d. = ", sd(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3)),
#           hjust = 1, vjust = 1, 
#           size = 3)
#pratio.median


#pratio.max.nis.rich <- summary_ind_size %>%
#  ggplot(aes(y = ratio.max, x = nis.richness)) +
#  geom_point(size = 4, stroke = 1.5, color = "#66a182", fill = "#66a182", alpha = 0.75) +
#  geom_smooth(method = lm, fill = "#66a192", color = "#66a192", alpha = 0.25) +
#  labs(y = "Ratio of maximum body size of\nnon-indigenous/native fish\nspecies per sampling event", x = "Non-indigenous species richness") +
  #scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), limits = c(-0.2, 0.8)) +
  #scale_y_continuous(breaks = c(5, 10, 15, 20), limits = c(5, 20)) +
#  coord_capped_cart(bottom = 'both', left = 'both') +
#  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
#  theme_classic() + 
#  theme(legend.position = "none",
#        axis.text = element_text(size = 14, colour = "#000000"),
#        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
#  stat_cor(method = "pearson")
#pratio.max.nis.rich

plot_grid(pratio.mean, 
          pratio.max,
          labels = c("A", "B"),
          ncol = 2, nrow = 1, align = "hv")
#10x8

summary_ind_size %>% drop_na(ratio.mean) %>% dplyr::filter(ratio.mean > 1) %>% nrow(.)
summary_ind_size %>% drop_na(ratio.mean) %>% dplyr::filter(ratio.mean <= 1) %>% nrow(.)

summary_ind_size %>% drop_na(ratio.max) %>% dplyr::filter(ratio.max > 1) %>% nrow(.)
summary_ind_size %>% drop_na(ratio.max) %>% dplyr::filter(ratio.max <= 1) %>% nrow(.)

