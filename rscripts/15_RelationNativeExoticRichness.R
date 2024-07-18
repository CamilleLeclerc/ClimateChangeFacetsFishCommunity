rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(cowplot)
library(dplyr)
library(ggh4x)
library(ggpattern)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(kableExtra)
library(lemon)
library(lme4)
library(piecewiseSEM)
library(purrr)
library(semEff)
library(stringr)
library(tidyr)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/vif_func.R")


##--------------
## LOAD DATASETS
##--------------
myload(dataset_9BSCBenthicPelagicGillnetSelectivity,
       dir = "outputs")


##----------------
## PREPARE DATASET
##----------------
summary(dataset_9BSCBenthicPelagicGillnetSelectivity)
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity
dataset.thermal.trajectories$native.richness <- dataset.thermal.trajectories$fish.richness - dataset.thermal.trajectories$nis.richness
summary(dataset.thermal.trajectories)

##----------------
## CORRELATION
##----------------
dataset.thermal.trajectories %>%
  ggplot(aes(y = native.richness, x = nis.richness)) +
  geom_point(size = 3, shape = 21, color = "black", fill = "#bdbdbd") + guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  stat_poly_line(color = "#737373", fill = "#d9d9d9") +
  stat_cor(method = "spearman", label.x = 5, label.y = 14, size = 3) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 15)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6), limits = c(0, 6)) +
  guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  labs(y = "Number of native fish species", x = "Number of exotic fish species") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, colour = "#000000"),
        axis.title = element_text(size = 14, face = "bold", colour = "#000000"))

dataset.thermal.trajectories %>%
  ggplot(aes(y = fish.richness, x = nis.richness)) +
  geom_point(size = 3, shape = 21, color = "black", fill = "#bdbdbd") + guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  stat_poly_line(color = "#737373", fill = "#d9d9d9") +
  stat_cor(method = "spearman", label.x = 5, label.y = 19, size = 3) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20), limits = c(0, 20)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6), limits = c(0, 6)) +
  guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  labs(y = "Number of native and exotic fish", x = "Number of exotic fish") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, colour = "#000000"),
        axis.title = element_text(size = 14, face = "bold", colour = "#000000"))
