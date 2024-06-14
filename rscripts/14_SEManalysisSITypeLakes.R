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


##-----------------------------------------------------------------------------------------------------------------------------------------------
## CATEGORIZE LAKES (e.g. COLD, MODERATE, WARM) TO SEE IF WE OBSERVE THE SAME EFFECTS OF WARMING IN LAKES THAT ARE ALREADY WARM VERSUS COLD LAKES
##-----------------------------------------------------------------------------------------------------------------------------------------------
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.25, 0.75))
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.3, 0.6))
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.33333, 0.6666))

dataset.thermal.trajectories$type.quant.33.66 <- NA
dataset.thermal.trajectories$type.quant.33.66[dataset.thermal.trajectories$bio1.current <= 13.05480] <- "cold"
dataset.thermal.trajectories$type.quant.33.66[dataset.thermal.trajectories$bio1.current >= 14.57316] <- "warm"
dataset.thermal.trajectories$type.quant.33.66[is.na(dataset.thermal.trajectories$type.quant.33.66)] <- "moderate"
summary(as.factor(dataset.thermal.trajectories$type.quant.33.66))


dataset.thermal.trajectories %>%
  ggplot(aes(x = bio1.current), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#8d96a3", fill = "#8d96a3") +
  geom_density(size = 1, color = "#8d96a3", adjust = 2) +
  geom_segment(x = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.33333), xend = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.33333), y = -Inf, yend = 0.3, linetype = "dashed", size = 1) +
  geom_segment(x = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.66666), xend = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.66666), y = -Inf, yend = 0.3, linetype = "dashed", size = 1) +
    labs(y = "Density", x = "Climatic condition (°C)") +
  scale_x_continuous(breaks = c(5, 10, 15, 20), limits = c(5, 20)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3), limits = c(0, 0.3)) +
  guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, colour = "#000000"),
        axis.title = element_text(size = 14, face = "bold", colour = "#000000"))


##---------------
## MULTIGROUP SEM
##---------------
