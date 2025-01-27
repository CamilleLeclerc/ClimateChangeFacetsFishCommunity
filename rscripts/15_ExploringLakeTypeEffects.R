rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(dplyr)
library(lme4)

##FUNCTIONS##
source("./rfunctions/misc.R")


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
dataset.thermal.trajectories <- left_join(dataset.thermal.trajectories, lake.type, by = "lake.code")




# Modèle linéaire mixte
modele <- lmer(nis.richness ~ typo_pla + (1|lake.code), data = dataset.thermal.trajectories)



# Résumé des résultats
summary(modele)
library(lmerTest)

anova(modele)