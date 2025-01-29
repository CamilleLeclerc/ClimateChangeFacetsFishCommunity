rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(dplyr)
library(lmerTest)
library(ggh4x)
library(ggpubr)
library(rstatix)

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
dataset.thermal.trajectories <- dataset.thermal.trajectories %>% dplyr::filter(typo_pla %in% c("R", "LN"))



#Exotic species richness vs. lake type
modele <- lmer(nis.richness ~ typo_pla + (1|lake.code), data = dataset.thermal.trajectories)
modele
summary(modele)
anova(modele)


fixed_effects <- data.frame(
  typo_pla = c("(Intercept)", "typo_plaR"),
  Estimate = c(1.3958, 0.7252),
  Std_Error = c(0.1513, 0.1712),
  p_value = c("< 2e-16", "3.37e-05")
)

ggplot(dataset.thermal.trajectories, aes(x = typo_pla, y = nis.richness)) +
  geom_boxplot(aes(fill = typo_pla), alpha = 0.5, outlier.shape = NA) +
  geom_text(
    data = fixed_effects %>% filter(typo_pla != "(Intercept)"),  # Exclure Intercept
    aes(
      x = 1.5,
      y = 7,
      label = paste0("Estimate: ", round(Estimate, 2), " ; Std Error: ", round(Std_Error, 2), " ; p value: ", p_value)
    ),
    size = 4, hjust = 0.5, color = "black"
  ) +
  labs(x = "Type",
      y = "Exotic species richness") +
  guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  #scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 60)) + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#f0f0f0", "#636363"))

##
##modele_connectance <- lmer(connect ~ nis.richness * typo_pla + (1 | lake.code), data = dataset.thermal.trajectories)
##summary(modele_connectance)
##modele_maxtl <- lmer(maxtl ~ nis.richness * typo_pla + (1 | lake.code), data = dataset.thermal.trajectories)
##summary(modele_maxtl)

