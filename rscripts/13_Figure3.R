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
library(lemon)
library(stringr)
library(tidyr)


##FUNCTIONS##
source("./rfunctions/misc.R")


##--------------
## LOAD DATASETS
##--------------
myload(sem_effect_all_lakes,
       dir = "outputs/SEM")


##---------------------------
## FIGURE ABOUT TOTAL EFFECTS
##---------------------------

data.total.effect <- sem_effect_all_lakes %>% dplyr::filter(Type == "Total")


pCAMT <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Climatic conditions"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.35, fill = "black") +
  geom_point(colour = "black", fill = "#8d96a3", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) 
pCAMT #4x6


pTAMT <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Climate warming"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.35, fill = "black") +
  geom_point(colour = "black", fill = "#8d96a3", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTAMT


pNISR <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Exotic sp. richness"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_point(colour = "black", fill = "#66a182", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pNISR


pTSR <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Total sp. richness"),
               aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_point(colour = "black", fill = "#66a182", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTSR


pSlope <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Slope"),
                 aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.15, fill = "black") +
  geom_point(colour = "black", fill = "#edae49", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pSlope


pMP <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Elevation"),
              aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.15, fill = "black") +
  geom_point(colour = "black", fill = "#edae49", shape = 21, size = 4) +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pMP



plot_grid(pCAMT, pTAMT, 
          pNISR, pTSR,
          pSlope, pMP,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "hv")
#landscape 10x15
