rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(cowplot)
library(grid)
library(tidyverse)

##FUNCTIONS##
source("./rfunctions/misc.R")


##--------------
## LOAD DATASETS
##--------------
myload(sem_effect_cold_lakes, sem_effect_moderate_lakes, sem_effect_warm_lakes,
       dir = "outputs/SEM")


##-----------------
## PREPARE DATASETS
##-----------------
sem_effect_cold_lakes$lake.type <- "Cold"
sem_effect_moderate_lakes$lake.type <- "Moderate"
sem_effect_warm_lakes$lake.type <- "Warm"
data.effect <- rbind(sem_effect_cold_lakes, sem_effect_moderate_lakes, sem_effect_warm_lakes)
str(data.effect)
data.effect$SymbSign <- factor(data.effect$SymbSign, levels = c('*', ''), ordered = TRUE) 
data.effect$significant <- factor(data.effect$significant, levels = c('TRUE', 'FALSE'), ordered = TRUE) 
str(data.effect)
unique(data.effect$Predictor)


##--------
## FIGURES
##--------

##FIGURE ABOUT TOTAL EFFECTS
pClimWarm <- ggplot(data.effect %>% filter(Predictor == "Climate warming" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 6.5, xmax = 7.5, fill = "grey", alpha = 0.25)
#pClimWarm
#5 x 10


pClimCond <- ggplot(data.effect %>% filter(Predictor == "Climatic conditions" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "grey", alpha = 0.25)
#pClimCond
#5 x 10


pTotSR <- ggplot(data.effect %>% filter(Predictor == "Total sp. richness" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pTotSR
#5 x 10


pExoSR <- ggplot(data.effect %>% filter(Predictor == "Exotic sp. richness" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25)
#pExoSR
#5 x 10


pElev <- ggplot(data.effect %>% filter(Predictor == "Elevation" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pElev 
#5 x 10


pSlope <- ggplot(data.effect %>% filter(Predictor == "Slope" & Type == "Total")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pSlope 
#5 x 10

plot_grid(pClimCond, pClimWarm, 
          pExoSR, pTotSR,
          pSlope, pElev,
          labels = c("A", "B",
                     "C", "D",
                     "E", "F"),
          ncol = 2, nrow = 3, align = "hv")
#6 x 8
rm(pClimCond, pClimWarm, 
   pExoSR, pTotSR,
   pSlope, pElev)





##FIGURE ABOUT DIRECT EFFECTS
pClimWarm <- ggplot(data.effect %>% filter(Predictor == "Climate warming" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 6.5, xmax = 7.5, fill = "grey", alpha = 0.25)
#pClimWarm
#5 x 10


pClimCond <- ggplot(data.effect %>% filter(Predictor == "Climatic conditions" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "grey", alpha = 0.25)
#pClimCond
#5 x 10


pTotSR <- ggplot(data.effect %>% filter(Predictor == "Total sp. richness" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pTotSR
#5 x 10


pExoSR <- ggplot(data.effect %>% filter(Predictor == "Exotic sp. richness" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25)
#pExoSR
#5 x 10


pElev <- ggplot(data.effect %>% filter(Predictor == "Elevation" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pElev 
#5 x 10


pSlope <- ggplot(data.effect %>% filter(Predictor == "Slope" & Type == "Direct")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pSlope 
#5 x 10

plot_grid(pClimCond, pClimWarm, 
          pExoSR, pTotSR,
          pSlope, pElev,
          labels = c("A", "B",
                     "C", "D",
                     "E", "F"),
          ncol = 2, nrow = 3, align = "hv")
#6 x 8
rm(pClimCond, pClimWarm, 
   pExoSR, pTotSR,
   pSlope, pElev)





##FIGURE ABOUT INDIRECT EFFECTS
pClimWarm <- ggplot(data.effect %>% filter(Predictor == "Climate warming" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 6.5, xmax = 7.5, fill = "grey", alpha = 0.25)
#pClimWarm
#5 x 10


pClimCond <- ggplot(data.effect %>% filter(Predictor == "Climatic conditions" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .35, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#8d96a3") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 5.5, xmax = 6.5, fill = "grey", alpha = 0.25)
#pClimCond
#5 x 10


pTotSR <- ggplot(data.effect %>% filter(Predictor == "Total sp. richness" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pTotSR
#5 x 10


pExoSR <- ggplot(data.effect %>% filter(Predictor == "Exotic sp. richness" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .25, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#66a182") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 0.5, xmax = 1.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2.5, xmax = 3.5, fill = "grey", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 3.5, xmax = 4.5, fill = "white", alpha = 0.25) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 4.5, xmax = 5.5, fill = "grey", alpha = 0.25)
#pExoSR
#5 x 10


pElev <- ggplot(data.effect %>% filter(Predictor == "Elevation" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pElev 
#5 x 10


pSlope <- ggplot(data.effect %>% filter(Predictor == "Slope" & Type == "Indirect")) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_errorbar(aes(x = Response, ymin = LowerCI, ymax = UpperCI, alpha = SymbSign),
                width = .15, size = .6, color = "black") +
  geom_point(aes(x = Response, y = Effect, alpha = SymbSign), size = 3, shape = 21, color = "black", fill = "#edae49") +
  scale_alpha_manual(values = c(1, .3)) +
  facet_wrap(~lake.type) +
  coord_flip() +
  ylab("Standardised effect") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 14, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 16, face = "bold", colour = "#000000"),
    strip.text.x = element_text(size = 16, face = "bold", colour = "#FFFFFF"),
    strip.background = element_rect(color = "black", fill="#737373", linetype = "solid"),
    axis.ticks = element_line(size = 0.5, color = "black"), 
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA)) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1.5, xmax = 2.5, fill = "grey", alpha = 0.25)
#pSlope 
#5 x 10

plot_grid(pClimCond, pClimWarm, 
          pExoSR, pTotSR,
          pSlope, pElev,
          labels = c("A", "B",
                     "C", "D",
                     "E", "F"),
          ncol = 2, nrow = 3, align = "hv")
#6 x 8

