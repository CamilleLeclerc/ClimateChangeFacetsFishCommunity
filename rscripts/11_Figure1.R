rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(biscale)
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
library(lme4)
library(purrr)
library(rnaturalearth)
library(rstatix)
library(sf)
library(stringr)
library(tidyr)
library(tidyverse)
library(watina)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/theme_map.R")


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


##----------------------------------
## CORRELATION TEMPERATURE VARIABLES
##----------------------------------
#pTemp <- dataset.thermal.trajectories %>%
#  ggplot(aes(y = bio1.current, x = bio1.slope.40y*10)) +
#  geom_point(size = 4, stroke = 1.5, color = "#8d96a3", fill = "#8d96a3", alpha = 0.75) +
#  geom_smooth(method = lm, fill = "#626972", color = "#626972", alpha = 0.25) +
#  labs(y = "Current annual\nmean temperature (°C)", x = "Trend in annual mean temperature (°C/dec)") +
#  scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), limits = c(-0.2, 0.8)) +
#  scale_y_continuous(breaks = c(5, 10, 15, 20), limits = c(5, 20)) +
#  coord_capped_cart(bottom = 'both', left = 'both') +
#  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
#  theme_classic() + 
#  theme(legend.position = "none",
#        axis.text = element_text(size = 14, colour = "#000000"),
#        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
#  stat_cor(method = "pearson")
#pTemp

worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
fr <- data.frame(Country = "France", Focus = "YES") 
world_joined <- left_join(worldmap, fr, by = c("name" = "Country"))

francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')

lakes <- ne_download(scale = 10, type = 'lakes', category = 'physical')

rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')

sf::sf_use_s2(FALSE)
francelakes <- st_intersection(st_as_sf(lakes), st_as_sf(francemap))
francerivers <- st_intersection(st_as_sf(rivers), st_as_sf(francemap))

coord <- dataset.thermal.trajectories %>%
  group_by(lake.code) %>%
  dplyr::summarize(lat = mean(lat),
                   long = mean(long),
                   bio1.slope.40y = mean(bio1.slope.40y),
                   bio1.current = mean(bio1.current))
str(coord)
coord.sf <- st_as_sf(coord, coords = c("long", "lat"), crs = 4326)
data <- bi_class(coord.sf, x = bio1.slope.40y, y = bio1.current, style = "quantile", dim = 3)


map <- ggplot() +
  geom_sf(data = world_joined, fill = "white", color = "black", size = 0.05) +
  geom_sf(data = francemap, fill = gray(0.9), color = "black", size = 0.25) +
  geom_sf(data = francerivers, col = '#6baed6', size = 0.25) +  
  geom_sf(data = francelakes, col = '#6baed6', fill = '#6baed6', size = 0.05) + 
  geom_sf(data = data, mapping = aes(fill = bi_class),  shape = 21, size = 1.75, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "Brown2", dim = 3) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(which_north = "true", location = "tr", height = unit(0.5, "cm"), width = unit(0.5, "cm"), style = north_arrow_orienteering(fill = c("black", "black"), text_size = 6)) +           
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  map_theme +
  theme(strip.background = element_rect(color = "black", size = 1, linetype = "solid"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + theme(legend.position = "none")
map
#5 x 5

legend <- bi_legend(pal = "Brown2",
                    dim = 3,
                    xlab = "bio1.slope.40y",
                    ylab = "bio1.current",
                    #rotate_pal = TRUE,
                    #flip_axes = TRUE,
                    size = 8)
ggdraw() + draw_plot(legend, 0.2, .65, 0.2, 0.2)
#"Bluegill", "BlueGold", "BlueOr", "BlueYl", "Brown"/"Brown2", "DkBlue"/"DkBlue2", "DkCyan"/"DkCyan2", "DkViolet"/"DkViolet2", "GrPink"/"GrPink2", "PinkGrn", "PurpleGrn", or "PurpleOr".

##-------------------------
## HISTOGRAM CSS PROPERTIES
##-------------------------
pslope <- dataset.thermal.trajectories %>%
  ggplot(aes(x = ols.slope), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#edae49", fill = "#edae49") +
  geom_density(size = 1, color = "#edae49", adjust = 2) +
  geom_segment(x = mean(dataset.thermal.trajectories$ols.slope), xend = mean(dataset.thermal.trajectories$ols.slope), y = -Inf, yend = 0.5, linetype = "dashed", size = 1) +
  labs(y = "Density", x = "Fish size spectrum slope") +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2), limits = c(-5, 2)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.5)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
                label = paste("mean = ", mean(dataset.thermal.trajectories$ols.slope) %>% round(digits = 3),
                "\nmedian = ", median(dataset.thermal.trajectories$ols.slope) %>% round(digits = 3),
                "\nmin = ", min(dataset.thermal.trajectories$ols.slope) %>% round(digits = 3),
                "\nmax = ", max(dataset.thermal.trajectories$ols.slope) %>% round(digits = 3),
                "\ns.d. = ", sd(dataset.thermal.trajectories$ols.slope) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)

pslope

pmidpoint <- dataset.thermal.trajectories %>%
  ggplot(aes(x = ols.elevation), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#edae49", fill = "#edae49") +
  geom_density(size = 1, color = "#edae49", adjust = 2) +
  geom_segment(x = mean(dataset.thermal.trajectories$ols.elevation), xend = mean(dataset.thermal.trajectories$ols.elevation), y = -Inf, yend = 0.5, linetype = "dashed", size = 1) +
  labs(y = "Density", x = "Fish size spectrum mid-point") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.5)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(dataset.thermal.trajectories$ols.elevation) %>% round(digits = 3),
                         "\nmedian = ", median(dataset.thermal.trajectories$ols.elevation) %>% round(digits = 3),
                         "\nmin = ", min(dataset.thermal.trajectories$ols.elevation) %>% round(digits = 3),
                         "\nmax = ", max(dataset.thermal.trajectories$ols.elevation) %>% round(digits = 3),
                         "\ns.d. = ", sd(dataset.thermal.trajectories$ols.elevation) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pmidpoint


##------------------------
## HISTOGRAM FW PROPERTIES
##------------------------

#CONNECTANCE
pconnect <- dataset.thermal.trajectories %>%
  ggplot(aes(x = connect), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#2e4057", fill = "#2e4057") +
  geom_density(size = 1, color = "#2e4057", adjust = 2) +
  geom_segment(x = mean(dataset.thermal.trajectories$connect), xend = mean(dataset.thermal.trajectories$connect), y = -Inf, yend = 30, linetype = "dashed", size = 1) +
  labs(y = "Density", x = "Connectance") +
  #scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2), limits = c(-5, 2)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(dataset.thermal.trajectories$connect) %>% round(digits = 3),
                         "\nmedian = ", median(dataset.thermal.trajectories$connect) %>% round(digits = 3),
                         "\nmin = ", min(dataset.thermal.trajectories$connect) %>% round(digits = 3),
                         "\nmax = ", max(dataset.thermal.trajectories$connect) %>% round(digits = 3),
                         "\ns.d. = ", sd(dataset.thermal.trajectories$connect) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pconnect


#MAXIMUM TROPHIC LEVEL
pmaxtl <- dataset.thermal.trajectories %>%
  ggplot(aes(x = maxtl), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#2e4057", fill = "#2e4057", bins = 20) +
  geom_density(size = 1, color = "#2e4057", adjust = 3) +
  geom_segment(x = mean(dataset.thermal.trajectories$maxtl), xend = mean(dataset.thermal.trajectories$maxtl), y = -Inf, yend = 3, linetype = "dashed", size = 1) +
  labs(y = "Density", x = "Maximum trophic level") +
  scale_x_continuous(breaks = c(3.5, 4, 4.5, 5), limits = c(3.5, 5)) +
  #scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(0, 6)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(dataset.thermal.trajectories$maxtl) %>% round(digits = 3),
                         "\nmedian = ", median(dataset.thermal.trajectories$maxtl) %>% round(digits = 3),
                         "\nmin = ", min(dataset.thermal.trajectories$maxtl) %>% round(digits = 3),
                         "\nmax = ", max(dataset.thermal.trajectories$maxtl) %>% round(digits = 3),
                         "\ns.d. = ", sd(dataset.thermal.trajectories$maxtl) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pmaxtl


##----------------------------------------------------
## HISTOGRAM SIZE OF NON-INDIGENOUS/INDIGENOUS SPECIES
##----------------------------------------------------
myload(ind_size, dir = "./outputs/IndividualSize")
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
NIS <- ind_size %>% dplyr::filter(type == "non-indigenous")
NS <- ind_size %>% dplyr::filter(type == "native")

psizespecies <- ind_size %>%
  ggplot(aes(x = fish/10, color = type), position = "identity") +
  geom_histogram(aes(y = stat(density), fill = type), alpha = 0.4) +
  geom_density(size = 1, adjust = 4) +
  scale_color_manual(values = c("#969696", "#66a182")) +
  scale_fill_manual(values = c("#969696", "#66a182")) +
  scale_x_break(c(40, 80)) +
  geom_vline(xintercept = mean(NIS$fish/10), linetype = "dashed", size = 1, color = "#66a182") +
  geom_vline(xintercept = mean(NS$fish/10), linetype = "dashed", size = 1, color = "#969696") +
  #geom_segment(x = mean(NIS$fish/10), xend = mean(NIS$fish/10), y = -Inf, yend = 0.15, linetype = "dashed", size = 1, color = "#66a182") +
  #geom_segment(x = mean(NS$fish/10), xend = mean(NS$fish/10), y = -Inf, yend = 0.15, linetype = "dashed", size = 1, color = "#969696") +
  labs(y = "Density", x = "Fish body size (cm)") +
  guides(x = "axis_truncated", x.sec = guide_none()) +
  #scale_x_continuous(breaks = c(3.5, 4, 4.5, 5), limits = c(3.5, 5)) +
  #scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), limits = c(0, 0.25)) +
  guides(y = "axis_truncated", y.sec = guide_none()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(#legend.box = "horizontal", legend.position = "top",
        #legend.text = element_text(size = 10, colour = "#000000"),
        #legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(NIS$fish/10) %>% round(digits = 3),
                         "\nmedian = ", median(NIS$fish/10) %>% round(digits = 3),
                         "\nmin = ", min(NIS$fish/10) %>% round(digits = 3),
                         "\nmax = ", max(NIS$fish/10) %>% round(digits = 3),
                         "\ns.d. = ", sd(NIS$fish/10) %>% round(digits = 3),
                         "\nmean = ", mean(NS$fish/10) %>% round(digits = 3),
                         "\nmedian = ", median(NIS$fish/10) %>% round(digits = 3),
                         "\nmin = ", min(NS$fish/10) %>% round(digits = 3),
                         "\nmax = ", max(NS$fish/10) %>% round(digits = 3),
                         "\ns.d. = ", sd(NS$fish/10) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
psizespecies

#https://www.datanovia.com/en/fr/lessons/test-de-wilcoxon-dans-r/
ind_size$fish <- ind_size$fish/10
ind_size %>%
  wilcox_test(fish ~ type) %>%
  add_significance()

ind_size %>% wilcox_effsize(fish ~ type, paired = FALSE)
mod.size <- glm(fish ~ type, family = Gamma(inverse), data = ind_size)
summary(mod.size)
#mod.size.diag <- glm.diag(mod.size)
#glm.diag.plots(mod.size, mod.size.diag)
#str(ind_size)
#ind_size$code_lac <- as.factor(ind_size$code_lac)
#summary(glmer(fish ~ type + (1|code_lac), family = Gamma, data = ind_size))

#ggarrange(pTemp, psizespecies, 
#          pslope, pmidpoint,
#          pconnect, pmaxtl,
#          labels = c("A", "B", "C", "D", "E", "F"),
#          ncol = 2, nrow = 3,
#          common.legend = FALSE)

plot_grid(pTemp, psizespecies, 
          pslope, pmidpoint,
          pconnect, pmaxtl,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "hv")
#landscape 10x15 or individually 3.33 x 7.50

####-------------
####-------------

nis <- ind_size %>% dplyr::filter(type == "non-indigenous") %>% dplyr::select(fish) 
native <- ind_size %>% dplyr::filter(type == "native") %>% dplyr::select(fish) 
mean(nis[,1])/mean(native[,1])

summary_ind_size <- ind_size %>%
  select(id_campagne, type, fish) %>%
  group_by(id_campagne, type) %>%
  summarise(mean_size = mean(fish),
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
summary_ind_size <- left_join(summary_ind_size, dataset.thermal.trajectories %>% dplyr::select(id.samp, fish.richness, nis.richness))

pratio.mean <- summary_ind_size %>%
  ggplot(aes(x = ratio.mean), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
  geom_density(size = 1, color = "#66a182", adjust = 2) +
  geom_segment(x = mean(summary_ind_size$ratio.mean, na.rm=TRUE), xend = mean(summary_ind_size$ratio.mean, na.rm=TRUE), y = -Inf, yend = 0.6, linetype = "dashed", size = 1) +
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


pratio.median <- summary_ind_size %>%
  ggplot(aes(x = ratio.median), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
  geom_density(size = 1, color = "#66a182", adjust = 2) +
  geom_segment(x = mean(summary_ind_size$ratio.median, na.rm=TRUE), xend = mean(summary_ind_size$ratio.median, na.rm=TRUE), y = -Inf, yend = 0.5, linetype = "dashed", size = 1) +
  labs(y = "Density", x = "Ratio of median body size of non-indigenous/native fish species per sampling event") +
  #scale_x_continuous(breaks = c(3.5, 4, 4.5, 5), limits = c(3.5, 5)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.5)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("mean = ", mean(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
                         "\nmedian = ", median(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
                         "\nmin = ", min(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
                         "\nmax = ", max(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3),
                         "\ns.d. = ", sd(summary_ind_size$ratio.median, na.rm=TRUE) %>% round(digits = 3)),
           hjust = 1, vjust = 1, 
           size = 3)
pratio.median



pratio.max <- summary_ind_size %>%
  ggplot(aes(x = ratio.max), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#66a182", fill = "#66a182") +
  geom_density(size = 1, color = "#66a182", adjust = 2) +
  geom_segment(x = mean(summary_ind_size$ratio.max, na.rm=TRUE), xend = mean(summary_ind_size$ratio.max, na.rm=TRUE), y = -Inf, yend = 1.2, linetype = "dashed", size = 1) +
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

pratio.max.nis.rich <- summary_ind_size %>%
  ggplot(aes(y = ratio.max, x = nis.richness)) +
  geom_point(size = 4, stroke = 1.5, color = "#66a182", fill = "#66a182", alpha = 0.75) +
  geom_smooth(method = lm, fill = "#66a192", color = "#66a192", alpha = 0.25) +
  labs(y = "Ratio of maximum body size of\nnon-indigenous/native fish\nspecies per sampling event", x = "Non-indigenous species richness") +
  #scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), limits = c(-0.2, 0.8)) +
  #scale_y_continuous(breaks = c(5, 10, 15, 20), limits = c(5, 20)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) +
  stat_cor(method = "pearson")
pratio.max.nis.rich

plot_grid(pratio.mean, 
          pratio.max,
          labels = c("A", "B"),
          ncol = 1, nrow = 2, align = "hv")
#10x8

summary_ind_size %>% drop_na(ratio.mean) %>% dplyr::filter(ratio.mean > 1) %>% nrow(.)
summary_ind_size %>% drop_na(ratio.mean) %>% dplyr::filter(ratio.mean <= 1) %>% nrow(.)

summary_ind_size %>% drop_na(ratio.max) %>% dplyr::filter(ratio.max > 1) %>% nrow(.)
summary_ind_size %>% drop_na(ratio.max) %>% dplyr::filter(ratio.max <= 1) %>% nrow(.)


