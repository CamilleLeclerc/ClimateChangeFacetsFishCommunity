rm(list=ls())


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)
library(magrittr)
library(kableExtra)
library(igraph)

getwd()

source("rfunctions/misc.R")
source("rfunctions/metaweb_build.R")
source("rfunctions/plot_code_webs.R")



##--------------
## LOAD DATASETS
##--------------
myload(sampling_protocol, station, dir = "outputs")
myload(ind_size, dir = "outputs/IndividualSize")
#code_species <- unique(fish_diet_shift %>% select(species, species_name))
#colnames(code_species) <- c("code", "latin_name")
#write.table(code_species, "_files_to_send/codage_especes.txt", row.names = FALSE)

##-------------
## CODE SPECIES
##-------------
code_species_river_lake <- read.delim("data/code_species_river_lake.txt")
code_species_lake <- code_species_river_lake %>% select(sp_code, sp_lake) %>% drop_na(.)  
code_species_lake$sp_lake <- gsub(" ", "_", code_species_lake$sp_lake)
colnames(code_species_lake)[2] <- "species"
ind_size <- left_join(ind_size, code_species_lake, by = "species")
ind_size <- ind_size[,c(1:5, 8, 7)]
colnames(ind_size)[6] <- "species"
ind_size <- ind_size %>% dplyr::filter(fish >= 25 & fish <= 965)


##---------------------------
## KEEP ONLY BENTHIC GILLNETS
##---------------------------
#benthic_sampling_protocol <- sampling_protocol %>% filter(cd_engin_peche == "FB")
#length(unique(benthic_sampling_protocol$id_campagne))
#length(unique(benthic_sampling_protocol$id_prelev_poisson))
#length(unique(benthic_sampling_protocol$id_point_prelev))

#length(unique(ind_size$id_prelev_poisson))
#benthic_ind_size <- ind_size %>% dplyr::filter(id_prelev_poisson %in% benthic_sampling_protocol$id_prelev_poisson)
#length(unique(benthic_ind_size$id_prelev_poisson))


##------------------
## BUILD THE METAWEB
##------------------
myload(pred_win, fish_diet_shift, resource_diet_shift, dir = "data")


fish_diet_shift %<>%
  select(-species_name) %>%
  mutate(size_max = str_replace(size_max, "max", "Inf"),
         size_max = as.numeric(size_max))


resource_diet_shift %<>%
  select(-species)  %>%
  rename(species = "species_code")


metaweb_lake <- build_metaweb(data = ind_size,
                              species = species,
                              size = fish,
                              pred_win = pred_win,
                              beta_min = beta_min,
                              beta_max = beta_max,
                              fish_diet_shift = (fish_diet_shift),
                              low_bound = size_min,
                              upper_bound = size_max,
                              fish = fish,
                              resource_diet_shift = resource_diet_shift,
                              class_method = "percentile",
                              nb_class = 9,
                              pred_win_method = "midpoint",
                              fish_resource_method = "midpoint",
                              na.rm = TRUE,
                              replace_min_by_one = FALSE)

mysave(metaweb_lake, dir = "outputs/FoodWebs", overwrite = TRUE)

#size_class <- as.data.frame(metaweb_lake$size_class)
#head(size_class)
#colnames(size_class)[1] <- "code"
#write.table(size_class, "_files_to_send/classes_tailles_especes.txt", row.names = FALSE)


##-----------------
## PLOT THE METAWEB
##-----------------
sp_color <- set_color_species(node_list = colnames(metaweb_lake$metaweb),
                              species_list = metaweb_lake$species,
                              resource_list = metaweb_lake$resource)

names(sp_color)[names(sp_color) %in% metaweb_lake$resource] <- c("detritivore", "biofilm", "phytobenthos", "macrophage", "phytoplankton", "zooplankton", "zoobenthos")
node_sp <- str_remove(colnames(metaweb_lake$metaweb), "_\\d+")

TL <- NetIndices::TrophInd(metaweb_lake$metaweb)$TL

PlotWeb(TL = TL,
        webTL = metaweb_lake$metaweb,
        colnode = sp_color[node_sp],
        abund = 6,
        collink = "grey90",
        scale_abun = .01)

title("Lake Metaweb")

legend(x = "bottom",
       inset = -.07,
       legend = names(sp_color),
       pch = 21,
       col = "#777777",
       pt.bg = sp_color,
       pt.cex = 0.7,
       cex = .8,
       bty = "n",
       x.intersp = 1.5,
       text.width = .1,
       ncol = 6)


##------------------------
## METAWEB CHARACTERISTICS
##------------------------
ind_lake <- as.data.frame(metaweb_lake$metaweb %>% NetIndices::GenInd() %>% c(., Tlvl = mean(NetIndices::TrophInd(metaweb_lake$metaweb)$TL)))
ind_net_to_keep <- c("N", "Ltot", "LD", "C", "Tlvl")
ind_lake <- ind_lake %>% select(ind_net_to_keep)
ind_lake


get_prop_pisc <- function(metaweb = NULL) { mask_fish_metaweb <- str_remove(colnames(metaweb$metaweb), "_\\d+") %in% metaweb$species
                                            fish_fish_metaweb <- metaweb$metaweb[mask_fish_metaweb, mask_fish_metaweb]
                                            # Nb of piscivores:
                                            tibble(nb_pisc = length(which(colSums(fish_fish_metaweb) > 0)),
                                                   prop_pisc = length(which(colSums(fish_fish_metaweb) > 0)) / length(colSums(fish_fish_metaweb)))
                                            }
get_prop_pisc(metaweb_lake)
