rm(list=ls())


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)
library(magrittr)
library(kableExtra)
library(igraph)
#library(sizeTrophicInteractions)
library(furrr)

getwd()
source("rfunctions/misc.R")
source("rfunctions/local_network_build.R")
source("rfunctions/metaweb_build.R")
source("rfunctions/plot_code_webs.R")


##--------------
## LOAD DATASETS
##--------------
myload(metaweb_lake, dir = "outputs/FoodWebs")
myload(sampling_protocol, station, dir = "outputs")
myload(ind_size, dir = "outputs/IndividualSize")


##------------------
## GET LOCAL NETWORK
##------------------
code_species_river_lake <- read.delim("data/code_species_river_lake.txt")
code_species_lake <- code_species_river_lake %>% select(sp_code, sp_lake) %>% drop_na(.)  
code_species_lake$sp_lake <- gsub(" ", "_", code_species_lake$sp_lake)
colnames(code_species_lake)[2] <- "species"

ind_size <- left_join(ind_size, code_species_lake, by = "species")
ind_size <- ind_size[,c(1:5, 8, 7)]
colnames(ind_size)[6] <- "species"

benthic_sampling_protocol <- sampling_protocol %>% filter(cd_engin_peche == "FB")
length(unique(benthic_sampling_protocol$id_campagne))
length(unique(benthic_sampling_protocol$id_prelev_poisson))
length(unique(benthic_sampling_protocol$id_point_prelev))

length(unique(ind_size$id_prelev_poisson))
benthic_ind_size <- ind_size %>% dplyr::filter(id_prelev_poisson %in% benthic_sampling_protocol$id_prelev_poisson)
length(unique(benthic_ind_size$id_prelev_poisson))


network_lake <- build_local_network(data = filter(benthic_ind_size, species %in% metaweb_lake$size_class$species),
                                    species = species,
                                    var = fish,
                                    group_var = id_campagne,
                                    metaweb = metaweb_lake,
                                    classes = NULL,
                                    out_format = "igraph"
                                    )

#extract specific local network
#network_lake_id154 <- network_lake %>% filter(id_campagne == 154)
#  size_ind_id154 <- as.data.frame(network_lake_id154[[2]][[1]])
#  size_ind_id154$id_campagne <- 154
#  head(size_ind_id154)
#  size_ind_id154 <- size_ind_id154 %>% select(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev, species, class_id, fish)
#  colnames(size_ind_id154)[6] <- "code" ; colnames(size_ind_id154)[8] <- "size"

#  interaction_id154 <- as.data.frame(network_lake_id154[[3]][[1]])
#  interaction_id154$code_lac <- "SCR04"
#  interaction_id154$camp_annee <- 2009
#  interaction_id154$id_campagne <- 154
#  interaction_id154 <- interaction_id154 %>% select(code_lac, camp_annee, id_campagne, from, to)

#write.table(interaction_id154, "_files_to_send/interactions_trophiques.txt", row.names = FALSE)
#write.table(size_ind_id154, "_files_to_send/tailles_individuelles.txt", row.names = FALSE)


# Check
network_lake %>%
  unnest(data) %>%
  filter(is.na(class_id))

mysave(network_lake, dir = "outputs/FoodWebs", overwrite = TRUE)


##--------------
## PLOT LOCAL NETWORK
##--------------
sp_color <- set_color_species(node_list = colnames(metaweb_lake$metaweb),
                              species_list = metaweb_lake$species,
                              resource_list = metaweb_lake$resource)

names(sp_color)[names(sp_color) %in% metaweb_lake$resource] <- c("detritivore", "biofilm", "phytobenthos", "macrophage", "phytoplankton", "zooplankton", "zoobenthos")
node_sp <- str_remove(colnames(metaweb_lake$metaweb), "_\\d+")

TL <- NetIndices::TrophInd(metaweb_lake$metaweb)$TL
net_ex <- network_lake$network[[1]] %>% graph_from_data_frame() %>% as_adjacency_matrix(., sparse = FALSE)
node_sp <- str_remove(colnames(net_ex), "_\\d+")
TL <- NetIndices::TrophInd(net_ex)$TL

PlotWeb(TL = TL,
        webTL = net_ex,
        colnode = sp_color[node_sp],
        abund = 6,
        collink = "grey90",
        scale_abun = .01)

legend(x = "bottom",
       inset = 0,
       legend = names(sp_color)[names(sp_color) %in% node_sp],
       pch = 21,
       col = "#777777",
       pt.bg = sp_color[names(sp_color) %in% node_sp],
       pt.cex = 0.7,
       cex = .5,
       bty = "n",
       x.intersp = 1,
       text.width = .1,
       ncol = 6)


##--------------
## GET NETWORK CONNECTANCE, Tmax AND RICHNESS
##--------------
plan(multicore, workers = 3)

network_lake %<>% mutate( network = future_map(network, igraph::graph_from_data_frame, directed = TRUE),
                          network = future_map(network, igraph::as_adjacency_matrix, sparse = FALSE))

# Compute generic indices
network_lake_metrics <- network_lake %>% mutate(metrics = future_map(network, NetIndices::GenInd),
                                                connectance = map_dbl(metrics, "C"),
                                                nbnode = map_dbl(metrics, "N"))
network_lake_metrics %<>% select(-data, metrics)

network_lake %<>% mutate(troph = future_map(network, NetIndices::TrophInd))
network_lake %>% select(id_campagne, troph) %>%
  unnest(troph) %>%
  filter(is.na(TL))

network_lake$troph[[1]]

# Get obs_troph_level
network_lake %<>% mutate(obs_troph_level = map(troph, function(x) { out <- tibble(species_name = row.names(x),
                                                                                  obs_troph_level = x$TL)
                                                                    return(out)
                                                                  }),
                         max_troph_lvl = map_dbl(troph, ~max(.x$TL))
                         )

network_lake_metrics %<>% left_join(select(network_lake, id_campagne, obs_troph_level, max_troph_lvl), by = "id_campagne")

mysave(network_lake_metrics, dir = "outputs/FoodWebs", overwrite = TRUE)


# Compute weighted average trophic level
myload(weight_fish_lake, dir = "outputs/FoodWebs")


size_class_weight_lake <- assign_size_class(data = weight_fish_lake %>%
                                              filter(!is.na(fish), species %in% metaweb_lake$size_class$species),
                                            species = species, var = fish,
                                            classes = metaweb_lake$size_class
                                            ) %>%
                          unite(sp_class, species, class_id, sep = "_") %>%
                          dplyr::select(id_campagne, sp_class, weight) %>%
                          group_by(id_campagne, sp_class) %>%
                          summarise(bm = sum(weight)) %>%
                          ungroup()

obs_troph_level <- network_lake_metrics %>% 
                      select(id_campagne, obs_troph_level) %>%
                      unnest() %>%
                      rename(sp_class = species_name) %>%
                      left_join(size_class_weight_lake, by = c("id_campagne", "sp_class"))

weighted_trophic_lvl <- obs_troph_level %>%
                          filter(!sp_class %in% metaweb_lake$resource) %>%
                          group_by(id_campagne) %>%
                          summarise(w_trph_lvl_avg = round(sum(obs_troph_level * bm / sum(bm)), 2))

if ("w_trph_lvl_avg" %in% colnames(network_lake_metrics)) {
  network_lake_metrics %<>% select(-w_trph_lvl_avg)
}

network_lake_metrics %<>%
  left_join(weighted_trophic_lvl, by = "id_campagne")

mysave(network_lake_metrics, dir = "outputs/FoodWebs", overwrite = TRUE)
