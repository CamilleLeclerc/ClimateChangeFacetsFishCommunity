rm(list=ls())


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)
library(magrittr)
library(kableExtra)
library(igraph)
#library(sizeTrophicInteractions)

getwd()
source("rfunctions/misc.R")


##--------------
## LOAD DATASETS
##--------------
myload(sampling_protocol, station, dir = "outputs")
myload(ind_size, dir = "outputs/IndividualSize")


##----------------------------
## COMPUTE FISH BODY MASS
##----------------------------
#Compute fish body mass 
#@param length in mm numeric 
#@param gram logical TRUE (default), miligram if not

calc_fish_body_mass <- function (length = NULL, unit = "gram") {
  
  weight  <- 0.01 * (length ^ 3.03)
  
  if (unit == "gram") { weight <- weight * 10 ^ -3 } #in grams
  
  return(weight)
  
}


# Lake
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



weight_fish_lake <- benthic_ind_size %>% mutate(weight = calc_fish_body_mass(fish))

mysave(weight_fish_lake, dir = "outputs/FoodWebs", overwrite = TRUE)


##----------------------------
## GET BIOMASS BY SPECIES
##----------------------------
# Lake
community_lake <- weight_fish_lake %>% group_by(id_campagne, species) %>% #group_by(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev, species)
                    summarise(biomass = sum(weight, na.rm = TRUE),
                              length = mean(fish, na.rm = TRUE),
                              nind = n())

mysave(community_lake, dir = "outputs/FoodWebs", overwrite = TRUE)



com_lake <- community_lake %>% group_by(id_campagne) %>% #group_by(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev)
              summarise(richness = n(),
                        nind = sum(nind),
                        biomass = sum(biomass),
                        #bm_std = sum(biomass)/surface
                        )


species_vector_lake <- community_lake %>% select(id_campagne, species, biomass) %>%
                          group_by(id_campagne) %>%
                          nest() %>%
                          mutate(sp_vector = furrr::future_map(data, function(.data) { .data %<>%
                                                                                        spread(species, biomass)
                                                                                        stopifnot(nrow(.data) == 1)
                                                                                        return(unlist(.data))
                                                                                        }
                                                               ),
                                 rel_bm = furrr::future_map(sp_vector, function(.data) { return(.data / sum(.data))
                                   })
  )

community_lake_metrics <- community_lake %>% left_join(species_vector_lake, by = "id_campagne")

mysave(community_lake_metrics, dir = "outputs/FoodWebs", overwrite = TRUE)
