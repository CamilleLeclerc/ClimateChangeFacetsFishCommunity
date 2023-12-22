rm(list=ls())


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(cheddar)
library(furrr)
library(igraph)
library(kableExtra)
library(magrittr)
library(tidyverse)


getwd()
source("./rfunctions/misc.R")
source("./rfunctions/local_network_build.R")
source("./rfunctions/metaweb_build.R")
source("./rfunctions/plot_code_webs.R")
source("./rfunctions/code_network_metrics.R")


##--------------
## LOAD DATASETS
##--------------
myload(sampling_protocol, station, dir = "outputs")
myload(metaweb_lake, dir = "outputs/FoodWebs")
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
ind_size <- ind_size %>% dplyr::filter(fish >= 25 & fish <= 965)

benthic_sampling_protocol <- sampling_protocol #%>% filter(cd_engin_peche == "FB")
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


# Check
network_lake %>%
  unnest(data) %>%
  filter(is.na(class_id))

mysave(network_lake, dir = "./outputs/FoodWebs", overwrite = TRUE)


##--------------
## GET FOOD-WEB METRICS
##--------------
#https://mlurgi.github.io/networks_for_r/lesson-5.html
#https://rfrelat.github.io/BalticFoodWeb.html


#COMPUTE UNWEIGHTED METRICS
#--------------------------
network_lake_matrix <- network_lake %>% mutate( network = future_map(network, igraph::graph_from_data_frame, directed = TRUE),
                          network = future_map(network, igraph::as_adjacency_matrix, sparse = FALSE))

for(i in 1:nrow(network_lake)){

trophic_links <- network_lake[[3]][[i]]
colnames(trophic_links) <- c("resource", "consumer")
nodes <- c(trophic_links[,1], trophic_links[,2]) %>% unique(.) %>% as.data.frame(.)
colnames(nodes) <- "node"
property <- list(title = c("foodweb"))
foodweb <- Community(nodes, properties = property, trophic.links = trophic_links)

topological_metrics <- as.matrix(c(length(unique(network_lake[[2]][[i]]$species)), #Number of fish species
                                   nrow(nodes), #Number of nodes
                                   mean(network_lake[[2]][[i]]$fish, na.rm = TRUE)/10, #Average body size (cm) of the community
                                   nrow(trophic_links), #Number of links
                                   nrow(trophic_links)/nrow(nodes), #Linkage density // ?LinkageDensity #returns the NumberOfTrophicLinks / NumberOfNodes, including cannibalistic links and isolated nodes ; The number of trophic links in Community
                                   DirectedConnectance(foodweb), #Connectance // DirectedConnectance #returns NumberOfTrophicLinks / NumberOfNodes^2, including cannibalistic links and isolated nodes ; The number of trophic links in Community
                                   Generality(network_lake_matrix[[3]][[i]]), #Generality	Representing the mean number of prey species per predator
                                   Vulnerability(network_lake_matrix[[3]][[i]]), #Vulnerability : Representing the mean number of consumer species per prey species
                                   SDGenerality(network_lake_matrix[[3]][[i]]),
                                   SDVulnerability(network_lake_matrix[[3]][[i]]),
                                   FractionOfBasal(network_lake_matrix[[3]][[i]]), #report the connectivity of nodes in a food web - No resources and one or more consumers
                                   FractionOfIntermediate(network_lake_matrix[[3]][[i]]), #report the connectivity of nodes in a food web - Nodes not fitting any of the above categories (i.e. isolated / basal / top-level)
                                   FractionOfTop(network_lake_matrix[[3]][[i]]), #report the connectivity of nodes in a food web - One or more resources and no consumers, other than possibly itself
                                   Maxsim(network_lake_matrix[[3]][[i]]), #Maximum similarity: The average maximum trophic similarity across species in the network
                                   MeanFoodChainLength(network_lake_matrix[[3]][[i]]), #Mean food chain length: Average length (i.e. number of links) of all the paths (food chains) running from each basal species to each top predator species in the food web
                                   mean(PreyAveragedTrophicLevel(RemoveCannibalisticLinks(foodweb, title='community'))), # Mean trophic level according to Braga et al. 2019 GEB
                                   max(PreyAveragedTrophicLevel(RemoveCannibalisticLinks(foodweb, title='community'))), # Maximum trophic level according to Braga et al. 2019 GEB
                                   transitivity(graph.adjacency(as.matrix(network_lake_matrix[[3]][[i]]), mode="directed")), #weighted = "TRUE" ; #Clustering coefficient: Probability of linkage of two species, given that both are linked to a third species
                                   modularity(cluster_louvain(graph.adjacency(network_lake_matrix[[3]][[i]], mode = "undirected", weighted = TRUE, diag = TRUE))), #Modularity: uantifies the extent to which species within the same compartment share more interactions among themselves than with species in other compartments
                                   CalculatePredatorOverlap(network_lake_matrix[[3]][[i]])))

rownames(topological_metrics) <- c("Fish.rich",
                                   "Nodes",
                                   "Fish.size", 
                                   "No.links",
                                   "Link.dens", 
                                   "Connect", 
                                   "Gen",
                                   "Vul",
                                   "Gen.SD",
                                   "Vul.SD",
                                   "Frac.bas",
                                   "Frac.int",
                                   "Frac.top",
                                   "Max.sim",
                                   "MFCL",
                                   "MeanTL",
                                   "MaxTL",
                                   "Cluster.coef",
                                   "Modular",
                                   "PredatorOverlap")
colnames(topological_metrics) <- network_lake$id_campagne[i]
topological_metrics <- t(topological_metrics)

links <- cbind(PredatorID = NodeNameIndices(foodweb, TLPS(foodweb)[,'consumer']), PreyID = NodeNameIndices(foodweb, TLPS(foodweb)[,'resource']))
#write.table(links, file.path("outputs/FoodWebs/Web/", paste('network_ID', network_lake$id_campagne[i],'.web', sep = '')), row.names = FALSE, sep = ' ')

write.table(topological_metrics, file.path("outputs/TopologicalMetrics/", paste('topologicalmetrics_ID', network_lake$id_campagne[i],'.txt', sep = '')), row.names = TRUE, col.names = TRUE, sep = ' ')

rm(trophic_links, nodes, property, foodweb, topological_metrics, links)
}

rm(i)

