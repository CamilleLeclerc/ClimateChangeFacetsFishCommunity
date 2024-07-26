rm(list=ls()) #Removes all objects from the current workspace (R memory)


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(dplyr)
library(plyr)
library(reshape2)
library(tidyr)

##FUNCTIONS##
source("./rfunctions/misc.R")


##--------------
## LOAD DATASETS
##--------------

df.lakelist <- read.csv("./outputs/lake_list.txt", sep="")
colnames(df.lakelist) <- c("lake.code", "year.samp", "id.samp")

#--

df.speciestype <- read.delim("data/Species_type.txt")
colnames(df.speciestype) ; colnames(df.speciestype)[1] <- "species"

#--

myload(ind_size, dir = "outputs/IndividualSize")

head(ind_size)
df.fish <- ind_size %>% dplyr::select(id_campagne, species, fish)
colnames(df.fish) <- c("id.samp", "species", "size.mm")
sort(unique(df.fish$species))
length(unique(df.fish$species))

df.fish$species[df.fish$species == "Abramis"] <- "Abramis_brama"
df.fish$species[df.fish$species == "Salmo_trutta_fario"] <- "Salmo_trutta"
df.fish$species[df.fish$species == "Salmo_trutta_lacustris"] <- "Salmo_trutta"
df.fish$species[df.fish$species == "Hybride_br�me-gardon"] <- "Cyprinidae"
df.fish$species[df.fish$species == "Hybrides_de_cyprinid�s"] <- "Cyprinidae"
df.fish <- df.fish %>% filter(!(species %in% c("Cyprinidae", "Mugilidae", "Percidae"))) #delete these families as there are not considered in food-webs
length(unique(df.fish$species))
df.fish$species <- gsub('_', ' ', df.fish$species)

summary(df.fish)

# Supprime les individus avec taille = NA
df.fish <- df.fish %>% drop_na()

# Transformer taille de mm en cm
df.fish$size.cm = df.fish$size.mm/10

# Restriction de la gamme de taille en raison de la sélectivité des filets maillants 
# Dupagne et al. (2021) [https://doi.org/10.1111/fme.12476]:
# Size classes at the lower and upper end of the communities’ size range were excluded from the analysis to avoid bias due to,
# respectively, poor retention in the gear and too infrequent captures (i.e. juveniles or small fish can swim through meshes and largest individuals avoid nets more easily)
# (Axenrot & Hansson, 2004; Elliott & Fletcher, 2001; Mehner & Schulz, 2002) (size range considered = 2.5 - 96.5 cm).
df.fish <- df.fish %>% dplyr::filter(size.mm >= 25 & size.mm <= 965)

##Select only usefull columns
df.fish <- df.fish %>% dplyr::select(id.samp, species) %>% unique(.) %>% na.omit(.)

##Select id_campagne
df.fish <- df.fish %>% dplyr::filter(id.samp %in% df.lakelist$id.samp)
length(unique(df.fish$id.samp))


##Associate fish name with type
df.fish <- left_join(df.fish, df.speciestype, by = 'species')
summary(df.fish)


##------------------
## COMPUTE DIVERSITY
##------------------
Nonindigenousfishcommunity <- df.lakelist
Nonindigenousfishcommunity$fish.richness <- NA
Nonindigenousfishcommunity$nis.richness <- NA
Nonindigenousfishcommunity$pnis.perc <- NA



for(i in 1:nrow(Nonindigenousfishcommunity)){
sub.id <- df.fish %>% dplyr::filter(id.samp == Nonindigenousfishcommunity$id.samp[i])
Nonindigenousfishcommunity[i, 4] <- nrow(sub.id)
Nonindigenousfishcommunity[i, 5] <- sub.id %>% dplyr::filter(type == "non-indigenous") %>% nrow(.)
Nonindigenousfishcommunity[i, 6] <- (Nonindigenousfishcommunity[i, 5]/Nonindigenousfishcommunity[i, 4])*100
}

rm(sub.id, i)


mysave(Nonindigenousfishcommunity, dir = "./outputs/NonNativeDiversity", overwrite = TRUE)
