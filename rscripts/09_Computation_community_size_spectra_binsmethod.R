rm(list=ls()) #Removes all objects from the current workspace (R memory)


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(dplyr)
library(plyr)
library(sizeSpectra)
library(tidyr)

##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/linear_size_spectrum_calculation.R")


##-------------
## LOAD DATASET
##-------------
myload(ind_size, dir = "outputs/IndividualSize")
lake_list <- read.csv("outputs/lake_list.txt", sep="")


##----------------
## PREPARE DATASET
##----------------
head(ind_size)
df <- ind_size %>% dplyr::select(id_campagne, species, fish)
colnames(df) <- c("id.samp", "species", "size.mm")
sort(unique(df$species))
length(unique(df$species))

df$species[df$species == "Abramis"] <- "Abramis_brama"
df$species[df$species == "Salmo_trutta_fario"] <- "Salmo_trutta"
df$species[df$species == "Salmo_trutta_lacustris"] <- "Salmo_trutta"
df$species[df$species == "Hybride_br�me-gardon"] <- "Cyprinidae"
df$species[df$species == "Hybrides_de_cyprinid�s"] <- "Cyprinidae"
df <- df %>% filter(!(species %in% c("Cyprinidae", "Mugilidae", "Percidae"))) #delete these families as there are not considered in food-webs
length(unique(df$species))

summary(df)

# Supprime les individus avec taille = NA
df <- df %>% drop_na()

# Transformer taille de mm en cm
df$size.cm = df$size.mm/10

# Restriction de la gamme de taille en raison de la sélectivité des filets maillants 
# Dupagne et al. (2021) [https://doi.org/10.1111/fme.12476]:
# Size classes at the lower and upper end of the communities’ size range were excluded from the analysis to avoid bias due to,
# respectively, poor retention in the gear and too infrequent captures (i.e. juveniles or small fish can swim through meshes and largest individuals avoid nets more easily)
# (Axenrot & Hansson, 2004; Elliott & Fletcher, 2001; Mehner & Schulz, 2002) (size range considered = 2.5 - 96.5 cm).
df <- df %>% dplyr::filter(size.mm >= 25 & size.mm <= 965)




##----------------------------------------
## COMMUNITY SIZE SPECTRA - bins method from Marin et al. 2023
## ordinary-least square linear regression between the log2 of each size class midpoint and the log2 of the total number of individuals per size class, normalized by bin width.
##----------------------------------------
head(df)
id <- unique(df$id.samp)

CSSOLSfishcommunity <- data.frame(matrix(ncol = 8, nrow = 0, dimnames = list(NULL, c("id.samp", "ols.slope", "ols.elevation", "ols.linearity", "pvalue", "number.size.class", "int.empt.size.class", "extreme.empt.size.class"))))
CSSOLSfrequencytable <- data.frame(matrix(ncol = 8, nrow = 0, dimnames = list(NULL, c("id.samp", "size.classes", "low.bound", "upp.bound", "midpoint", "width", "values", "norm.abun"))))

for(i in 1:length(id)){
  sub.df <- df %>% dplyr::filter(id.samp == id [i])
  size <- sub.df$size.cm
  
  SS.parameters <- size.spectra(size, 1, max(size), 14, 2, 1, "no") #size as the vector of individual body sizes
  
  CSSOLSfishcommunity[i, 1] <- id [i]
  CSSOLSfishcommunity[i, 2] <- SS.parameters$NASS.slope
  CSSOLSfishcommunity[i, 3] <- SS.parameters$NASS.intercept
  CSSOLSfishcommunity[i, 4] <- SS.parameters$NASS.adj.R.squared
  CSSOLSfishcommunity[i, 5] <- SS.parameters$NASS.p.value
  CSSOLSfishcommunity[i, 6] <- SS.parameters$number.size.classes
  CSSOLSfishcommunity[i, 7] <- SS.parameters$int.empt.size.class
  CSSOLSfishcommunity[i, 8] <- SS.parameters$extreme.empt.size.class
  
  frequencytable <- SS.parameters$frequency.table
  frequencytable$id.samp <- id [i]
  frequencytable <- frequencytable %>% dplyr::select(id.samp, size.classes, low.bound, upp.bound, midpoint, width, values, norm.abun)
  CSSOLSfrequencytable <- rbind(CSSOLSfrequencytable, frequencytable)
  
  rm(sub.df, size, SS.parameters, frequencytable)
}
CSSOLSfishcommunityBenthicPelagic <- CSSOLSfishcommunity
CSSOLSfrequencytableBenthicPelagic <- CSSOLSfrequencytable
mysave(CSSOLSfishcommunityBenthicPelagic, CSSOLSfrequencytableBenthicPelagic, dir = "./outputs/CommunitySizeSpectra", overwrite = TRUE)
