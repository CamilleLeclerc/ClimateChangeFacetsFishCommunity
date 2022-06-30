#note: pour le calcul du nombre de filet, il faut bien repartir du fichier initial "fish_data"
#car dans les fichiers lake individual size "Indsize_Code.lake_Year", il peut manquer des filets en raison d'absence de poissons
#To check with https://dashboard.ecla.inrae.fr/lacs-poissons/

##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(devtools)
library(dplyr)
library(tidyverse)


##--------------
## LOAD DATASETS
##--------------
db.fish <- read.csv("data/fish_data.csv") #fish_data.csv is too large to be on github ; it is listed in gitignore
nrow(db.fish)

##Data period
sort(unique(db.fish$camp_annee))
#db.fish <- db.fish %>% filter(camp_annee < 2020)
#nrow(db.fish)
#sort(unique(db.fish$camp_annee))


##--------------------------------------------------------
## COMPILATION SAMPLING EFFORT - NUMBER OF GILLNETS LAYING
##--------------------------------------------------------

##Sampling effort
sampling_effort_benthic <- data.frame(code_lac = character(),
                                      camp_annee = numeric(),
                                      nb_id_campagne = numeric(),
                                      nb_id_prelev_poisson = numeric(),
                                      nb_id_point_prelev = numeric())

sampling_effort_pelagic <- data.frame(code_lac = character(),
                                      camp_annee = numeric(),
                                      nb_id_campagne = numeric(),
                                      nb_id_prelev_poisson = numeric(),
                                      nb_id_point_prelev = numeric())




for (i in 1:length(unique(db.fish$code_lac))){
  sub_data <- db.fish %>% filter(code_lac == unique(db.fish$code_lac)[i])
  
  for (j in 1:length(unique(sub_data$camp_annee))){
    subset_data <- sub_data %>% filter(camp_annee == unique(sub_data$camp_annee)[j])
    subset_data_benthic <- subset_data %>% filter(cd_engin_peche == "FB")
    subset_data_pelagic <- subset_data %>% filter(cd_engin_peche %in% c("FP", "FPa"))    
    
    if(nrow(subset_data_benthic) !=0 ){ sub_sampling_effort_benthic <- data.frame(as.list(c(unique(subset_data_benthic$code_lac), unique(subset_data_benthic$camp_annee), length(unique(subset_data_benthic$id_campagne)),
                                                                                            length(unique(subset_data_benthic$id_point_prelev)), length(unique(subset_data_benthic$id_prelev_poisson))
    )))
    colnames(sub_sampling_effort_benthic) <- c("code_lac", "camp_annee", "nb_id_campagne", "nb_id_point_prelev", "nb_id_prelev_poisson")
    sampling_effort_benthic <- rbind(sampling_effort_benthic,  sub_sampling_effort_benthic)
    }
    
    if(nrow(subset_data_pelagic) !=0 ){ sub_sampling_effort_pelagic  <- data.frame(as.list(c(unique(subset_data_pelagic$code_lac), unique(subset_data_pelagic$camp_annee), length(unique(subset_data_pelagic$id_campagne)),
                                                                                             length(unique(subset_data_pelagic$id_point_prelev)), length(unique(subset_data_pelagic$id_prelev_poisson))
    )))
    colnames(sub_sampling_effort_pelagic) <- c("code_lac", "camp_annee", "nb_id_campagne", "nb_id_point_prelev", "nb_id_prelev_poisson")
    sampling_effort_pelagic <- rbind(sampling_effort_pelagic, sub_sampling_effort_pelagic)
    }
  }
}

rm(sub_data, sub_sampling_effort_benthic, sub_sampling_effort_pelagic, subset_data, subset_data_benthic, subset_data_pelagic, i, j)

write.table(sampling_effort_benthic, "outputs/lake_number_gillnets_benthic.txt", row.names = FALSE)
write.table(sampling_effort_pelagic, "outputs/lake_number_gillnets_pelagic.txt", row.names = FALSE)

