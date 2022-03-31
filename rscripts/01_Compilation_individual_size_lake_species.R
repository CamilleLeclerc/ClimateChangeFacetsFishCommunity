rm(list=ls())

##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(devtools)
library(dplyr)
library(tidyverse)
source_url("https://raw.githubusercontent.com/alaindanet/SizeTrophicInteractions/main/R/building_dataset.R")


##--------------
## LOAD DATASETS
##--------------
db.fish <- read.csv("data/fish_data.csv") #fish_data.csv is too large to be on github ; it is listed in gitignore


##--------------------
## DATASET EXPLORATION
##--------------------
head(db.fish)

  ##Focused period for the project is 2005 - 2018
  sort(unique(db.fish$camp_annee))


  ##Rename fish name
  length(unique(db.fish$code_lac))
  length(unique(db.fish$nom_latin_taxon))
  sort(unique(db.fish$nom_latin_taxon))

  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Aspius aspius"] <- "Leuciscus aspius"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Blennius fluviatilis"] <- "Salaria fluviatilis"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Carassius auratus auratus"] <- "Carassius auratus"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Carassius auratus gibelio"] <- "Carassius gibelio"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Chondrostoma toxostoma"] <- "Parachondrostoma toxostoma"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Coregonus"] <- "Coregonus lavaretus"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Cyprinidaes"] <- "Cyprinidae"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Gasterosteus aculeatus aculeatus"] <- "Gasterosteus aculeatus"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Gymnocephalus cernuus"] <- "Gymnocephalus cernua"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Hybride breme-gardon"] <- "Cyprinidae"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Hybrides de cyprinides"] <- "Cyprinidae"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Leuciscus cephalus"] <- "Squalius cephalus"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Leuciscus souffia"] <- "Telestes souffia"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Liza aurata"] <- "Chelon auratus"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Liza ramada"] <- "Chelon ramada"
  db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Salvelinus umbla"] <- "Salvelinus alpinus"

  ##Delete no-fish species
  db.fish <- db.fish %>% filter(!(nom_latin_taxon %in% c("Astacus astacus", "Eriocheir sinensis", "Orconectes limosus", "Pacifastacus leniusculus", "Procambarus clarkii")))

  ##Analysis dataset  
  sub.db.fish <- db.fish %>% select(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev,
                                    coord_x, coord_y, cd_proj, date_pose, heure_pose, date_releve, heure_releve,
                                    prof_min_point_pose, prof_max_point_pose, cd_engin_peche, strate,
                                    ident_lot, nom_latin_taxon, cd_type_lot, effectif_lot, taille_min_lot, taille_max_lot, taille_ind, type_longueur)

  unique(sub.db.fish$cd_type_lot)
  sub.db.fish$cd_type_lot[sub.db.fish$cd_type_lot == "L"] <- "S/L"
  sub.db.fish$cd_type_lot[sub.db.fish$cd_type_lot == "S"] <- "S/L"
  sub.db.fish$cd_type_lot[sub.db.fish$cd_type_lot == "0"] <- NA
  sub.db.fish <- sub.db.fish %>% drop_na(cd_type_lot)
  unique(sub.db.fish$cd_type_lot)
  
  sub.db.fish$taille_ind[sub.db.fish$taille_ind == 0] <- NA
  
  unique(sub.db.fish$strate)
  unique(sub.db.fish$cd_engin_peche) #FB : Filet benthique ; FP : Filet pelagique ; FPa : 2 filets pelagiques consideres comme 1 seul filet
  
  
  
  
##----------------------------
## COMPILATION INDIVIDUAL SIZE
##----------------------------
for (i in 1:length(unique(sub.db.fish$code_lac))){
  sub.lake <- sub.db.fish %>% filter(code_lac == unique(sub.db.fish$code_lac)[i])
  
  for (j in 1:length(unique(sub.lake$camp_annee))){
    sub.year <- sub.lake %>% filter(camp_annee == unique(sub.lake$camp_annee)[j])
    
    df.size <- data.frame(matrix(ncol = 18, nrow = 0))
    colnames(df.size) <- c("code_lac", "camp_annee", "id_campagne", "id_prelev_poisson", "id_point_prelev",
                           "coord_x", "coord_y", "cd_proj", "date_pose", "heure_pose", "date_releve", "heure_releve",
                           "prof_min_point_pose", "prof_max_point_pose", "cd_engin_peche", "strate", "species", "fish")
    
    for (k in 1:length(unique(sub.year$id_prelev_poisson))){
        sub.strate <- sub.year %>% filter(id_prelev_poisson == unique(sub.year$id_prelev_poisson)[k])
        
        sub.strate <- sub.strate %>% mutate(ident_lot = replace(ident_lot, 
                                                                cd_type_lot == "N",
                                                                paste0(sub.strate$id_point_prelev,'_', 1:30000)))
        
        sub.strate <- sub.strate %>% mutate(ident_lot = replace(ident_lot, 
                                                                cd_type_lot == "G",
                                                                paste0(sub.strate$id_point_prelev,'_', 30001:60000)))

        sub.strate <- sub.strate %>% mutate(ident_lot = replace(ident_lot, 
                                                                cd_type_lot == "I",
                                                                paste0(sub.strate$id_point_prelev,'_', 60001:90000)))
        sub.strate <- sub.strate %>% mutate(effectif_lot = replace(effectif_lot, 
                                                                    cd_type_lot == "I",
                                                                    1))
        sub.strate <- sub.strate[!(sub.strate$cd_type_lot=="N" & is.na(sub.strate$taille_ind)),]
        sub.strate <- sub.strate[!(sub.strate$cd_type_lot=="I" & is.na(sub.strate$taille_ind)),]
        
        sub.lot <- unique(sub.strate %>% drop_na(effectif_lot) %>% select(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev,
                                                                          coord_x, coord_y, cd_proj, date_pose, heure_pose, date_releve, heure_releve,
                                                                          prof_min_point_pose, prof_max_point_pose, cd_engin_peche, strate, ident_lot, nom_latin_taxon, cd_type_lot, effectif_lot, taille_min_lot, taille_max_lot))

        sub.measure <- unique(sub.strate %>% drop_na(ident_lot | taille_ind) %>% select(ident_lot, taille_ind))
        

        sub.size <- get_size_from_lot(lot = sub.lot,
                                      id_var = ident_lot,
                                      type_var = cd_type_lot,
                                      nb_var = effectif_lot,
                                      min_var = taille_min_lot,
                                      max_var = taille_max_lot,
                                      species = nom_latin_taxon,
                                      measure = sub.measure,
                                      measure_id_var = ident_lot,
                                      size_var = taille_ind)
        
        colnames(sub.size) <- c("id", "species", "fish")
        sub.size <- sub.size %>% select(species, fish)
        sub.size[ , c("code_lac", "camp_annee", "id_campagne", "id_prelev_poisson", "id_point_prelev",
                      "coord_x", "coord_y", "cd_proj", "date_pose", "heure_pose", "date_releve", "heure_releve",
                      "prof_min_point_pose", "prof_max_point_pose", "cd_engin_peche", "strate")] <- unique(sub.strate %>% select(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev,
                                                                                                               coord_x, coord_y, cd_proj, date_pose, heure_pose, date_releve, heure_releve,
                                                                                                               prof_min_point_pose, prof_max_point_pose, cd_engin_peche, strate))
        sub.size <- sub.size %>% select(code_lac, camp_annee, id_campagne, id_prelev_poisson, id_point_prelev,
                                        coord_x, coord_y, cd_proj, date_pose, heure_pose, date_releve, heure_releve,
                                        prof_min_point_pose, prof_max_point_pose, cd_engin_peche, strate, species, fish)
        
        df.size <- rbind(df.size, sub.size)
        
        print(c(i, j, k))
    }
    
    df.size$date_pose <- gsub("-", ".", paste(df.size$date_pose))
    df.size$date_releve <- gsub("-", ".", paste(df.size$date_releve))
    df.size$heure_pose <- gsub(":", ".", paste(df.size$heure_pose))
    df.size$heure_releve <- gsub(":", ".", paste(df.size$heure_releve))
    df.size.final <- apply(df.size,2,as.character)
    write.table(df.size.final, paste0("outputs/lake_individual_size/Indsize_", unique(sub.year$code_lac), "_", unique(sub.year$camp_annee),".txt"), row.names = FALSE)
    
  }
}

rm(i, j, k, sub.lake, sub.year, sub.strate, sub.size, df.size, df.size.final)



