##--------------
## LOAD PACKAGES
##--------------
library(ggplot2)
library(knitr)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthhires)
library(tidyverse)


##-------------
## LOAD DATASET
##-------------
db.fish <- read.csv("data/fish_data.csv") 


##--------------------
## DATASET EXPLORATION
##--------------------
head(db.fish)
nrow(db.fish)

##Period available
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

colnames(db.fish)


##----------------------------------------
## NUMBER OF LAKES WHERE THE SPECIES OCCUR
##----------------------------------------
db.fish %>%
  #filter(nom_latin_taxon %in% sp.to.check) %>%
  select(nom_latin_taxon, code_lac) %>%
  unique(.) %>%
  group_by(nom_latin_taxon ) %>%
  summarise(occurrence = n()) %>%
  knitr::kable()


##----------------------------------
## LIST OF LAKES WHERE SPECIES OCCUR
##----------------------------------
db.fish %>%
  #filter(nom_latin_taxon %in% sp.to.check) %>%
  select(nom_latin_taxon, code_lac) %>%
  unique(.) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = nom_latin_taxon, values_from = present) %>%
  #mutate_at(vars(sp.to.check), ~ifelse(is.na(.), 0, 1)) %>%
  knitr::kable()

##--------------------------------------
## PLOT OF LAKES WHERE THE SPECIES OCCUR
##--------------------------------------
lake_dataset <- db.fish %>% select(code_lac, camp_annee, nom_latin_taxon) %>% drop_na(.) %>% unique(.)
db.plando.caracteristiques <- read.csv("data/plan_deau_lacs_caracteristiques.csv") 
lake_dataset <- left_join(lake_dataset, db.plando.caracteristiques %>% select(code_lac, lat_pla, long_pla), by = "code_lac")
write.table(lake_dataset, "outputs/lake_dataset.txt", row.names = FALSE)


lake_list <- db.fish %>% select(code_lac, camp_annee, id_campagne) %>% unique(.)
write.table(lake_list, "outputs/lake_list.txt", row.names = FALSE)


worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')


ggplot() +
  geom_sf(data = worldmap, fill = "#ffffff", color = "#000000", size = 0.05) +
  geom_sf(data = francemap, fill = "#d9d9d9", color = "#000000", size = 0.25) +
  geom_point(data = lake_dataset  %>%
                      #filter(taxon %in% sp.to.check) %>%
                      select(nom_latin_taxon, lat_pla, long_pla) %>%
                      unique(.),
             aes(x = long_pla, y = lat_pla), shape = 21, colour = "#000000", fill = "#9e0142", size = 1) +
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  facet_wrap(~ nom_latin_taxon) +
  theme(title = element_text(),
        plot.title = element_text(margin = margin(20,20,20,20), size = 18, hjust = 0.5),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.background = element_blank(),
        strip.background = element_rect(fill = "#000000", color = "#000000", size = 1, linetype = "solid"),
        strip.text.x = element_text(size = 12, color = "#ffffff", face = "bold"),
        panel.border = element_rect(colour = "#000000", fill = NA, size = 1)) 
