rm(list = ls()) #Removes all objects from the current workspace (R memory)
getwd()

##------------------------------------
##LOADING PACKAGES, FUNCTIONS AND DATA
##------------------------------------
library(dplyr)



lake_list <- read.csv("./outputs/lake_list.txt", sep="")
length(unique(lake_list$code_lac)) #258 lakes
length(unique(lake_list$id_campagne)) #432 sampling campaigns

fish_data <- read.csv("./data/fish_data.csv")
length(unique(fish_data$code_lac)) #285 lakes




##---------------------------------------
##GET THE DATE FOR EACH SAMPLING CAMPAIGN
##---------------------------------------
fish_data <- fish_data %>% dplyr::select(code_lac, camp_annee, id_campagne, id_point_prelev, id_prelev_poisson, date_pose, heure_pose, date_releve, heure_releve) %>% unique(.)
nrow(fish_data %>% dplyr::select(code_lac, id_campagne) %>% unique(.)) #477 couples code_lac-id_campagne
fish_data <- fish_data %>% dplyr::filter(id_campagne %in% lake_list$id_campagne)
nrow(fish_data %>% dplyr::select(code_lac, id_campagne) %>% unique(.)) #432 couples code_lac-id_campagne


date_fish_sampling <- fish_data %>% 
                        select(code_lac, camp_annee, id_campagne, date_pose) %>%
                        unique(.) %>%
                        group_by(id_campagne) %>%
                        filter(date_pose == min(date_pose))

write.table(date_fish_sampling, "./outputs/date_fish_sampling.txt", row.names = FALSE)

