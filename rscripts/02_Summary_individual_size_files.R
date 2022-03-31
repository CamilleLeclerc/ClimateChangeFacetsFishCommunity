rm(list=ls())

##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)
library(magrittr)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

#get all text files of lake individual size
#txt_files_ls = list.files(path = "./outputs/lake_individual_size", pattern="*\\.txt$", recursive = TRUE, full.names = TRUE) 
#txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = T, sep ="")})
#combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
#write.table(combined_df, "data-raw/lake_individual_size.txt", row.names = FALSE)
#rm(txt_files_ls, txt_files_df, combined_df)

# Import file
lake_size <- read_delim(
  file = mypath("data-raw", "lake_individual_size.txt"),
  delim = " ")

summary(lake_size)


# Separate data to avoid too much redundancy in table
sampling_protocol <- lake_size %>%
  select(code_lac, camp_annee, id_campagne, date_pose:strate)
usethis::use_data(sampling_protocol, overwrite = TRUE)

station <- lake_size %>%
  select(code_lac, coord_x, coord_y) %>%
  distinct()
usethis::use_data(station, overwrite = TRUE)

# Check unique sampling:
lake_size %>%
  distinct(code_lac, camp_annee, .keep_all = TRUE) %>%
  group_by(code_lac, camp_annee) %>%
  summarise(n = n())
#pb: more campaign than combination of lake/year
unique(lake_size$id_campagne) %>% length
lake_size %>% select(code_lac, camp_annee) %>% unique(.) %>% nrow
# Camille said:
#id_campagne est bien un identifiant unique associé à chaque campagne de pêche.
#Effectivement, pour certains lacs alpins (ANN74 et LEM74) il y a deux campagnes
#la même année (respectivement 2012 et 2015). D'ailleurs, de ce que l'on m'a
#dit, ces lacs ont des années d'échantillonnages parfois partielles, effectuées
#avec un nombre plus restreint de filet. Je pense qu'il ne faudra pas les
#considérer.  Je regarderais ça en détails et mettrais un script et un output
#dispo sur github.

# Get individual size for fishes 

ind_size <- lake_size %>%
  select(id_campagne, species, fish)

#problem: the txt stored vector as character ! 
head(ind_size$fish)

convert_chr_vector_to_integer <- function(x) {

  # Remove the string delimitating the vector
  str_remove_all(x, "[c(|)]") %>%
    # separate numbers based on ", "
    str_split(., ", ") %>%
    # the result is as a list
    .[[1]] %>%
    # turn as an integer
    as.integer()
}

## test
convert_chr_vector_to_integer(ind_size[3, ]$fish[[1]])
convert_chr_vector_to_integer(ind_size[4, ]$fish[[1]])


ind_size$fish <- map(ind_size$fish, convert_chr_vector_to_integer)

# get the right variable
ind_size %<>%
  unnest(fish)
# Remove spaces in species to avoid mismatches error
ind_size %<>%
  mutate(species = str_replace_all(species, " ", "_"))
usethis::use_data(ind_size, overwrite = TRUE)
