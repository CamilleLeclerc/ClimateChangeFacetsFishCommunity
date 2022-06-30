##GET CLEAN SIZE DATASET


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)
library(magrittr)

getwd()


##-------------------------------------------
## GET ALL TEXT FILES OF LAKE INDIVIDUAL SIZE
##-------------------------------------------
txt_files_ls = list.files(path = "outputs/IndividualSize/individual_files_LakeIndSize", pattern="*\\.txt$", recursive = TRUE, full.names = TRUE) 
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = T, sep ="")})
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
combined_df <- combined_df[!(combined_df$code_lac == "ANN74" & combined_df$camp_annee == 2012),]
combined_df <- combined_df[!(combined_df$code_lac == "LEM74" & combined_df$camp_annee == 2015),]
write.table(combined_df, "outputs/IndividualSize/lake_individual_size.txt", row.names = FALSE)
rm(txt_files_ls, txt_files_df, combined_df)

# Import file
lake_size <- read_delim(file = "outputs/IndividualSize/lake_individual_size.txt",
                        delim = " ")

summary(lake_size)


# Separate data to avoid too much redundancy in table
sampling_protocol <- unique(lake_size %>% select(code_lac:id_point_prelev, date_pose:strate))
save(sampling_protocol, file = "outputs/sampling_protocol.rda")

station <- unique(lake_size %>% select(code_lac:id_point_prelev, coord_x:cd_proj))
save(station, file = "outputs/station.rda")

# Check unique sampling:
lake_size %>%
  distinct(code_lac, camp_annee, .keep_all = TRUE) %>%
  group_by(code_lac, camp_annee) %>%
  summarise(n = n())
#pb: more campaign than combination of lake/year
unique(lake_size$id_campagne) %>% length
lake_size %>% select(code_lac, camp_annee) %>% unique(.) %>% nrow
#Note :
#id_campagne est un identifiant unique associé à chaque campagne de pêche.
#Pour certains lacs alpins (ANN74 et LEM74) il y a deux campagnes
#la même année (respectivement 2012 et 2015) avec parfois des échantillonnages
#parfois partielles, effectuées avec un nombre plus restreint de filet.
#En conséquence, les données ANN74-2012 et LEM75-2015 n'ont pas été prises en compte.

# Get individual size for fishes 

ind_size <- lake_size %>% select(code_lac:id_point_prelev, species, fish)

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

save(ind_size, file = "outputs/IndividualSize/ind_size.rda")
