rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(cowplot)
library(dplyr)
library(ggh4x)
library(ggpattern)
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(lemon)
library(lme4)
library(piecewiseSEM)
library(purrr)
library(semEff)
library(stringr)
library(tidyr)


##FUNCTIONS##
source("./rfunctions/misc.R")
source("./rfunctions/modifications_piecewiseSEM.R")


##--------------
## LOAD DATASETS
##--------------
myload(dataset_9BSCBenthicPelagicGillnetSelectivity,
       dir = "outputs")
lake.type <- read.csv("./data/Lake_type.txt", sep="")


##----------------
## PREPARE DATASET
##----------------
summary(dataset_9BSCBenthicPelagicGillnetSelectivity)
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity
dataset.thermal.trajectories <- left_join(dataset.thermal.trajectories, lake.type, by = "lake.code")
dataset.thermal.trajectories <- dataset.thermal.trajectories %>% dplyr::filter(typo_pla %in% c("R", "LN"))


##---------------
## MULTIGROUP SEM
##---------------
#https://github.com/jslefche/piecewiseSEM/issues/284

data <- dataset.thermal.trajectories %>% dplyr::select(lake.code, typo_pla, fish.richness, nis.richness, ols.slope, ols.elevation, connect, maxtl, bio1.slope.40y, bio1.current)         
str(data)
data$typo_pla <- as.factor(data$typo_pla)

##SEM WITH RESERVOIRS - DIRECT, INDIRECT AND TOTAL EFFECTS
data.reservoir <- data %>% dplyr::filter(typo_pla == "R")

thermtraj_reservoir <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.reservoir)
)

# Use the `psem` function to create the SEM
thermtraj_reservoir_model <- as.psem(thermtraj_reservoir)
summary(thermtraj_reservoir_model)
dSep(thermtraj_reservoir_model) #Can use `dsep` function to perform the tests automatically
fisherC(thermtraj_reservoir_model)  # Can use `fisherC` function to evaluate claims. Low p-value implies that the hypothesized structure is statistically different than the structure implied by our data. => a low p-value is actually bad as the model doesn't match the associations that are implied by the data
coefs(thermtraj_reservoir_model) #Get coefficients from good-fitting SEM

# Direct and indirect effect [https://murphymv.github.io/semEff/articles/semEff.html]
thermtraj.reservoir.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.reservoir),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.reservoir),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.reservoir)
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.reservoir.sem.boot <- bootEff(thermtraj.reservoir.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.reservoir.sem.eff <- semEff(thermtraj.reservoir.sem.boot))
summary(thermtraj.reservoir.sem.eff, response = "connect")
summary(thermtraj.reservoir.sem.eff, response = "maxtl")
summary(thermtraj.reservoir.sem.eff, response = "ols.slope")
summary(thermtraj.reservoir.sem.eff, response = "ols.elevation")
summary(thermtraj.reservoir.sem.eff, response = "fish.richness")
summary(thermtraj.reservoir.sem.eff, response = "nis.richness")
summary(thermtraj.reservoir.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
reservoir.response.connect <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$connect)
head(reservoir.response.connect)
reservoir.response.connect <- reservoir.response.connect[, -c(3, 5, 7, 9, 12)]
reservoir.response.connect <- reservoir.response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(reservoir.response.connect)[1] <- "Type"
colnames(reservoir.response.connect)[2] <- "Predictor"
colnames(reservoir.response.connect)[8] <- "Sym.sign"
reservoir.response.connect$Response <- "connect"
reservoir.response.connect <- reservoir.response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.connect[1:6, 3] <- "DIRECT" 
reservoir.response.connect[7:10, 3] <- "INDIRECT"  
reservoir.response.connect[11:16, 3] <- "TOTAL"    

reservoir.response.maxtl <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$maxtl)
head(reservoir.response.maxtl)
reservoir.response.maxtl <- reservoir.response.maxtl[, -c(3, 5, 7, 9, 12)]
reservoir.response.maxtl <- reservoir.response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(reservoir.response.maxtl)[1] <- "Type"
colnames(reservoir.response.maxtl)[2] <- "Predictor"
colnames(reservoir.response.maxtl)[8] <- "Sym.sign"
reservoir.response.maxtl$Response <- "maxtl"
reservoir.response.maxtl <- reservoir.response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.maxtl[1:7, 3] <- "DIRECT" 
reservoir.response.maxtl[8:13, 3] <- "INDIRECT"  
reservoir.response.maxtl[14:20, 3] <- "TOTAL"   

reservoir.response.ols.slope <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$ols.slope)
head(reservoir.response.ols.slope)
reservoir.response.ols.slope <- reservoir.response.ols.slope[, -c(3, 5, 7, 9, 12)]
reservoir.response.ols.slope <- reservoir.response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(reservoir.response.ols.slope)[1] <- "Type"
colnames(reservoir.response.ols.slope)[2] <- "Predictor"
colnames(reservoir.response.ols.slope)[8] <- "Sym.sign"
reservoir.response.ols.slope$Response <- "ols.slope"
reservoir.response.ols.slope <- reservoir.response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.ols.slope[1:4, 3] <- "DIRECT" 
reservoir.response.ols.slope[5:7, 3] <- "INDIRECT"  
reservoir.response.ols.slope[8:11, 3] <- "TOTAL"    

reservoir.response.ols.elevation <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$ols.elevation)
head(reservoir.response.ols.elevation)
reservoir.response.ols.elevation <- reservoir.response.ols.elevation[, -c(3, 5, 7, 9, 12)]
reservoir.response.ols.elevation <- reservoir.response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(reservoir.response.ols.elevation)[1] <- "Type"
colnames(reservoir.response.ols.elevation)[2] <- "Predictor"
colnames(reservoir.response.ols.elevation)[8] <- "Sym.sign"
reservoir.response.ols.elevation$Response <- "ols.elevation"
reservoir.response.ols.elevation <- reservoir.response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.ols.elevation[1:4, 3] <- "DIRECT" 
reservoir.response.ols.elevation[5:7, 3] <- "INDIRECT"  
reservoir.response.ols.elevation[8:11, 3] <- "TOTAL"    

reservoir.response.fish.richness <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$fish.richness)
head(reservoir.response.fish.richness)
reservoir.response.fish.richness <- reservoir.response.fish.richness[, -c(3, 5, 7, 9, 12)]
reservoir.response.fish.richness <- reservoir.response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(reservoir.response.fish.richness)[1] <- "Type"
colnames(reservoir.response.fish.richness)[2] <- "Predictor"
colnames(reservoir.response.fish.richness)[8] <- "Sym.sign"
reservoir.response.fish.richness$Response <- "fish.richness"
reservoir.response.fish.richness <- reservoir.response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.fish.richness[1:3, 3] <- "DIRECT" 
reservoir.response.fish.richness[4:5, 3] <- "INDIRECT"  
reservoir.response.fish.richness[6:8, 3] <- "TOTAL"    

reservoir.response.nis.richness <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$nis.richness)
head(reservoir.response.nis.richness)
reservoir.response.nis.richness <- reservoir.response.nis.richness[, -c(3, 5, 7, 9, 12)]
reservoir.response.nis.richness <- reservoir.response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(reservoir.response.nis.richness)[1] <- "Type"
colnames(reservoir.response.nis.richness)[2] <- "Predictor"
colnames(reservoir.response.nis.richness)[8] <- "Sym.sign"
reservoir.response.nis.richness$Response <- "nis.richness"
reservoir.response.nis.richness <- reservoir.response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.nis.richness[1:2, 3] <- "DIRECT" 
reservoir.response.nis.richness[3, 3] <- "INDIRECT"  
reservoir.response.nis.richness[4:5, 3] <- "TOTAL"    

reservoir.response.bio1.current <- as.data.frame(thermtraj.reservoir.sem.eff$Summary$bio1.current)
head(reservoir.response.bio1.current)
reservoir.response.bio1.current <- reservoir.response.bio1.current[, -c(3, 5, 7, 9, 12)]
reservoir.response.bio1.current <- reservoir.response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(reservoir.response.bio1.current)[1] <- "Type"
colnames(reservoir.response.bio1.current)[2] <- "Predictor"
colnames(reservoir.response.bio1.current)[8] <- "Sym.sign"
reservoir.response.bio1.current$Response <- "bio1.current"
reservoir.response.bio1.current <- reservoir.response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(reservoir.response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
reservoir.response.bio1.current[1, 3] <- "DIRECT" 
reservoir.response.bio1.current[2, 3] <- "TOTAL" 

reservoir.effect <- rbind(reservoir.response.connect, reservoir.response.maxtl,
                     reservoir.response.ols.slope, reservoir.response.ols.elevation,
                     reservoir.response.fish.richness, reservoir.response.nis.richness,
                     reservoir.response.bio1.current)
str(reservoir.effect)
reservoir.effect$Predictor <- str_replace(reservoir.effect$Predictor, "AsIs", "")
reservoir.effect$Predictor <- str_split(reservoir.effect$Predictor, ",") %>% sapply(as.character)
reservoir.effect$Type <- str_replace(reservoir.effect$Type, "AsIs", "")
reservoir.effect$Type <- str_split(reservoir.effect$Type, ",") %>% sapply(as.character)
reservoir.effect$Effect <- str_replace(reservoir.effect$Effect, "AsIs", "")
reservoir.effect$Effect <- str_split(reservoir.effect$Effect, ",") %>% sapply(as.numeric)
reservoir.effect$Bias<- str_replace(reservoir.effect$Bias, "AsIs", "")
reservoir.effect$Bias <- str_split(reservoir.effect$Bias, ",") %>% sapply(as.numeric)
reservoir.effect$StdErr <- str_replace(reservoir.effect$StdErr, "AsIs", "")
reservoir.effect$StdErr <- str_split(reservoir.effect$StdErr, ",") %>% sapply(as.numeric)
reservoir.effect$LowerCI <- str_replace(reservoir.effect$LowerCI, "AsIs", "")
reservoir.effect$LowerCI <- str_split(reservoir.effect$LowerCI, ",") %>% sapply(as.numeric)
reservoir.effect$UpperCI <- str_replace(reservoir.effect$UpperCI, "AsIs", "")
reservoir.effect$UpperCI <- str_split(reservoir.effect$UpperCI, ",") %>% sapply(as.numeric)
reservoir.effect$SymbSign <- str_replace(reservoir.effect$SymbSign, "AsIs", "")
reservoir.effect$SymbSign <- str_split(reservoir.effect$SymbSign, ",") %>% sapply(as.character)

str(reservoir.effect)

unique(reservoir.effect$Predictor)
unique(reservoir.effect$Response)
unique(reservoir.effect$Type)

reservoir.effect[reservoir.effect == "connect"] <- "Connectance"
reservoir.effect[reservoir.effect == "maxtl"] <- "Maximum trophic level"
reservoir.effect[reservoir.effect == "ols.slope"] <- "Slope"
reservoir.effect[reservoir.effect == "ols.elevation"] <- "Elevation"
reservoir.effect[reservoir.effect == "fish.richness"] <- "Total sp. richness"
reservoir.effect[reservoir.effect == "nis.richness"] <- "Exotic sp. richness"
reservoir.effect[reservoir.effect == "bio1.current"] <- "Climatic conditions"
reservoir.effect[reservoir.effect == "bio1.slope.40y"] <- "Climate warming"

reservoir.effect[reservoir.effect == "connect       "] <- "Connectance"
reservoir.effect[reservoir.effect == "maxtl"] <- "Maximum trophic level"
reservoir.effect[reservoir.effect == "ols.slope     "] <- "Slope"
reservoir.effect[reservoir.effect == "ols.elevation "] <- "Elevation"
reservoir.effect[reservoir.effect == "fish.richness "] <- "Total sp. richness"
reservoir.effect[reservoir.effect == "nis.richness  "] <- "Exotic sp. richness"
reservoir.effect[reservoir.effect == "bio1.current  "] <- "Climatic conditions"

reservoir.effect[reservoir.effect == "DIRECT"] <- "Direct"
reservoir.effect[reservoir.effect == "INDIRECT"] <- "Indirect"
reservoir.effect[reservoir.effect == "TOTAL"] <- "Total"
reservoir.effect[reservoir.effect == "DIRECT   "] <- "Direct"
reservoir.effect[reservoir.effect == "INDIRECT "] <- "Indirect"
reservoir.effect[reservoir.effect == "TOTAL    "] <- "Total"

str(reservoir.effect)
reservoir.effect$Response <- factor(reservoir.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination.reservoir <- reservoir.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.reservoir.effect <- reservoir.effect %>% dplyr::filter(Type == "Indirect")
indirect.reservoir.effect.absent <- anti_join(all.combination.reservoir, indirect.reservoir.effect %>% dplyr::select(Predictor, Response))
indirect.reservoir.effect.absent$Type <- "Indirect"
indirect.reservoir.effect.absent$Effect <- 0
indirect.reservoir.effect.absent$Bias <- 0
indirect.reservoir.effect.absent$StdErr <- 0
indirect.reservoir.effect.absent$LowerCI <- 0
indirect.reservoir.effect.absent$UpperCI <- 0
indirect.reservoir.effect.absent$SymbSign <- ""
reservoir.effect <- rbind(reservoir.effect, indirect.reservoir.effect.absent)
reservoir.effect$significant <- NA
reservoir.effect$significant[reservoir.effect$SymbSign == "*"] <- TRUE
reservoir.effect$significant[reservoir.effect$SymbSign == ""] <- FALSE
reservoir.effect <- reservoir.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
sem_effect_reservoir_lakes <- reservoir.effect
mysave(sem_effect_reservoir_lakes, dir = "outputs/SEM", overwrite = TRUE)
rm(sem_effect_reservoir_lakes, data.reservoir, thermtraj_reservoir, thermtraj_reservoir_model, thermtraj.reservoir.sem, thermtraj.reservoir.sem.boot, thermtraj.reservoir.sem.eff, reservoir.response.connect, reservoir.response.maxtl, reservoir.response.ols.slope, reservoir.response.ols.elevation, reservoir.response.fish.richness, reservoir.response.nis.richness, reservoir.response.bio1.current, all.combination.reservoir, indirect.reservoir.effect, indirect.reservoir.effect.absent)




##SEM WITH NATURAL LAKES - DIRECT, INDIRECT AND TOTAL EFFECTS
data.natural <- data %>% dplyr::filter(typo_pla == "LN")

thermtraj_natural <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.natural)
)

# Use the `psem` function to create the SEM
thermtraj_natural_model <- as.psem(thermtraj_natural)
summary(thermtraj_natural_model)
dSep(thermtraj_natural_model) #Can use `dsep` function to perform the tests automatically
fisherC(thermtraj_natural_model)  # Can use `fisherC` function to evaluate claims. Low p-value implies that the hypothesized structure is statistically different than the structure implied by our data. => a low p-value is actually bad as the model doesn't match the associations that are implied by the data
coefs(thermtraj_natural_model) #Get coefficients from good-fitting SEM

# Direct and indirect effect [https://murphymv.github.io/semEff/articles/semEff.html]
thermtraj.natural.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.natural),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.natural),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.natural)
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.natural.sem.boot <- bootEff(thermtraj.natural.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.natural.sem.eff <- semEff(thermtraj.natural.sem.boot))
summary(thermtraj.natural.sem.eff, response = "connect")
summary(thermtraj.natural.sem.eff, response = "maxtl")
summary(thermtraj.natural.sem.eff, response = "ols.slope")
summary(thermtraj.natural.sem.eff, response = "ols.elevation")
summary(thermtraj.natural.sem.eff, response = "fish.richness")
summary(thermtraj.natural.sem.eff, response = "nis.richness")
summary(thermtraj.natural.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
natural.response.connect <- as.data.frame(thermtraj.natural.sem.eff$Summary$connect)
head(natural.response.connect)
natural.response.connect <- natural.response.connect[, -c(3, 5, 7, 9, 12)]
natural.response.connect <- natural.response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(natural.response.connect)[1] <- "Type"
colnames(natural.response.connect)[2] <- "Predictor"
colnames(natural.response.connect)[8] <- "Sym.sign"
natural.response.connect$Response <- "connect"
natural.response.connect <- natural.response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.connect[1:6, 3] <- "DIRECT" 
natural.response.connect[7:10, 3] <- "INDIRECT"  
natural.response.connect[11:16, 3] <- "TOTAL"    

natural.response.maxtl <- as.data.frame(thermtraj.natural.sem.eff$Summary$maxtl)
head(natural.response.maxtl)
natural.response.maxtl <- natural.response.maxtl[, -c(3, 5, 7, 9, 12)]
natural.response.maxtl <- natural.response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(natural.response.maxtl)[1] <- "Type"
colnames(natural.response.maxtl)[2] <- "Predictor"
colnames(natural.response.maxtl)[8] <- "Sym.sign"
natural.response.maxtl$Response <- "maxtl"
natural.response.maxtl <- natural.response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.maxtl[1:7, 3] <- "DIRECT" 
natural.response.maxtl[8:13, 3] <- "INDIRECT"  
natural.response.maxtl[14:20, 3] <- "TOTAL"   

natural.response.ols.slope <- as.data.frame(thermtraj.natural.sem.eff$Summary$ols.slope)
head(natural.response.ols.slope)
natural.response.ols.slope <- natural.response.ols.slope[, -c(3, 5, 7, 9, 12)]
natural.response.ols.slope <- natural.response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(natural.response.ols.slope)[1] <- "Type"
colnames(natural.response.ols.slope)[2] <- "Predictor"
colnames(natural.response.ols.slope)[8] <- "Sym.sign"
natural.response.ols.slope$Response <- "ols.slope"
natural.response.ols.slope <- natural.response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.ols.slope[1:4, 3] <- "DIRECT" 
natural.response.ols.slope[5:7, 3] <- "INDIRECT"  
natural.response.ols.slope[8:11, 3] <- "TOTAL"    

natural.response.ols.elevation <- as.data.frame(thermtraj.natural.sem.eff$Summary$ols.elevation)
head(natural.response.ols.elevation)
natural.response.ols.elevation <- natural.response.ols.elevation[, -c(3, 5, 7, 9, 12)]
natural.response.ols.elevation <- natural.response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(natural.response.ols.elevation)[1] <- "Type"
colnames(natural.response.ols.elevation)[2] <- "Predictor"
colnames(natural.response.ols.elevation)[8] <- "Sym.sign"
natural.response.ols.elevation$Response <- "ols.elevation"
natural.response.ols.elevation <- natural.response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.ols.elevation[1:4, 3] <- "DIRECT" 
natural.response.ols.elevation[5:7, 3] <- "INDIRECT"  
natural.response.ols.elevation[8:11, 3] <- "TOTAL"    

natural.response.fish.richness <- as.data.frame(thermtraj.natural.sem.eff$Summary$fish.richness)
head(natural.response.fish.richness)
natural.response.fish.richness <- natural.response.fish.richness[, -c(3, 5, 7, 9, 12)]
natural.response.fish.richness <- natural.response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(natural.response.fish.richness)[1] <- "Type"
colnames(natural.response.fish.richness)[2] <- "Predictor"
colnames(natural.response.fish.richness)[8] <- "Sym.sign"
natural.response.fish.richness$Response <- "fish.richness"
natural.response.fish.richness <- natural.response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.fish.richness[1:3, 3] <- "DIRECT" 
natural.response.fish.richness[4:5, 3] <- "INDIRECT"  
natural.response.fish.richness[6:8, 3] <- "TOTAL"    

natural.response.nis.richness <- as.data.frame(thermtraj.natural.sem.eff$Summary$nis.richness)
head(natural.response.nis.richness)
natural.response.nis.richness <- natural.response.nis.richness[, -c(3, 5, 7, 9, 12)]
natural.response.nis.richness <- natural.response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(natural.response.nis.richness)[1] <- "Type"
colnames(natural.response.nis.richness)[2] <- "Predictor"
colnames(natural.response.nis.richness)[8] <- "Sym.sign"
natural.response.nis.richness$Response <- "nis.richness"
natural.response.nis.richness <- natural.response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.nis.richness[1:2, 3] <- "DIRECT"
natural.response.nis.richness[3, 3] <- "INDIRECT"
natural.response.nis.richness[4:5, 3] <- "TOTAL"    

natural.response.bio1.current <- as.data.frame(thermtraj.natural.sem.eff$Summary$bio1.current)
head(natural.response.bio1.current)
natural.response.bio1.current <- natural.response.bio1.current[, -c(3, 5, 7, 9, 12)]
natural.response.bio1.current <- natural.response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(natural.response.bio1.current)[1] <- "Type"
colnames(natural.response.bio1.current)[2] <- "Predictor"
colnames(natural.response.bio1.current)[8] <- "Sym.sign"
natural.response.bio1.current$Response <- "bio1.current"
natural.response.bio1.current <- natural.response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(natural.response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
natural.response.bio1.current[1, 3] <- "DIRECT"
natural.response.bio1.current[2, 3] <- "TOTAL" 


natural.effect <- rbind(natural.response.connect, natural.response.maxtl,
                     natural.response.ols.slope, natural.response.ols.elevation,
                     natural.response.fish.richness, natural.response.nis.richness,
                     natural.response.bio1.current)
str(natural.effect)
natural.effect$Predictor <- str_replace(natural.effect$Predictor, "AsIs", "")
natural.effect$Predictor <- str_split(natural.effect$Predictor, ",") %>% sapply(as.character)
natural.effect$Type <- str_replace(natural.effect$Type, "AsIs", "")
natural.effect$Type <- str_split(natural.effect$Type, ",") %>% sapply(as.character)
natural.effect$Effect <- str_replace(natural.effect$Effect, "AsIs", "")
natural.effect$Effect <- str_split(natural.effect$Effect, ",") %>% sapply(as.numeric)
natural.effect$Bias<- str_replace(natural.effect$Bias, "AsIs", "")
natural.effect$Bias <- str_split(natural.effect$Bias, ",") %>% sapply(as.numeric)
natural.effect$StdErr <- str_replace(natural.effect$StdErr, "AsIs", "")
natural.effect$StdErr <- str_split(natural.effect$StdErr, ",") %>% sapply(as.numeric)
natural.effect$LowerCI <- str_replace(natural.effect$LowerCI, "AsIs", "")
natural.effect$LowerCI <- str_split(natural.effect$LowerCI, ",") %>% sapply(as.numeric)
natural.effect$UpperCI <- str_replace(natural.effect$UpperCI, "AsIs", "")
natural.effect$UpperCI <- str_split(natural.effect$UpperCI, ",") %>% sapply(as.numeric)
natural.effect$SymbSign <- str_replace(natural.effect$SymbSign, "AsIs", "")
natural.effect$SymbSign <- str_split(natural.effect$SymbSign, ",") %>% sapply(as.character)

str(natural.effect)

unique(natural.effect$Predictor)
unique(natural.effect$Response)
unique(natural.effect$Type)

natural.effect[natural.effect == "connect"] <- "Connectance"
natural.effect[natural.effect == "maxtl"] <- "Maximum trophic level"
natural.effect[natural.effect == "ols.slope"] <- "Slope"
natural.effect[natural.effect == "ols.elevation"] <- "Elevation"
natural.effect[natural.effect == "fish.richness"] <- "Total sp. richness"
natural.effect[natural.effect == "nis.richness"] <- "Exotic sp. richness"
natural.effect[natural.effect == "bio1.current"] <- "Climatic conditions"
natural.effect[natural.effect == "bio1.slope.40y"] <- "Climate warming"

natural.effect[natural.effect == "connect       "] <- "Connectance"
natural.effect[natural.effect == "maxtl"] <- "Maximum trophic level"
natural.effect[natural.effect == "ols.slope     "] <- "Slope"
natural.effect[natural.effect == "ols.elevation "] <- "Elevation"
natural.effect[natural.effect == "fish.richness "] <- "Total sp. richness"
natural.effect[natural.effect == "nis.richness  "] <- "Exotic sp. richness"
natural.effect[natural.effect == "bio1.current  "] <- "Climatic conditions"

natural.effect[natural.effect == "DIRECT"] <- "Direct"
natural.effect[natural.effect == "INDIRECT"] <- "Indirect"
natural.effect[natural.effect == "TOTAL"] <- "Total"
natural.effect[natural.effect == "DIRECT   "] <- "Direct"
natural.effect[natural.effect == "INDIRECT "] <- "Indirect"
natural.effect[natural.effect == "TOTAL    "] <- "Total"

str(natural.effect)
natural.effect$Response <- factor(natural.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination.natural <- natural.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.natural.effect <- natural.effect %>% dplyr::filter(Type == "Indirect")
indirect.natural.effect.absent <- anti_join(all.combination.natural, indirect.natural.effect %>% dplyr::select(Predictor, Response))
indirect.natural.effect.absent$Type <- "Indirect"
indirect.natural.effect.absent$Effect <- 0
indirect.natural.effect.absent$Bias <- 0
indirect.natural.effect.absent$StdErr <- 0
indirect.natural.effect.absent$LowerCI <- 0
indirect.natural.effect.absent$UpperCI <- 0
indirect.natural.effect.absent$SymbSign <- ""
natural.effect <- rbind(natural.effect, indirect.natural.effect.absent)
natural.effect$significant <- NA
natural.effect$significant[natural.effect$SymbSign == "*"] <- TRUE
natural.effect$significant[natural.effect$SymbSign == ""] <- FALSE
natural.effect <- natural.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
sem_effect_natural_lakes <- natural.effect
mysave(sem_effect_natural_lakes, dir = "outputs/SEM", overwrite = TRUE)
rm(sem_effect_natural_lakes, data.natural, thermtraj_natural, thermtraj_natural_model, thermtraj.natural.sem, thermtraj.natural.sem.boot, thermtraj.natural.sem.eff, natural.response.connect, natural.response.maxtl, natural.response.ols.slope, natural.response.ols.elevation, natural.response.fish.richness, natural.response.nis.richness, natural.response.bio1.current, all.combination.natural, indirect.natural.effect, indirect.natural.effect.absent)




