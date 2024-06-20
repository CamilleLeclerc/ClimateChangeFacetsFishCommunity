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


##----------------
## PREPARE DATASET
##----------------
summary(dataset_9BSCBenthicPelagicGillnetSelectivity)
dataset.thermal.trajectories <- dataset_9BSCBenthicPelagicGillnetSelectivity


##-----------------------------------------------------------------------------------------------------------------------------------------------
## CATEGORIZE LAKES (e.g. COLD, MODERATE, WARM) TO SEE IF WE OBSERVE THE SAME EFFECTS OF WARMING IN LAKES THAT ARE ALREADY WARM VERSUS COLD LAKES
##-----------------------------------------------------------------------------------------------------------------------------------------------
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.25, 0.75))
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.3, 0.6))
quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = c(0.33333, 0.6666))

dataset.thermal.trajectories$type.quant.33.66 <- NA
dataset.thermal.trajectories$type.quant.33.66[dataset.thermal.trajectories$bio1.current <= 13.05480] <- "cold"
dataset.thermal.trajectories$type.quant.33.66[dataset.thermal.trajectories$bio1.current >= 14.57316] <- "warm"
dataset.thermal.trajectories$type.quant.33.66[is.na(dataset.thermal.trajectories$type.quant.33.66)] <- "moderate"
summary(as.factor(dataset.thermal.trajectories$type.quant.33.66))


dataset.thermal.trajectories %>%
  ggplot(aes(x = bio1.current), position = "identity") +
  geom_histogram(aes(y = stat(density)), alpha = 0.4, color = "#8d96a3", fill = "#8d96a3") +
  geom_density(size = 1, color = "#8d96a3", adjust = 2) +
  geom_segment(x = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.33333), xend = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.33333), y = -Inf, yend = 0.3, linetype = "dashed", size = 1) +
  geom_segment(x = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.66666), xend = quantile(dataset.thermal.trajectories$bio1.current, na.rm = T, probs = 0.66666), y = -Inf, yend = 0.3, linetype = "dashed", size = 1) +
    labs(y = "Density", x = "Climatic condition (°C)") +
  scale_x_continuous(breaks = c(5, 10, 15, 20), limits = c(5, 20)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3), limits = c(0, 0.3)) +
  guides(y = guide_axis_truncated(), x = guide_axis_truncated()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12, colour = "#000000"),
        axis.title = element_text(size = 14, face = "bold", colour = "#000000"))


##---------------
## MULTIGROUP SEM
##---------------
#https://github.com/jslefche/piecewiseSEM/issues/284

data <- dataset.thermal.trajectories %>% dplyr::select(lake.code, type.quant.33.66, fish.richness, nis.richness, ols.slope, ols.elevation, connect, maxtl, bio1.slope.40y, bio1.current)         
str(data)
data$type.quant.33.66 <- as.factor(data$type.quant.33.66)

thermtraj_lme <- psem(
  lme(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(nis.richness ~ bio1.slope.40y + bio1.current, random = ~ 1 |lake.code, data),
  lme(bio1.current ~ bio1.slope.40y, random = ~ 1 |lake.code, data),
  data = data)


thermtraj_lme.multigroup <- multigroup2(thermtraj_lme, group = "type.quant.33.66")
thermtraj_lme.multigroup 




##SEM WITH "COLD" LAKES - DIRECT, INDIRECT AND TOTAL EFFECTS
data.cold <- data %>% dplyr::filter(type.quant.33.66 == "cold")

thermtraj_cold <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.cold)
)

# Use the `psem` function to create the SEM
thermtraj_cold_model <- as.psem(thermtraj_cold)
summary(thermtraj_cold_model)
#dSep(thermtraj_cold_model) #Can use `dsep` function to perform the tests automatically
#fisherC(thermtraj_cold_model)  # Can use `fisherC` function to evaluate claims. Low p-value implies that the hypothesized structure is statistically different than the structure implied by our data. => a low p-value is actually bad as the model doesn't match the associations that are implied by the data
#coefs(thermtraj_cold_model) #Get coefficients from good-fitting SEM

# Direct and indirect effect [https://murphymv.github.io/semEff/articles/semEff.html]
thermtraj.cold.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.cold),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.cold)
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.cold.sem.boot <- bootEff(thermtraj.cold.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.cold.sem.eff <- semEff(thermtraj.cold.sem.boot))
summary(thermtraj.cold.sem.eff, response = "connect")
summary(thermtraj.cold.sem.eff, response = "maxtl")
summary(thermtraj.cold.sem.eff, response = "ols.slope")
summary(thermtraj.cold.sem.eff, response = "ols.elevation")
summary(thermtraj.cold.sem.eff, response = "fish.richness")
summary(thermtraj.cold.sem.eff, response = "nis.richness")
summary(thermtraj.cold.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
cold.response.connect <- as.data.frame(thermtraj.cold.sem.eff$Summary$connect)
head(cold.response.connect)
cold.response.connect <- cold.response.connect[, -c(3, 5, 7, 9, 12)]
cold.response.connect <- cold.response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(cold.response.connect)[1] <- "Type"
colnames(cold.response.connect)[2] <- "Predictor"
colnames(cold.response.connect)[8] <- "Sym.sign"
cold.response.connect$Response <- "connect"
cold.response.connect <- cold.response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.connect[2:6, 3] <- "DIRECT" 
cold.response.connect[8:10, 3] <- "INDIRECT"  
cold.response.connect[12:16, 3] <- "TOTAL"    

cold.response.maxtl <- as.data.frame(thermtraj.cold.sem.eff$Summary$maxtl)
head(cold.response.maxtl)
cold.response.maxtl <- cold.response.maxtl[, -c(3, 5, 7, 9, 12)]
cold.response.maxtl <- cold.response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(cold.response.maxtl)[1] <- "Type"
colnames(cold.response.maxtl)[2] <- "Predictor"
colnames(cold.response.maxtl)[8] <- "Sym.sign"
cold.response.maxtl$Response <- "maxtl"
cold.response.maxtl <- cold.response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.maxtl[2:7, 3] <- "DIRECT" 
cold.response.maxtl[9:13, 3] <- "INDIRECT"  
cold.response.maxtl[15:20, 3] <- "TOTAL"   

cold.response.ols.slope <- as.data.frame(thermtraj.cold.sem.eff$Summary$ols.slope)
head(cold.response.ols.slope)
cold.response.ols.slope <- cold.response.ols.slope[, -c(3, 5, 7, 9, 12)]
cold.response.ols.slope <- cold.response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(cold.response.ols.slope)[1] <- "Type"
colnames(cold.response.ols.slope)[2] <- "Predictor"
colnames(cold.response.ols.slope)[8] <- "Sym.sign"
cold.response.ols.slope$Response <- "ols.slope"
cold.response.ols.slope <- cold.response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.ols.slope[2:4, 3] <- "DIRECT" 
cold.response.ols.slope[6:7, 3] <- "INDIRECT"  
cold.response.ols.slope[9:11, 3] <- "TOTAL"    

cold.response.ols.elevation <- as.data.frame(thermtraj.cold.sem.eff$Summary$ols.elevation)
head(cold.response.ols.elevation)
cold.response.ols.elevation <- cold.response.ols.elevation[, -c(3, 5, 7, 9, 12)]
cold.response.ols.elevation <- cold.response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(cold.response.ols.elevation)[1] <- "Type"
colnames(cold.response.ols.elevation)[2] <- "Predictor"
colnames(cold.response.ols.elevation)[8] <- "Sym.sign"
cold.response.ols.elevation$Response <- "ols.elevation"
cold.response.ols.elevation <- cold.response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.ols.elevation[2:4, 3] <- "DIRECT" 
cold.response.ols.elevation[6:7, 3] <- "INDIRECT"  
cold.response.ols.elevation[9:11, 3] <- "TOTAL"    

cold.response.fish.richness <- as.data.frame(thermtraj.cold.sem.eff$Summary$fish.richness)
head(cold.response.fish.richness)
cold.response.fish.richness <- cold.response.fish.richness[, -c(3, 5, 7, 9, 12)]
cold.response.fish.richness <- cold.response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(cold.response.fish.richness)[1] <- "Type"
colnames(cold.response.fish.richness)[2] <- "Predictor"
colnames(cold.response.fish.richness)[8] <- "Sym.sign"
cold.response.fish.richness$Response <- "fish.richness"
cold.response.fish.richness <- cold.response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.fish.richness[2:3, 3] <- "DIRECT" 
cold.response.fish.richness[5, 3] <- "INDIRECT"  
cold.response.fish.richness[7:8, 3] <- "TOTAL"    

cold.response.nis.richness <- as.data.frame(thermtraj.cold.sem.eff$Summary$nis.richness)
head(cold.response.nis.richness)
cold.response.nis.richness <- cold.response.nis.richness[, -c(3, 5, 7, 9, 12)]
cold.response.nis.richness <- cold.response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(cold.response.nis.richness)[1] <- "Type"
colnames(cold.response.nis.richness)[2] <- "Predictor"
colnames(cold.response.nis.richness)[8] <- "Sym.sign"
cold.response.nis.richness$Response <- "nis.richness"
cold.response.nis.richness <- cold.response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
cold.response.nis.richness[2, 3] <- "DIRECT" 
cold.response.nis.richness[5, 3] <- "TOTAL"    

cold.response.bio1.current <- as.data.frame(thermtraj.cold.sem.eff$Summary$bio1.current)
head(cold.response.bio1.current)
cold.response.bio1.current <- cold.response.bio1.current[, -c(3, 5, 7, 9, 12)]
cold.response.bio1.current <- cold.response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(cold.response.bio1.current)[1] <- "Type"
colnames(cold.response.bio1.current)[2] <- "Predictor"
colnames(cold.response.bio1.current)[8] <- "Sym.sign"
cold.response.bio1.current$Response <- "bio1.current"
cold.response.bio1.current <- cold.response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(cold.response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")

cold.effect <- rbind(cold.response.connect, cold.response.maxtl,
                     cold.response.ols.slope, cold.response.ols.elevation,
                     cold.response.fish.richness, cold.response.nis.richness,
                     cold.response.bio1.current)
str(cold.effect)
cold.effect$Predictor <- str_replace(cold.effect$Predictor, "AsIs", "")
cold.effect$Predictor <- str_split(cold.effect$Predictor, ",") %>% sapply(as.character)
cold.effect$Type <- str_replace(cold.effect$Type, "AsIs", "")
cold.effect$Type <- str_split(cold.effect$Type, ",") %>% sapply(as.character)
cold.effect$Effect <- str_replace(cold.effect$Effect, "AsIs", "")
cold.effect$Effect <- str_split(cold.effect$Effect, ",") %>% sapply(as.numeric)
cold.effect$Bias<- str_replace(cold.effect$Bias, "AsIs", "")
cold.effect$Bias <- str_split(cold.effect$Bias, ",") %>% sapply(as.numeric)
cold.effect$StdErr <- str_replace(cold.effect$StdErr, "AsIs", "")
cold.effect$StdErr <- str_split(cold.effect$StdErr, ",") %>% sapply(as.numeric)
cold.effect$LowerCI <- str_replace(cold.effect$LowerCI, "AsIs", "")
cold.effect$LowerCI <- str_split(cold.effect$LowerCI, ",") %>% sapply(as.numeric)
cold.effect$UpperCI <- str_replace(cold.effect$UpperCI, "AsIs", "")
cold.effect$UpperCI <- str_split(cold.effect$UpperCI, ",") %>% sapply(as.numeric)
cold.effect$SymbSign <- str_replace(cold.effect$SymbSign, "AsIs", "")
cold.effect$SymbSign <- str_split(cold.effect$SymbSign, ",") %>% sapply(as.character)

str(cold.effect)

unique(cold.effect$Predictor)
unique(cold.effect$Response)
unique(cold.effect$Type)

cold.effect[cold.effect == "connect"] <- "Connectance"
cold.effect[cold.effect == "maxtl"] <- "Maximum trophic level"
cold.effect[cold.effect == "ols.slope"] <- "Slope"
cold.effect[cold.effect == "ols.elevation"] <- "Elevation"
cold.effect[cold.effect == "fish.richness"] <- "Total sp. richness"
cold.effect[cold.effect == "nis.richness"] <- "Exotic sp. richness"
cold.effect[cold.effect == "bio1.current"] <- "Climatic conditions"
cold.effect[cold.effect == "bio1.slope.40y"] <- "Climate warming"

cold.effect[cold.effect == "connect       "] <- "Connectance"
cold.effect[cold.effect == "maxtl"] <- "Maximum trophic level"
cold.effect[cold.effect == "ols.slope     "] <- "Slope"
cold.effect[cold.effect == "ols.elevation "] <- "Elevation"
cold.effect[cold.effect == "fish.richness "] <- "Total sp. richness"
cold.effect[cold.effect == "nis.richness  "] <- "Exotic sp. richness"
cold.effect[cold.effect == "bio1.current  "] <- "Climatic conditions"

cold.effect[cold.effect == "DIRECT"] <- "Direct"
cold.effect[cold.effect == "INDIRECT"] <- "Indirect"
cold.effect[cold.effect == "TOTAL"] <- "Total"
cold.effect[cold.effect == "DIRECT   "] <- "Direct"
cold.effect[cold.effect == "INDIRECT "] <- "Indirect"
cold.effect[cold.effect == "TOTAL    "] <- "Total"

str(cold.effect)
cold.effect$Response <- factor(cold.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination.cold <- cold.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.cold.effect <- cold.effect %>% dplyr::filter(Type == "Indirect")
indirect.cold.effect.absent <- anti_join(all.combination.cold, indirect.cold.effect %>% dplyr::select(Predictor, Response))
indirect.cold.effect.absent$Type <- "Indirect"
indirect.cold.effect.absent$Effect <- 0
indirect.cold.effect.absent$Bias <- 0
indirect.cold.effect.absent$StdErr <- 0
indirect.cold.effect.absent$LowerCI <- 0
indirect.cold.effect.absent$UpperCI <- 0
indirect.cold.effect.absent$SymbSign <- ""
cold.effect <- rbind(cold.effect, indirect.cold.effect.absent)
cold.effect$significant <- NA
cold.effect$significant[cold.effect$SymbSign == "*"] <- TRUE
cold.effect$significant[cold.effect$SymbSign == ""] <- FALSE
cold.effect <- cold.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
#sem_effect_cold_lakes <- cold.effect
#mysave(sem_effect_cold_lakes, dir = "outputs/SEM", overwrite = TRUE)
rm(sem_effect_cold_lakes, data.cold, thermtraj_cold, thermtraj_cold_model, thermtraj.cold.sem, thermtraj.cold.sem.boot, thermtraj.cold.sem.eff, cold.response.connect, cold.response.maxtl, cold.response.ols.slope, cold.response.ols.elevation, cold.response.fish.richness, cold.response.nis.richness, cold.response.bio1.current, all.combination.cold, indirect.cold.effect, indirect.cold.effect.absent)




##SEM WITH "MODERATE" LAKES - DIRECT, INDIRECT AND TOTAL EFFECTS
data.moderate <- data %>% dplyr::filter(type.quant.33.66 == "moderate")

thermtraj_moderate <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.moderate)
)

# Use the `psem` function to create the SEM
thermtraj_moderate_model <- as.psem(thermtraj_moderate)
summary(thermtraj_moderate_model)
#dSep(thermtraj_moderate_model) #Can use `dsep` function to perform the tests automatically
#fisherC(thermtraj_moderate_model)  # Can use `fisherC` function to evaluate claims. Low p-value implies that the hypothesized structure is statistically different than the structure implied by our data. => a low p-value is actually bad as the model doesn't match the associations that are implied by the data
#coefs(thermtraj_moderate_model) #Get coefficients from good-fitting SEM

# Direct and indirect effect [https://murphymv.github.io/semEff/articles/semEff.html]
thermtraj.moderate.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.moderate),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.moderate),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.moderate)
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.moderate.sem.boot <- bootEff(thermtraj.moderate.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.moderate.sem.eff <- semEff(thermtraj.moderate.sem.boot))
summary(thermtraj.moderate.sem.eff, response = "connect")
summary(thermtraj.moderate.sem.eff, response = "maxtl")
summary(thermtraj.moderate.sem.eff, response = "ols.slope")
summary(thermtraj.moderate.sem.eff, response = "ols.elevation")
summary(thermtraj.moderate.sem.eff, response = "fish.richness")
summary(thermtraj.moderate.sem.eff, response = "nis.richness")
summary(thermtraj.moderate.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
moderate.response.connect <- as.data.frame(thermtraj.moderate.sem.eff$Summary$connect)
head(moderate.response.connect)
moderate.response.connect <- moderate.response.connect[, -c(3, 5, 7, 9, 12)]
moderate.response.connect <- moderate.response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(moderate.response.connect)[1] <- "Type"
colnames(moderate.response.connect)[2] <- "Predictor"
colnames(moderate.response.connect)[8] <- "Sym.sign"
moderate.response.connect$Response <- "connect"
moderate.response.connect <- moderate.response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.connect[2:6, 3] <- "DIRECT" 
moderate.response.connect[8:10, 3] <- "INDIRECT"  
moderate.response.connect[12:16, 3] <- "TOTAL"    

moderate.response.maxtl <- as.data.frame(thermtraj.moderate.sem.eff$Summary$maxtl)
head(moderate.response.maxtl)
moderate.response.maxtl <- moderate.response.maxtl[, -c(3, 5, 7, 9, 12)]
moderate.response.maxtl <- moderate.response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(moderate.response.maxtl)[1] <- "Type"
colnames(moderate.response.maxtl)[2] <- "Predictor"
colnames(moderate.response.maxtl)[8] <- "Sym.sign"
moderate.response.maxtl$Response <- "maxtl"
moderate.response.maxtl <- moderate.response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.maxtl[2:7, 3] <- "DIRECT" 
moderate.response.maxtl[9:13, 3] <- "INDIRECT"  
moderate.response.maxtl[15:20, 3] <- "TOTAL"   

moderate.response.ols.slope <- as.data.frame(thermtraj.moderate.sem.eff$Summary$ols.slope)
head(moderate.response.ols.slope)
moderate.response.ols.slope <- moderate.response.ols.slope[, -c(3, 5, 7, 9, 12)]
moderate.response.ols.slope <- moderate.response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(moderate.response.ols.slope)[1] <- "Type"
colnames(moderate.response.ols.slope)[2] <- "Predictor"
colnames(moderate.response.ols.slope)[8] <- "Sym.sign"
moderate.response.ols.slope$Response <- "ols.slope"
moderate.response.ols.slope <- moderate.response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.ols.slope[2:4, 3] <- "DIRECT" 
moderate.response.ols.slope[6:7, 3] <- "INDIRECT"  
moderate.response.ols.slope[9:11, 3] <- "TOTAL"    

moderate.response.ols.elevation <- as.data.frame(thermtraj.moderate.sem.eff$Summary$ols.elevation)
head(moderate.response.ols.elevation)
moderate.response.ols.elevation <- moderate.response.ols.elevation[, -c(3, 5, 7, 9, 12)]
moderate.response.ols.elevation <- moderate.response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(moderate.response.ols.elevation)[1] <- "Type"
colnames(moderate.response.ols.elevation)[2] <- "Predictor"
colnames(moderate.response.ols.elevation)[8] <- "Sym.sign"
moderate.response.ols.elevation$Response <- "ols.elevation"
moderate.response.ols.elevation <- moderate.response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.ols.elevation[2:4, 3] <- "DIRECT" 
moderate.response.ols.elevation[6:7, 3] <- "INDIRECT"  
moderate.response.ols.elevation[9:11, 3] <- "TOTAL"    

moderate.response.fish.richness <- as.data.frame(thermtraj.moderate.sem.eff$Summary$fish.richness)
head(moderate.response.fish.richness)
moderate.response.fish.richness <- moderate.response.fish.richness[, -c(3, 5, 7, 9, 12)]
moderate.response.fish.richness <- moderate.response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(moderate.response.fish.richness)[1] <- "Type"
colnames(moderate.response.fish.richness)[2] <- "Predictor"
colnames(moderate.response.fish.richness)[8] <- "Sym.sign"
moderate.response.fish.richness$Response <- "fish.richness"
moderate.response.fish.richness <- moderate.response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.fish.richness[2:3, 3] <- "DIRECT" 
moderate.response.fish.richness[5, 3] <- "INDIRECT"  
moderate.response.fish.richness[7:8, 3] <- "TOTAL"    

moderate.response.nis.richness <- as.data.frame(thermtraj.moderate.sem.eff$Summary$nis.richness)
head(moderate.response.nis.richness)
moderate.response.nis.richness <- moderate.response.nis.richness[, -c(3, 5, 7, 9, 12)]
moderate.response.nis.richness <- moderate.response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(moderate.response.nis.richness)[1] <- "Type"
colnames(moderate.response.nis.richness)[2] <- "Predictor"
colnames(moderate.response.nis.richness)[8] <- "Sym.sign"
moderate.response.nis.richness$Response <- "nis.richness"
moderate.response.nis.richness <- moderate.response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
moderate.response.nis.richness[2, 3] <- "DIRECT" 
moderate.response.nis.richness[5, 3] <- "TOTAL"    

moderate.response.bio1.current <- as.data.frame(thermtraj.moderate.sem.eff$Summary$bio1.current)
head(moderate.response.bio1.current)
moderate.response.bio1.current <- moderate.response.bio1.current[, -c(3, 5, 7, 9, 12)]
moderate.response.bio1.current <- moderate.response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(moderate.response.bio1.current)[1] <- "Type"
colnames(moderate.response.bio1.current)[2] <- "Predictor"
colnames(moderate.response.bio1.current)[8] <- "Sym.sign"
moderate.response.bio1.current$Response <- "bio1.current"
moderate.response.bio1.current <- moderate.response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(moderate.response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")

moderate.effect <- rbind(moderate.response.connect, moderate.response.maxtl,
                     moderate.response.ols.slope, moderate.response.ols.elevation,
                     moderate.response.fish.richness, moderate.response.nis.richness,
                     moderate.response.bio1.current)
str(moderate.effect)
moderate.effect$Predictor <- str_replace(moderate.effect$Predictor, "AsIs", "")
moderate.effect$Predictor <- str_split(moderate.effect$Predictor, ",") %>% sapply(as.character)
moderate.effect$Type <- str_replace(moderate.effect$Type, "AsIs", "")
moderate.effect$Type <- str_split(moderate.effect$Type, ",") %>% sapply(as.character)
moderate.effect$Effect <- str_replace(moderate.effect$Effect, "AsIs", "")
moderate.effect$Effect <- str_split(moderate.effect$Effect, ",") %>% sapply(as.numeric)
moderate.effect$Bias<- str_replace(moderate.effect$Bias, "AsIs", "")
moderate.effect$Bias <- str_split(moderate.effect$Bias, ",") %>% sapply(as.numeric)
moderate.effect$StdErr <- str_replace(moderate.effect$StdErr, "AsIs", "")
moderate.effect$StdErr <- str_split(moderate.effect$StdErr, ",") %>% sapply(as.numeric)
moderate.effect$LowerCI <- str_replace(moderate.effect$LowerCI, "AsIs", "")
moderate.effect$LowerCI <- str_split(moderate.effect$LowerCI, ",") %>% sapply(as.numeric)
moderate.effect$UpperCI <- str_replace(moderate.effect$UpperCI, "AsIs", "")
moderate.effect$UpperCI <- str_split(moderate.effect$UpperCI, ",") %>% sapply(as.numeric)
moderate.effect$SymbSign <- str_replace(moderate.effect$SymbSign, "AsIs", "")
moderate.effect$SymbSign <- str_split(moderate.effect$SymbSign, ",") %>% sapply(as.character)

str(moderate.effect)

unique(moderate.effect$Predictor)
unique(moderate.effect$Response)
unique(moderate.effect$Type)

moderate.effect[moderate.effect == "connect"] <- "Connectance"
moderate.effect[moderate.effect == "maxtl"] <- "Maximum trophic level"
moderate.effect[moderate.effect == "ols.slope"] <- "Slope"
moderate.effect[moderate.effect == "ols.elevation"] <- "Elevation"
moderate.effect[moderate.effect == "fish.richness"] <- "Total sp. richness"
moderate.effect[moderate.effect == "nis.richness"] <- "Exotic sp. richness"
moderate.effect[moderate.effect == "bio1.current"] <- "Climatic conditions"
moderate.effect[moderate.effect == "bio1.slope.40y"] <- "Climate warming"

moderate.effect[moderate.effect == "connect       "] <- "Connectance"
moderate.effect[moderate.effect == "maxtl"] <- "Maximum trophic level"
moderate.effect[moderate.effect == "ols.slope     "] <- "Slope"
moderate.effect[moderate.effect == "ols.elevation "] <- "Elevation"
moderate.effect[moderate.effect == "fish.richness "] <- "Total sp. richness"
moderate.effect[moderate.effect == "nis.richness  "] <- "Exotic sp. richness"
moderate.effect[moderate.effect == "bio1.current  "] <- "Climatic conditions"

moderate.effect[moderate.effect == "DIRECT"] <- "Direct"
moderate.effect[moderate.effect == "INDIRECT"] <- "Indirect"
moderate.effect[moderate.effect == "TOTAL"] <- "Total"
moderate.effect[moderate.effect == "DIRECT   "] <- "Direct"
moderate.effect[moderate.effect == "INDIRECT "] <- "Indirect"
moderate.effect[moderate.effect == "TOTAL    "] <- "Total"

str(moderate.effect)
moderate.effect$Response <- factor(moderate.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination.moderate <- moderate.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.moderate.effect <- moderate.effect %>% dplyr::filter(Type == "Indirect")
indirect.moderate.effect.absent <- anti_join(all.combination.moderate, indirect.moderate.effect %>% dplyr::select(Predictor, Response))
indirect.moderate.effect.absent$Type <- "Indirect"
indirect.moderate.effect.absent$Effect <- 0
indirect.moderate.effect.absent$Bias <- 0
indirect.moderate.effect.absent$StdErr <- 0
indirect.moderate.effect.absent$LowerCI <- 0
indirect.moderate.effect.absent$UpperCI <- 0
indirect.moderate.effect.absent$SymbSign <- ""
moderate.effect <- rbind(moderate.effect, indirect.moderate.effect.absent)
moderate.effect$significant <- NA
moderate.effect$significant[moderate.effect$SymbSign == "*"] <- TRUE
moderate.effect$significant[moderate.effect$SymbSign == ""] <- FALSE
moderate.effect <- moderate.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
#sem_effect_moderate_lakes <- moderate.effect
#mysave(sem_effect_moderate_lakes, dir = "outputs/SEM", overwrite = TRUE)
rm(sem_effect_moderate_lakes, data.moderate, thermtraj_moderate, thermtraj_moderate_model, thermtraj.moderate.sem, thermtraj.moderate.sem.boot, thermtraj.moderate.sem.eff, moderate.response.connect, moderate.response.maxtl, moderate.response.ols.slope, moderate.response.ols.elevation, moderate.response.fish.richness, moderate.response.nis.richness, moderate.response.bio1.current, all.combination.moderate, indirect.moderate.effect, indirect.moderate.effect.absent)




##SEM WITH "WARM" LAKES - DIRECT, INDIRECT AND TOTAL EFFECTS
data.warm <- data %>% dplyr::filter(type.quant.33.66 == "warm")

thermtraj_warm <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.warm)
)

# Use the `psem` function to create the SEM
thermtraj_warm_model <- as.psem(thermtraj_warm)
summary(thermtraj_warm_model)
#dSep(thermtraj_warm_model) #Can use `dsep` function to perform the tests automatically
#fisherC(thermtraj_warm_model)  # Can use `fisherC` function to evaluate claims. Low p-value implies that the hypothesized structure is statistically different than the structure implied by our data. => a low p-value is actually bad as the model doesn't match the associations that are implied by the data
#coefs(thermtraj_warm_model) #Get coefficients from good-fitting SEM

# Direct and indirect effect [https://murphymv.github.io/semEff/articles/semEff.html]
thermtraj.warm.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.warm),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.warm),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.warm)
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.warm.sem.boot <- bootEff(thermtraj.warm.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.warm.sem.eff <- semEff(thermtraj.warm.sem.boot))
summary(thermtraj.warm.sem.eff, response = "connect")
summary(thermtraj.warm.sem.eff, response = "maxtl")
summary(thermtraj.warm.sem.eff, response = "ols.slope")
summary(thermtraj.warm.sem.eff, response = "ols.elevation")
summary(thermtraj.warm.sem.eff, response = "fish.richness")
summary(thermtraj.warm.sem.eff, response = "nis.richness")
summary(thermtraj.warm.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
warm.response.connect <- as.data.frame(thermtraj.warm.sem.eff$Summary$connect)
head(warm.response.connect)
warm.response.connect <- warm.response.connect[, -c(3, 5, 7, 9, 12)]
warm.response.connect <- warm.response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(warm.response.connect)[1] <- "Type"
colnames(warm.response.connect)[2] <- "Predictor"
colnames(warm.response.connect)[8] <- "Sym.sign"
warm.response.connect$Response <- "connect"
warm.response.connect <- warm.response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.connect[2:6, 3] <- "DIRECT" 
warm.response.connect[8:10, 3] <- "INDIRECT"  
warm.response.connect[12:16, 3] <- "TOTAL"    

warm.response.maxtl <- as.data.frame(thermtraj.warm.sem.eff$Summary$maxtl)
head(warm.response.maxtl)
warm.response.maxtl <- warm.response.maxtl[, -c(3, 5, 7, 9, 12)]
warm.response.maxtl <- warm.response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(warm.response.maxtl)[1] <- "Type"
colnames(warm.response.maxtl)[2] <- "Predictor"
colnames(warm.response.maxtl)[8] <- "Sym.sign"
warm.response.maxtl$Response <- "maxtl"
warm.response.maxtl <- warm.response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.maxtl[2:7, 3] <- "DIRECT" 
warm.response.maxtl[9:13, 3] <- "INDIRECT"  
warm.response.maxtl[15:20, 3] <- "TOTAL"   

warm.response.ols.slope <- as.data.frame(thermtraj.warm.sem.eff$Summary$ols.slope)
head(warm.response.ols.slope)
warm.response.ols.slope <- warm.response.ols.slope[, -c(3, 5, 7, 9, 12)]
warm.response.ols.slope <- warm.response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(warm.response.ols.slope)[1] <- "Type"
colnames(warm.response.ols.slope)[2] <- "Predictor"
colnames(warm.response.ols.slope)[8] <- "Sym.sign"
warm.response.ols.slope$Response <- "ols.slope"
warm.response.ols.slope <- warm.response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.ols.slope[2:4, 3] <- "DIRECT" 
warm.response.ols.slope[6:7, 3] <- "INDIRECT"  
warm.response.ols.slope[9:11, 3] <- "TOTAL"    

warm.response.ols.elevation <- as.data.frame(thermtraj.warm.sem.eff$Summary$ols.elevation)
head(warm.response.ols.elevation)
warm.response.ols.elevation <- warm.response.ols.elevation[, -c(3, 5, 7, 9, 12)]
warm.response.ols.elevation <- warm.response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(warm.response.ols.elevation)[1] <- "Type"
colnames(warm.response.ols.elevation)[2] <- "Predictor"
colnames(warm.response.ols.elevation)[8] <- "Sym.sign"
warm.response.ols.elevation$Response <- "ols.elevation"
warm.response.ols.elevation <- warm.response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.ols.elevation[2:4, 3] <- "DIRECT" 
warm.response.ols.elevation[6:7, 3] <- "INDIRECT"  
warm.response.ols.elevation[9:11, 3] <- "TOTAL"    

warm.response.fish.richness <- as.data.frame(thermtraj.warm.sem.eff$Summary$fish.richness)
head(warm.response.fish.richness)
warm.response.fish.richness <- warm.response.fish.richness[, -c(3, 5, 7, 9, 12)]
warm.response.fish.richness <- warm.response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(warm.response.fish.richness)[1] <- "Type"
colnames(warm.response.fish.richness)[2] <- "Predictor"
colnames(warm.response.fish.richness)[8] <- "Sym.sign"
warm.response.fish.richness$Response <- "fish.richness"
warm.response.fish.richness <- warm.response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.fish.richness[2:3, 3] <- "DIRECT" 
warm.response.fish.richness[5, 3] <- "INDIRECT"  
warm.response.fish.richness[7:8, 3] <- "TOTAL"    

warm.response.nis.richness <- as.data.frame(thermtraj.warm.sem.eff$Summary$nis.richness)
head(warm.response.nis.richness)
warm.response.nis.richness <- warm.response.nis.richness[, -c(3, 5, 7, 9, 12)]
warm.response.nis.richness <- warm.response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(warm.response.nis.richness)[1] <- "Type"
colnames(warm.response.nis.richness)[2] <- "Predictor"
colnames(warm.response.nis.richness)[8] <- "Sym.sign"
warm.response.nis.richness$Response <- "nis.richness"
warm.response.nis.richness <- warm.response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
warm.response.nis.richness[2, 3] <- "DIRECT" 
warm.response.nis.richness[5, 3] <- "TOTAL"    

warm.response.bio1.current <- as.data.frame(thermtraj.warm.sem.eff$Summary$bio1.current)
head(warm.response.bio1.current)
warm.response.bio1.current <- warm.response.bio1.current[, -c(3, 5, 7, 9, 12)]
warm.response.bio1.current <- warm.response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(warm.response.bio1.current)[1] <- "Type"
colnames(warm.response.bio1.current)[2] <- "Predictor"
colnames(warm.response.bio1.current)[8] <- "Sym.sign"
warm.response.bio1.current$Response <- "bio1.current"
warm.response.bio1.current <- warm.response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(warm.response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")

warm.effect <- rbind(warm.response.connect, warm.response.maxtl,
                         warm.response.ols.slope, warm.response.ols.elevation,
                         warm.response.fish.richness, warm.response.nis.richness,
                         warm.response.bio1.current)
str(warm.effect)
warm.effect$Predictor <- str_replace(warm.effect$Predictor, "AsIs", "")
warm.effect$Predictor <- str_split(warm.effect$Predictor, ",") %>% sapply(as.character)
warm.effect$Type <- str_replace(warm.effect$Type, "AsIs", "")
warm.effect$Type <- str_split(warm.effect$Type, ",") %>% sapply(as.character)
warm.effect$Effect <- str_replace(warm.effect$Effect, "AsIs", "")
warm.effect$Effect <- str_split(warm.effect$Effect, ",") %>% sapply(as.numeric)
warm.effect$Bias<- str_replace(warm.effect$Bias, "AsIs", "")
warm.effect$Bias <- str_split(warm.effect$Bias, ",") %>% sapply(as.numeric)
warm.effect$StdErr <- str_replace(warm.effect$StdErr, "AsIs", "")
warm.effect$StdErr <- str_split(warm.effect$StdErr, ",") %>% sapply(as.numeric)
warm.effect$LowerCI <- str_replace(warm.effect$LowerCI, "AsIs", "")
warm.effect$LowerCI <- str_split(warm.effect$LowerCI, ",") %>% sapply(as.numeric)
warm.effect$UpperCI <- str_replace(warm.effect$UpperCI, "AsIs", "")
warm.effect$UpperCI <- str_split(warm.effect$UpperCI, ",") %>% sapply(as.numeric)
warm.effect$SymbSign <- str_replace(warm.effect$SymbSign, "AsIs", "")
warm.effect$SymbSign <- str_split(warm.effect$SymbSign, ",") %>% sapply(as.character)

str(warm.effect)

unique(warm.effect$Predictor)
unique(warm.effect$Response)
unique(warm.effect$Type)

warm.effect[warm.effect == "connect"] <- "Connectance"
warm.effect[warm.effect == "maxtl"] <- "Maximum trophic level"
warm.effect[warm.effect == "ols.slope"] <- "Slope"
warm.effect[warm.effect == "ols.elevation"] <- "Elevation"
warm.effect[warm.effect == "fish.richness"] <- "Total sp. richness"
warm.effect[warm.effect == "nis.richness"] <- "Exotic sp. richness"
warm.effect[warm.effect == "bio1.current"] <- "Climatic conditions"
warm.effect[warm.effect == "bio1.slope.40y"] <- "Climate warming"

warm.effect[warm.effect == "connect       "] <- "Connectance"
warm.effect[warm.effect == "maxtl"] <- "Maximum trophic level"
warm.effect[warm.effect == "ols.slope     "] <- "Slope"
warm.effect[warm.effect == "ols.elevation "] <- "Elevation"
warm.effect[warm.effect == "fish.richness "] <- "Total sp. richness"
warm.effect[warm.effect == "nis.richness  "] <- "Exotic sp. richness"
warm.effect[warm.effect == "bio1.current  "] <- "Climatic conditions"

warm.effect[warm.effect == "DIRECT"] <- "Direct"
warm.effect[warm.effect == "INDIRECT"] <- "Indirect"
warm.effect[warm.effect == "TOTAL"] <- "Total"
warm.effect[warm.effect == "DIRECT   "] <- "Direct"
warm.effect[warm.effect == "INDIRECT "] <- "Indirect"
warm.effect[warm.effect == "TOTAL    "] <- "Total"

str(warm.effect)
warm.effect$Response <- factor(warm.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination.warm <- warm.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.warm.effect <- warm.effect %>% dplyr::filter(Type == "Indirect")
indirect.warm.effect.absent <- anti_join(all.combination.warm, indirect.warm.effect %>% dplyr::select(Predictor, Response))
indirect.warm.effect.absent$Type <- "Indirect"
indirect.warm.effect.absent$Effect <- 0
indirect.warm.effect.absent$Bias <- 0
indirect.warm.effect.absent$StdErr <- 0
indirect.warm.effect.absent$LowerCI <- 0
indirect.warm.effect.absent$UpperCI <- 0
indirect.warm.effect.absent$SymbSign <- ""
warm.effect <- rbind(warm.effect, indirect.warm.effect.absent)
warm.effect$significant <- NA
warm.effect$significant[warm.effect$SymbSign == "*"] <- TRUE
warm.effect$significant[warm.effect$SymbSign == ""] <- FALSE
warm.effect <- warm.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
#sem_effect_warm_lakes <- warm.effect
#mysave(sem_effect_warm_lakes, dir = "outputs/SEM", overwrite = TRUE)
rm(sem_effect_warm_lakes, data.warm, thermtraj_warm, thermtraj_warm_model, thermtraj.warm.sem, thermtraj.warm.sem.boot, thermtraj.warm.sem.eff, warm.response.connect, warm.response.maxtl, warm.response.ols.slope, warm.response.ols.elevation, warm.response.fish.richness, warm.response.nis.richness, warm.response.bio1.current, all.combination.warm, indirect.warm.effect, indirect.warm.effect.absent)


