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

# Can use `dsep` function to perform the tests automatically
dSep(thermtraj_cold_model)

# Can use `fisherC` function to evaluate claims
fisherC(thermtraj_cold_model) # low p-value implies that the hypothesized structure is statistically different than the structure implied by our data
#=> a low p-value is actually bad as the model doesn't match the associations that are implied by the data

# Get coefficients from good-fitting SEM
coefs(thermtraj_cold_model)

#Direct and indirect effect
#https://murphymv.github.io/semEff/articles/semEff.html
thermtraj.cold.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data.cold),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data.cold),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data.cold)
)


piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(thermtraj.sem),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)

#to calculate effects, we first bootstrap and save the standardised direct effects:
system.time(
  thermtraj.sem.boot <- bootEff(thermtraj.sem, R = 1000, seed = 13, ran.eff = "lake.code")
)

#using the bootstrapped estimates we can now calculate direct, indirect, and total effects
(thermtraj.sem.eff <- semEff(thermtraj.sem.boot))
summary(thermtraj.sem.eff, response = "connect")
summary(thermtraj.sem.eff, response = "maxtl")
summary(thermtraj.sem.eff, response = "ols.slope")
summary(thermtraj.sem.eff, response = "ols.elevation")
summary(thermtraj.sem.eff, response = "fish.richness")
summary(thermtraj.sem.eff, response = "nis.richness")
summary(thermtraj.sem.eff, response = "bio1.current")

#preparation dataset to plot figure about effects
response.connect <- as.data.frame(thermtraj.sem.eff$Summary$connect)
head(response.connect)
response.connect <- response.connect[, -c(3, 5, 7, 9, 12)]
response.connect <- response.connect %>% slice(-c(1, 8, 13, 20:25))
colnames(response.connect)[1] <- "Type"
colnames(response.connect)[2] <- "Predictor"
colnames(response.connect)[8] <- "Sym.sign"
response.connect$Response <- "connect"
response.connect <- response.connect %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.connect) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.connect[2:6, 3] <- "DIRECT" 
response.connect[8:10, 3] <- "INDIRECT"  
response.connect[12:16, 3] <- "TOTAL"    

response.maxtl <- as.data.frame(thermtraj.sem.eff$Summary$maxtl)
head(response.maxtl)
response.maxtl <- response.maxtl[, -c(3, 5, 7, 9, 12)]
response.maxtl <- response.maxtl %>% slice(-c(1, 9, 16, 24:30))
colnames(response.maxtl)[1] <- "Type"
colnames(response.maxtl)[2] <- "Predictor"
colnames(response.maxtl)[8] <- "Sym.sign"
response.maxtl$Response <- "maxtl"
response.maxtl <- response.maxtl %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.maxtl) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.maxtl[2:7, 3] <- "DIRECT" 
response.maxtl[9:13, 3] <- "INDIRECT"  
response.maxtl[15:20, 3] <- "TOTAL"   

response.ols.slope <- as.data.frame(thermtraj.sem.eff$Summary$ols.slope)
head(response.ols.slope)
response.ols.slope <- response.ols.slope[, -c(3, 5, 7, 9, 12)]
response.ols.slope <- response.ols.slope %>% slice(-c(1, 6, 10, 15:18))
colnames(response.ols.slope)[1] <- "Type"
colnames(response.ols.slope)[2] <- "Predictor"
colnames(response.ols.slope)[8] <- "Sym.sign"
response.ols.slope$Response <- "ols.slope"
response.ols.slope <- response.ols.slope %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.ols.slope) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.ols.slope[2:4, 3] <- "DIRECT" 
response.ols.slope[6:7, 3] <- "INDIRECT"  
response.ols.slope[9:11, 3] <- "TOTAL"    

response.ols.elevation <- as.data.frame(thermtraj.sem.eff$Summary$ols.elevation)
head(response.ols.elevation)
response.ols.elevation <- response.ols.elevation[, -c(3, 5, 7, 9, 12)]
response.ols.elevation <- response.ols.elevation %>% slice(-c(1, 6, 10, 15:18))
colnames(response.ols.elevation)[1] <- "Type"
colnames(response.ols.elevation)[2] <- "Predictor"
colnames(response.ols.elevation)[8] <- "Sym.sign"
response.ols.elevation$Response <- "ols.elevation"
response.ols.elevation <- response.ols.elevation %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.ols.elevation) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.ols.elevation[2:4, 3] <- "DIRECT" 
response.ols.elevation[6:7, 3] <- "INDIRECT"  
response.ols.elevation[9:11, 3] <- "TOTAL"    

response.fish.richness <- as.data.frame(thermtraj.sem.eff$Summary$fish.richness)
head(response.fish.richness)
response.fish.richness <- response.fish.richness[, -c(3, 5, 7, 9, 12)]
response.fish.richness <- response.fish.richness %>% slice(-c(1, 5, 8, 12:14))
colnames(response.fish.richness)[1] <- "Type"
colnames(response.fish.richness)[2] <- "Predictor"
colnames(response.fish.richness)[8] <- "Sym.sign"
response.fish.richness$Response <- "fish.richness"
response.fish.richness <- response.fish.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.fish.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.fish.richness[2:3, 3] <- "DIRECT" 
response.fish.richness[5, 3] <- "INDIRECT"  
response.fish.richness[7:8, 3] <- "TOTAL"    

response.nis.richness <- as.data.frame(thermtraj.sem.eff$Summary$nis.richness)
head(response.nis.richness)
response.nis.richness <- response.nis.richness[, -c(3, 5, 7, 9, 12)]
response.nis.richness <- response.nis.richness %>% slice(-c(1, 4, 6, 9:10))
colnames(response.nis.richness)[1] <- "Type"
colnames(response.nis.richness)[2] <- "Predictor"
colnames(response.nis.richness)[8] <- "Sym.sign"
response.nis.richness$Response <- "nis.richness"
response.nis.richness <- response.nis.richness %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.nis.richness) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")
response.nis.richness[2, 3] <- "DIRECT" 
response.nis.richness[5, 3] <- "TOTAL"    

response.bio1.current <- as.data.frame(thermtraj.sem.eff$Summary$bio1.current)
head(response.bio1.current)
response.bio1.current <- response.bio1.current[, -c(3, 5, 7, 9, 12)]
response.bio1.current <- response.bio1.current %>% slice(-c(1, 3:5, 7:8))
colnames(response.bio1.current)[1] <- "Type"
colnames(response.bio1.current)[2] <- "Predictor"
colnames(response.bio1.current)[8] <- "Sym.sign"
response.bio1.current$Response <- "bio1.current"
response.bio1.current <- response.bio1.current %>% dplyr::select('Predictor', 'Response', 'Type', 'Effect', 'Bias', 'Std. Err.', 'Lower CI', 'Upper CI', 'Sym.sign')
colnames(response.bio1.current) <- c("Predictor", "Response", "Type", "Effect", "Bias", "StdErr", "LowerCI", "UpperCI", "SymbSign")

data.effect <- rbind(response.connect, response.maxtl,
                     response.ols.slope, response.ols.elevation,
                     response.fish.richness, response.nis.richness,
                     response.bio1.current)
str(data.effect)
data.effect$Predictor <- str_replace(data.effect$Predictor, "AsIs", "")
data.effect$Predictor <- str_split(data.effect$Predictor, ",") %>% sapply(as.character)
data.effect$Type <- str_replace(data.effect$Type, "AsIs", "")
data.effect$Type <- str_split(data.effect$Type, ",") %>% sapply(as.character)
data.effect$Effect <- str_replace(data.effect$Effect, "AsIs", "")
data.effect$Effect <- str_split(data.effect$Effect, ",") %>% sapply(as.numeric)
data.effect$Bias<- str_replace(data.effect$Bias, "AsIs", "")
data.effect$Bias <- str_split(data.effect$Bias, ",") %>% sapply(as.numeric)
data.effect$StdErr <- str_replace(data.effect$StdErr, "AsIs", "")
data.effect$StdErr <- str_split(data.effect$StdErr, ",") %>% sapply(as.numeric)
data.effect$LowerCI <- str_replace(data.effect$LowerCI, "AsIs", "")
data.effect$LowerCI <- str_split(data.effect$LowerCI, ",") %>% sapply(as.numeric)
data.effect$UpperCI <- str_replace(data.effect$UpperCI, "AsIs", "")
data.effect$UpperCI <- str_split(data.effect$UpperCI, ",") %>% sapply(as.numeric)
data.effect$SymbSign <- str_replace(data.effect$SymbSign, "AsIs", "")
data.effect$SymbSign <- str_split(data.effect$SymbSign, ",") %>% sapply(as.character)

str(data.effect)

unique(data.effect$Predictor)
unique(data.effect$Response)
unique(data.effect$Type)

data.effect[data.effect == "connect"] <- "Connectance"
data.effect[data.effect == "maxtl"] <- "Maximum trophic level"
data.effect[data.effect == "ols.slope"] <- "Slope"
data.effect[data.effect == "ols.elevation"] <- "Elevation"
data.effect[data.effect == "fish.richness"] <- "Total sp. richness"
data.effect[data.effect == "nis.richness"] <- "Exotic sp. richness"
data.effect[data.effect == "bio1.current"] <- "Climatic conditions"
data.effect[data.effect == "bio1.slope.40y"] <- "Climate warming"

data.effect[data.effect == "connect       "] <- "Connectance"
data.effect[data.effect == "maxtl"] <- "Maximum trophic level"
data.effect[data.effect == "ols.slope     "] <- "Slope"
data.effect[data.effect == "ols.elevation "] <- "Elevation"
data.effect[data.effect == "fish.richness "] <- "Total sp. richness"
data.effect[data.effect == "nis.richness  "] <- "Exotic sp. richness"
data.effect[data.effect == "bio1.current  "] <- "Climatic conditions"

data.effect[data.effect == "DIRECT"] <- "Direct"
data.effect[data.effect == "INDIRECT"] <- "Indirect"
data.effect[data.effect == "TOTAL"] <- "Total"
data.effect[data.effect == "DIRECT   "] <- "Direct"
data.effect[data.effect == "INDIRECT "] <- "Indirect"
data.effect[data.effect == "TOTAL    "] <- "Total"

str(data.effect)
data.effect$Response <- factor(data.effect$Response, levels = c('Maximum trophic level', 'Connectance', 'Elevation', 'Slope', 'Total sp. richness', 'Exotic sp. richness', 'Climatic conditions'), ordered = TRUE) 

all.combination <- data.effect %>% dplyr::select(Predictor, Response) %>% unique(.)
indirect.effect <- data.effect %>% dplyr::filter(Type == "Indirect")
indirect.effect.absent <- anti_join(all.combination, indirect.effect %>% dplyr::select(Predictor, Response))
indirect.effect.absent$Type <- "Indirect"
indirect.effect.absent$Effect <- 0
indirect.effect.absent$Bias <- 0
indirect.effect.absent$StdErr <- 0
indirect.effect.absent$LowerCI <- 0
indirect.effect.absent$UpperCI <- 0
indirect.effect.absent$SymbSign <- ""
data.effect <- rbind(data.effect, indirect.effect.absent)
data.effect$significant <- NA
data.effect$significant[data.effect$SymbSign == "*"] <- TRUE
data.effect$significant[data.effect$SymbSign == ""] <- FALSE
data.effect <- data.effect %>% mutate(position = if_else(Effect > 0, UpperCI + 0.02, LowerCI - 0.02))
