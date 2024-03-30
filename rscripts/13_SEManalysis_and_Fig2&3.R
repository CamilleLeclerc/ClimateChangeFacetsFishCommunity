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
source("./rfunctions/vif_func.R")


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


##---------
##CHECK VIF
##---------
data <- dataset.thermal.trajectories %>% dplyr::select(lake.code, year.samp, fish.richness, nis.richness, ols.slope, ols.elevation, connect, maxtl, bio1.slope.40y, bio1.current)         
summary(data)
str(data)
data$lake.code <- as.factor(data$lake.code)
keep.dat <- vif_func(in_frame = data %>% dplyr::select(fish.richness, nis.richness, ols.slope, ols.elevation, connect, maxtl, bio1.slope.40y, bio1.current), thresh = 3, trace = T)


##-----------------------------
## STRUCTURAL EQUATION MODELING
##-----------------------------

# Break down component regressions with random AND autocorrelation structures
thermtraj_lme <- list(
  connect = lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  maxtl = lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  ols.slope = lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  ols.elevation = lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  fish.richness = lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  nis.richness = lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  bio1.current = lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data)
)


# Use the `psem` function to create the SEM
thermtraj_model <- as.psem(thermtraj_lme)
summary(thermtraj_model)

# Can use `dsep` function to perform the tests automatically
dSep(thermtraj_model)

# Can use `fisherC` function to evaluate claims
fisherC(thermtraj_model) # low p-value implies that the hypothesized structure is statistically different than the structure implied by our data
                  #=> a low p-value is actually bad as the model doesn't match the associations that are implied by the data

# Get coefficients from good-fitting SEM
coefs(thermtraj_model) #Std.Estimate takes the raw estimate and scale it by the ratio of standard deviation between predictor and response variables
                #unitless
                #Standardized estimates are in units of standard deviations of the mean
                # Can be directly compared even though initial units are very different

# Plot SEM with standardized coefficients
plot(thermtraj_model)
piecewiseSEM:::plot.psem(
  piecewiseSEM::as.psem(thermtraj_model),
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "grey"),
  layout = "tree"
)


#Direct and indirect effect
#https://murphymv.github.io/semEff/articles/semEff.html

thermtraj.sem <- list(
  lmer(connect ~ fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  lmer(maxtl ~ connect + fish.richness + nis.richness + ols.slope + ols.elevation + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  lmer(ols.slope ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  lmer(ols.elevation ~ fish.richness + nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  lmer(fish.richness ~ nis.richness + bio1.slope.40y + bio1.current + (1|lake.code), data = data),
  lmer(nis.richness ~ bio1.slope.40y + bio1.current + (1 | lake.code), data = data),
  lmer(bio1.current ~ bio1.slope.40y + (1|lake.code), data = data)
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


#figure about all effects (total, indirect, direct)
pCAMT <- ggplot(data.effect %>% dplyr::filter(Predictor == "Climatic conditions"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 14, colour = "#000000"),
          axis.title = element_text(size = 16, face = "bold", colour = "#000000")) 
pCAMT


pTAMT <- ggplot(data.effect %>% dplyr::filter(Predictor == "Climate warming"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTAMT


pNISR <- ggplot(data.effect %>% dplyr::filter(Predictor == "Exotic sp. richness"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pNISR


pTSR <- ggplot(data.effect %>% dplyr::filter(Predictor == "Total sp. richness"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTSR


pSlope <- ggplot(data.effect %>% dplyr::filter(Predictor == "Slope"),
               aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pSlope


pMP <- ggplot(data.effect %>% dplyr::filter(Predictor == "Elevation"),
                 aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effect size", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pMP



ggarrange(pCAMT, pTAMT, 
          pNISR, pTSR,
          pSlope, pMP,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3,
          common.legend = TRUE, legend="bottom")

plot_grid(pCAMT, pTAMT, 
          pNISR, pTSR,
          pSlope, pMP,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "hv")

#landscape 10x15






#figure about total effects
data.total.effect <- data.effect %>% dplyr::filter(Type == "Total")


pCAMT <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Climatic conditions"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) 
pCAMT #4x6


pTAMT <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Climate warming"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTAMT


pNISR <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Exotic sp. richness"),
                aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pNISR


pTSR <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Total sp. richness"),
               aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTSR


pSlope <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Slope"),
                 aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pSlope


pMP <- ggplot(data.total.effect %>% dplyr::filter(Predictor == "Elevation"),
              aes(x = Effect, y = Response)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized total effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pMP



plot_grid(pCAMT, pTAMT, 
          pNISR, pTSR,
          pSlope, pMP,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "hv")

#landscape 10x15






#figure about direct and indirect effects
#https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
data.direct.indirect.effect <- data.effect %>% dplyr::filter(!Type == "Total")


pCAMT <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Climatic conditions"),
                aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#8d96a3",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000")) 
pCAMT


pTAMT <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Climate warming"),
                aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#8d96a3") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#8d96a3",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTAMT


pNISR <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Exotic sp. richness"),
                aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#66a182",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pNISR


pTSR <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Total sp. richness"),
               aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#66a182") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#66a182",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pTSR


pSlope <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Slope"),
                 aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#edae49",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pSlope


pMP <- ggplot(data.direct.indirect.effect %>% dplyr::filter(Predictor == "Elevation"),
              aes(x = Effect, y = Response, pattern = factor(Type))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  #geom_bar(aes(alpha = factor(Type)), stat = "identity", position = "dodge", width = 0.75, colour = "black", fill = "#edae49") +
  geom_bar_pattern(stat='identity', position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   fill = "#edae49",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI, fill = Type), position = position_dodge(0.75), width = 0.25, fill = "black") +
  geom_text(aes(label = ifelse(significant, "*", ""), x = position, fill = Type), position = position_dodge(0.75), size = 20 / .pt) +
  labs(x = "Standardized effects", y = "Reponse variable") +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-0.4, 0.627), position = "top") +
  coord_flex_cart(left=brackets_vertical(), top = capped_horizontal('both')) +
  scale_pattern_manual(values = c(Indirect = "stripe", Direct = "none")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line()) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
pMP



plot_grid(pCAMT, pTAMT, 
          pNISR, pTSR,
          pSlope, pMP,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "hv")

#landscape 10x15
