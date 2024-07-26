rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(kableExtra)
library(lme4)
library(piecewiseSEM)
library(purrr)
library(semEff)
library(stringr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggbeeswarm)
library(gghalves)

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

detail.indirect.eff <- thermtraj.sem.eff[["All Indirect Effects"]]

connectance <- as.data.frame(detail.indirect.eff$connect)
colnames(connectance) <- "effect"
connectance$path <- rownames(connectance)
rownames(connectance) <- NULL
connectance <- connectance %>% dplyr::select(path, effect)
connectance$predictor <- sub(".*\\.", "", connectance$path)
connectance$predictor[connectance$predictor == "nis_richness"] <- "Exotic species richness"
connectance$predictor[connectance$predictor == "fish_richness"] <- "Total species richness"
connectance$predictor[connectance$predictor == "bio1_current"] <- "Climatic conditions"
connectance$predictor[connectance$predictor == "bio1_slope_40y"] <- "Climate warming"
connectance$response <- "Connectance"

maxtl <- as.data.frame(detail.indirect.eff$maxtl)
colnames(maxtl) <- "effect"
maxtl$path <- rownames(maxtl)
rownames(maxtl) <- NULL
maxtl <- maxtl %>% dplyr::select(path, effect)
maxtl$predictor <- sub(".*\\.", "", maxtl$path)
maxtl$predictor[maxtl$predictor == "ols_slope"] <- "CSS slope"
maxtl$predictor[maxtl$predictor == "ols_elevation"] <- "CSS elevation"
maxtl$predictor[maxtl$predictor == "nis_richness"] <- "Exotic species richness"
maxtl$predictor[maxtl$predictor == "fish_richness"] <- "Total species richness"
maxtl$predictor[maxtl$predictor == "bio1_current"] <- "Climatic conditions"
maxtl$predictor[maxtl$predictor == "bio1_slope_40y"] <- "Climate warming"
maxtl$response <- "Maximum trophic level"

css.slope <- as.data.frame(detail.indirect.eff$ols.slope)
colnames(css.slope) <- "effect"
css.slope$path <- rownames(css.slope)
rownames(css.slope) <- NULL
css.slope <- css.slope %>% dplyr::select(path, effect)
css.slope$predictor <- sub(".*\\.", "", css.slope$path)
css.slope$predictor[css.slope$predictor == "nis_richness"] <- "Exotic species richness"
css.slope$predictor[css.slope$predictor == "bio1_current"] <- "Climatic conditions"
css.slope$predictor[css.slope$predictor == "bio1_slope_40y"] <- "Climate warming"
css.slope$response <- "CSS slope"

css.elevation <- as.data.frame(detail.indirect.eff$ols.elevation)
colnames(css.elevation) <- "effect"
css.elevation$path <- rownames(css.elevation)
rownames(css.elevation) <- NULL
css.elevation <- css.elevation %>% dplyr::select(path, effect)
css.elevation$predictor <- sub(".*\\.", "", css.elevation$path)
css.elevation$predictor[css.elevation$predictor == "nis_richness"] <- "Exotic species richness"
css.elevation$predictor[css.elevation$predictor == "bio1_current"] <- "Climatic conditions"
css.elevation$predictor[css.elevation$predictor == "bio1_slope_40y"] <- "Climate warming"
css.elevation$response <- "CSS elevation"

tot.sp <- as.data.frame(detail.indirect.eff$fish.richness)
colnames(tot.sp) <- "effect"
tot.sp$path <- rownames(tot.sp)
rownames(tot.sp) <- NULL
tot.sp <- tot.sp %>% dplyr::select(path, effect)
tot.sp$predictor <- sub(".*\\.", "", tot.sp$path)
tot.sp$predictor[tot.sp$predictor == "bio1_current"] <- "Climatic conditions"
tot.sp$predictor[tot.sp$predictor == "bio1_slope_40y"] <- "Climate warming"
tot.sp$response <- "Total species richness"

DIE <- rbind(connectance, maxtl, css.slope, css.elevation, tot.sp)
colnames(DIE) <- c('path', 'effect', 'Predictor', 'response')
str(DIE)
ggplot(DIE, aes(x = response, y = effect)) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  geom_half_boxplot(nudge = 0.05, outlier.color = NA, color = "black") +
  geom_half_point(aes(fill = Predictor), size = 3, shape = 21, color = "black", position=position_jitterdodge(jitter.width = 0, dodge.width = 0)) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1)) +
  labs(y = "Standardized effet", x = "Response variables") +
  theme(axis.text = element_text(size = 14, colour = "#000000"),
        axis.title = element_text(size = 16, face = "bold", colour = "#000000"))
#4 x 3 

#sem_details_indirect_effect_all_lakes<- DIE
#mysave(sem_details_indirect_effect_all_lakes, dir = "outputs/SEM", overwrite = TRUE)
