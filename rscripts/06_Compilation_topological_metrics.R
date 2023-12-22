rm(list = ls()) #Removes all objects from the current workspace (R memory)


##------------------------------
##LOADING PACKAGES AND FUNCTIONS
##------------------------------
##PACKAGES##
library(dplyr)
library(ggplot2)
library(PupillometryR)
library(plyr)
library(stringr)
library(tidyr)


##FUNCTIONS##
source("./rfunctions/misc.R")


##--------------
## LOAD DATASETS
##--------------
myload(sampling_protocol, station, dir = "outputs")


##---------------------------------------------------------
##COMPILING A UNIQUE FILE OF UNWEIGHTED TOPOLOGICAL METRICS
##---------------------------------------------------------
mydir <- "./outputs/FoodWebs/TopologicalMetrics/"
myfiles <- list.files(path = mydir, pattern = "*.txt", full.names = TRUE)
topological_metrics <- ldply(myfiles, read.table, sep = "", fill = TRUE, header = TRUE)
id.campagne <- as.data.frame(str_match(myfiles, "_ID\\s*(.*?)\\s*.txt")[,2])
colnames(id.campagne) <- "id_campagne"
topological_metrics <- cbind(id.campagne, topological_metrics)
topological_metrics$id_campagne <- as.numeric(topological_metrics$id_campagne)
topological_metrics <- left_join(topological_metrics,
                                 station %>% dplyr::select(id_campagne, code_lac, camp_annee) %>% unique(.),
                                 by = 'id_campagne')
topological_metrics <- topological_metrics[,c(22:23, 1:21)]
mysave(topological_metrics, dir = "./outputs/FoodWebs", overwrite = TRUE)




##-------------------------------------------------------
##PLOTTING DITRISBUTION OF UNWEIGHTED TOPOLOGICAL METRICS
##-------------------------------------------------------
colnames(topological_metrics)
rownames(topological_metrics) <- paste(topological_metrics$code_lac, "_", topological_metrics$camp_annee)
topological_metrics$code_lac <- NULL ; topological_metrics$camp_annee <- NULL ; topological_metrics$id_campagne <- NULL
colnames(topological_metrics) <- c("Fish rich.", "No. nodes",
                                    "Fish size",
                                    "No. links", "Link dens.", "Connect.",
                                    "Gen.", "Vul.", "Gen. SD", "Vul. SD",
                                    "Frac. bas.", "Frac. int.", "Frac. top",
                                    "Max. Sim.", "MFCL",
                                    "Mean TL", "Max. TL", "Cluster. coef.", "Modular.","PredOverlap")

colnames(topological_metrics)
sapply(topological_metrics, summary, na.rm = TRUE)
sapply(topological_metrics, sd, na.rm = TRUE)

data <- topological_metrics %>% gather(key = "text", value = "value")
summary(data)
data <- mutate_at(data, vars(text), as.factor)


p <- data %>%
      ggplot(aes(x = text, y = value), color = "#bebebe", fill = "#000000") +
      geom_point(aes(y = value), position = position_jitter(width = 0.25, height = 0, seed = NULL), size = 1, alpha = 0.5) +
      geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.8) +
      PupillometryR::geom_flat_violin(trim = FALSE, position = position_nudge(x = 0.3, y = 0), alpha = 0.8) +
      facet_wrap(~text, ncol = 6, scales = "free") +
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         legend.position = "none",
                         axis.text = element_text(size = 16, colour = "#000000"), 
                         axis.line.x = element_line(color = "#000000"), 
                         axis.line.y = element_line(color = "#000000"),
                         strip.text.x = element_text(size = 20, face = "bold"),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank()) +
      labs(y = NULL, x = NULL)
p #save pdf 18x9
