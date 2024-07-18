#https://cran.r-project.org/web/packages/modifiedmk/modifiedmk.pdf

rm(list=ls()) #Removes all objects from the current workspace (R memory)


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(dismo)
library(dplyr)
library(lubridate)
library(modifiedmk)
library(plyr)
library(purrr)
library(readr)
library(stringr)




##FUNCTIONS##
source("./rfunctions/misc.R")


##-------------------------
##DOWNLOAD TEMPERATURE DATA
##-------------------------
##get all text files of temperature data (okp model)
list_of_files <- list.files(path = "./data/lake_temperature/Uncertainty_data", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

##read and merge with one colomn for lake_ID
uncertainty_temp_okp <- list_of_files %>% rlang::set_names(.) %>% map_df(read_table2, .id = "Lake_ID")
head(uncertainty_temp_okp)
class(uncertainty_temp_okp)
uncertainty_temp_okp = as.data.frame(uncertainty_temp_okp)


##get read of useless symbols
uncertainty_temp_okp$Lake_ID <- str_sub(uncertainty_temp_okp$Lake_ID, start = 54, end = 58)
uncertainty_temp_okp$Lake_ID[uncertainty_temp_okp$Lake_ID == "GRO21"] <- "GRO21b"
uncertainty_temp_okp$Lake_ID[uncertainty_temp_okp$Lake_ID == "LER27"] <- "LER27a"
head(uncertainty_temp_okp)


##break date down to calculate metrics later
uncertainty_temp_okp$year <- year(uncertainty_temp_okp$time)
uncertainty_temp_okp$month <- month(uncertainty_temp_okp$time)
uncertainty_temp_okp$day <- day(uncertainty_temp_okp$time)
head(uncertainty_temp_okp)
uncertainty_temp_okp <- uncertainty_temp_okp %>% dplyr::select(Lake_ID, time, year, month, day, p05, p95)

rm(list_of_files)


date_fish_sampling <- read.csv("./outputs/date_fish_sampling.txt", sep="")
length(unique(date_fish_sampling$code_lac))  
date_fish_sampling <- date_fish_sampling %>% dplyr::filter(code_lac != "PRA66")




##-----------------------------------------
##COMPUTE MONTHLY MEAN TEMPERATURE PER YEAR
##-----------------------------------------
monthly_uncertainty_temp_okp <- as.data.frame(matrix(nrow = 0, ncol = 6, dimnames = list(NULL, c("Lake_ID", "time", "year", "month", "p05", "p95"))))

for(i in 1:length(unique(uncertainty_temp_okp$Lake_ID))) {
  df1 <- uncertainty_temp_okp %>% filter(Lake_ID == unique(uncertainty_temp_okp$Lake_ID)[i])
  
  for(j in 1:length(unique(df1$year))) {
    df2 <- df1 %>% filter(year == unique(df1$year)[j])
    
    for(k in 1:length(unique(df2$month))) { 
      
      lake <- df2 %>% filter(month == unique(df2$month)[k])
      monthly_uncertainty_temp_okp <- rbind(monthly_uncertainty_temp_okp, as.data.frame(matrix(c(unique(lake$Lake_ID), unique(format(as.Date(lake$time), "%Y-%m")), unique(lake$year), unique(lake$month), mean(lake$p05), mean(lake$p95)), nrow = 1, ncol = 6, dimnames = list(NULL, c("Lake_ID", "date", "year", "month", "p05", "p95")))))
      
    }
  }
}

rm(lake, i, j, k, df1, df2) 
str(monthly_uncertainty_temp_okp)
monthly_uncertainty_temp_okp$year <- as.numeric(monthly_uncertainty_temp_okp$year)
monthly_uncertainty_temp_okp$month <- as.numeric(monthly_uncertainty_temp_okp$month)
monthly_uncertainty_temp_okp$p05 <- as.numeric(monthly_uncertainty_temp_okp$p05)
monthly_uncertainty_temp_okp$p95 <- as.numeric(monthly_uncertainty_temp_okp$p95)


##----------------------
##COMPUTE TEMPORAL TREND
##----------------------
##compute on a bioclim and on a 40y period
period <- 5
uncertainty <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(uncertainty) <- c("code_lac", "camp_annee", "id_campagne", "p05", "p95")


for(i in 1:nrow(date_fish_sampling)){
  for(j in 1:length(period)){
    
    sub_monthly_temp_okp <- monthly_uncertainty_temp_okp %>% dplyr::filter(Lake_ID == date_fish_sampling[i, 1])
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::filter(date >= format(as.Date(ymd(date_fish_sampling$date_pose[i]) - years(period[j])), "%Y-%m"))
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::filter(date < format(as.Date(date_fish_sampling$date_pose[i]), "%Y-%m"))
    
    sub_monthly_temp_okp$year2 <- rep(as.numeric(min(sub_monthly_temp_okp$year)):(max(as.numeric(max(sub_monthly_temp_okp$year)))-1), each = 12)
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::select(Lake_ID, date, year2, p05, p95)
    colnames(sub_monthly_temp_okp) <- c("Lake_ID", "date", "year", "p05", "p95")
    
    annual.p05 <- aggregate(sub_monthly_temp_okp$p05, list(sub_monthly_temp_okp$year), FUN=mean) 
    annual.p95 <- aggregate(sub_monthly_temp_okp$p95, list(sub_monthly_temp_okp$year), FUN=mean) 
    
    sub_uncertainty <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(sub_uncertainty) <- c("code_lac", "camp_annee", "id_campagne", "p05", "p95")
    
    sub_uncertainty[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_uncertainty[1, 4] <- mean(annual.p05[,2])
    sub_uncertainty[1, 5] <- mean(annual.p95[,2])
    
    uncertainty <- rbind(uncertainty, sub_uncertainty)
    
    rm(sub_monthly_temp_okp, annual.p05, annual.p95, sub_uncertainty)
  }
}

rm(i, j, period)
  

uncertainty_temperature_5y_period <- uncertainty
mysave(uncertainty_temperature_5y_period, dir = "./outputs/Temperature", overwrite = TRUE)
