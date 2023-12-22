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
list_of_files <- list.files(path = "./data/lake_temperature", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)


##read and merge with one colomn for lake_ID
daily_temp_okp <- list_of_files %>% rlang::set_names(.) %>% map_df(read_table2, .id = "Lake_ID")
head(daily_temp_okp)
class(daily_temp_okp)
daily_temp_okp = as.data.frame(daily_temp_okp)


##get read of useless symbols
daily_temp_okp$Lake_ID <- str_sub(daily_temp_okp$Lake_ID, start = 48, end = 52)
daily_temp_okp$Lake_ID[daily_temp_okp$Lake_ID == "GRO21"] <- "GRO21b"
daily_temp_okp$Lake_ID[daily_temp_okp$Lake_ID == "LER27"] <- "LER27a"
head(daily_temp_okp)

##break date down to calculate metrics later
daily_temp_okp$year <- year(daily_temp_okp$date)
daily_temp_okp$month <- month(daily_temp_okp$date)
daily_temp_okp$day <- day(daily_temp_okp$date)
head(daily_temp_okp)
daily_temp_okp <- daily_temp_okp %>% dplyr::select(Lake_ID, date, year, month, day, tepi)

rm(list_of_files)


date_fish_sampling <- read.csv("./outputs/date_fish_sampling.txt", sep="")
length(unique(date_fish_sampling$code_lac))  




##-----------------------------------------
##COMPUTE MONTHLY MEAN TEMPERATURE PER YEAR
##-----------------------------------------
monthly_temp_okp <- as.data.frame(matrix(nrow = 0, ncol = 7, dimnames = list(NULL, c("Lake_ID", "date", "year", "month", "tmean.epi", "tmin.epi", "tmax.epi"))))

for(i in 1:length(unique(daily_temp_okp$Lake_ID))) {
  df1 <- daily_temp_okp %>% filter(Lake_ID == unique(daily_temp_okp$Lake_ID)[i])
  
  for(j in 1:length(unique(df1$year))) {
    df2 <- df1 %>% filter(year == unique(df1$year)[j])
    
    for(k in 1:length(unique(df2$month))) { 
      
      lake <- df2 %>% filter(month == unique(df2$month)[k])
      monthly_temp_okp <- rbind(monthly_temp_okp, as.data.frame(matrix(c(unique(lake$Lake_ID), unique(format(as.Date(lake$date), "%Y-%m")), unique(lake$year), unique(lake$month), mean(lake$tepi), min(lake$tepi), max(lake$tepi)), nrow = 1, ncol = 7, dimnames = list(NULL, c("Lake_ID", "date", "year", "month", "tmean.epi", "tmin.epi", "tmax.epi")))))
      
    }
  }
}

rm(lake, i, j, k, df1, df2) 


##--------------------------------
##COMPUTE CURRENT MEAN TEMPERATURE
##--------------------------------
##compute on a bioclim and on a 5y period
period <- 5

bio1 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio1) <- c("lake.code", "year.samp", "id.samp", "bio1.current")
bio2 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio2) <- c("lake.code", "year.samp", "id.samp", "bio2.current")
bio3 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio3) <- c("lake.code", "year.samp", "id.samp", "bio3.current")
bio4 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio4) <- c("lake.code", "year.samp", "id.samp", "bio4.current")
bio5 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio5) <- c("lake.code", "year.samp", "id.samp", "bio5.current")
bio6 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio6) <- c("lake.code", "year.samp", "id.samp", "bio6.current")
bio7 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio7) <- c("lake.code", "year.samp", "id.samp", "bio7.current")
bio10 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio10) <- c("lake.code", "year.samp", "id.samp", "bio10.current")
bio11 <- data.frame(matrix(nrow = 0, ncol = 4)) ; colnames(bio11) <- c("lake.code", "year.samp", "id.samp", "bio11.current")



for(i in 1:nrow(date_fish_sampling)){
  for(j in 1:length(period)){
    
    sub_monthly_temp_okp <- monthly_temp_okp %>% dplyr::filter(Lake_ID == date_fish_sampling[i, 1])
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::filter(date >= format(as.Date(ymd(date_fish_sampling$date_pose[i]) - years(period[j])), "%Y-%m"))
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::filter(date < format(as.Date(date_fish_sampling$date_pose[i]), "%Y-%m"))

    sub_monthly_temp_okp$year2 <- rep(as.numeric(min(sub_monthly_temp_okp$year)):(max(as.numeric(max(sub_monthly_temp_okp$year)))-1), each = 12)
    sub_monthly_temp_okp <- sub_monthly_temp_okp %>% dplyr::select(Lake_ID, date, year2, tmean.epi, tmin.epi, tmax.epi)
    colnames(sub_monthly_temp_okp) <- c("Lake_ID", "date", "year", "tmean.epi", "tmin.epi", "tmax.epi")
    sub_monthly_temp_okp$tmean.epi <- as.numeric(sub_monthly_temp_okp$tmean.epi)
    sub_monthly_temp_okp$tmin.epi <- as.numeric(sub_monthly_temp_okp$tmin.epi)
    sub_monthly_temp_okp$tmax.epi <- as.numeric(sub_monthly_temp_okp$tmax.epi)
    
    
    
    biovars_epi <- as.data.frame(matrix(nrow = 0, ncol = 11, dimnames = list(NULL, c("Lake_ID", "year", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio10", "bio11"))))

    for(k in 1:length(unique(sub_monthly_temp_okp$year))) {
      
      lake <- sub_monthly_temp_okp %>% filter(year == unique(sub_monthly_temp_okp$year)[k])
      lake$prc.epi <- 0
      
      epi <- as.data.frame(biovars(lake$prc.epi, lake$tmin.epi, lake$tmax.epi))
      
      biovars_epi <- rbind(biovars_epi, as.data.frame(matrix(c(unique(lake$Lake_ID), unique(lake$year), epi$bio1, epi$bio2, epi$bio3, epi$bio4/100, epi$bio5, epi$bio6, epi$bio7, epi$bio10, epi$bio11), nrow = 1, ncol = 11, dimnames = list(NULL, c("LakeID", "period", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio10", "bio11")))))

    }
    rm(k, lake, epi)
    
    
    
    
    biovars_epi$bio1 <- as.numeric(biovars_epi$bio1)
    biovars_epi$bio2 <- as.numeric(biovars_epi$bio2)
    biovars_epi$bio3 <- as.numeric(biovars_epi$bio3)
    biovars_epi$bio4 <- as.numeric(biovars_epi$bio4)
    biovars_epi$bio5 <- as.numeric(biovars_epi$bio5)
    biovars_epi$bio6 <- as.numeric(biovars_epi$bio6)
    biovars_epi$bio7 <- as.numeric(biovars_epi$bio7)
    biovars_epi$bio10 <- as.numeric(biovars_epi$bio10)
    biovars_epi$bio11 <- as.numeric(biovars_epi$bio11)
    
    
    ##BIO1##
    sub_bio1 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio1) <- c("lake.code", "year.samp", "id.samp", "bio1.current")
    sub_bio1[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio1[1, 4] <- mean(biovars_epi$bio1)
    bio1 <- rbind(bio1, sub_bio1)
    
    
    ##BIO2##
    sub_bio2 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio2) <- c("lake.code", "year.samp", "id.samp", "bio2.current")
    sub_bio2[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio2[1, 4] <- mean(biovars_epi$bio2)
    bio2 <- rbind(bio2, sub_bio2)
    
    
    ##BIO3##
    sub_bio3 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio3) <- c("lake.code", "year.samp", "id.samp", "bio3.current")
    sub_bio3[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio3[1, 4] <- mean(biovars_epi$bio3)
    bio3 <- rbind(bio3, sub_bio3)
    
    
    ##BIO4##
    sub_bio4 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio4) <- c("lake.code", "year.samp", "id.samp", "bio4.current")
    sub_bio4[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio4[1, 4] <- mean(biovars_epi$bio4)
    bio4 <- rbind(bio4, sub_bio4)
    
    
    ##BIO5##
    sub_bio5 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio5) <- c("lake.code", "year.samp", "id.samp", "bio5.current")
    sub_bio5[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio5[1, 4] <- mean(biovars_epi$bio5)
    bio5 <- rbind(bio5, sub_bio5)
    
    
    ##BIO6##
    sub_bio6 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio6) <- c("lake.code", "year.samp", "id.samp", "bio6.current")
    sub_bio6[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio6[1, 4] <- mean(biovars_epi$bio6)
    bio6 <- rbind(bio6, sub_bio6)
    
    
    ##BIO7##
    sub_bio7 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio7) <- c("lake.code", "year.samp", "id.samp", "bio7.current")
    sub_bio7[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio7[1, 4] <- mean(biovars_epi$bio7)
    bio7 <- rbind(bio7, sub_bio7)
    
    
    ##BIO10##
    sub_bio10 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio10) <- c("lake.code", "year.samp", "id.samp", "bio10.current")
    sub_bio10[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio10[1, 4] <- mean(biovars_epi$bio10)
    bio10 <- rbind(bio10, sub_bio10)
    
    
    ##BIO11##
    sub_bio11 <- data.frame(matrix(nrow = 1, ncol = 4)) ; colnames(sub_bio11) <- c("lake.code", "year.samp", "id.samp", "bio11.current")
    sub_bio11[1, 1:3] <- date_fish_sampling[i, 1:3]
    sub_bio11[1, 4] <- mean(biovars_epi$bio11)
    bio11 <- rbind(bio11, sub_bio11)
    
    
    
    
    rm(sub_monthly_temp_okp, sub_bio1, sub_bio2, sub_bio3, sub_bio4, sub_bio5, sub_bio6, sub_bio7, sub_bio10, sub_bio11, biovars_epi)
    
  }
}

rm(i, j, period)




datasetTempCurrentBioclim_258L <- left_join(bio1, bio2 %>% dplyr::select(id.samp, bio2.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio3 %>% dplyr::select(id.samp, bio3.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio4 %>% dplyr::select(id.samp, bio4.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio5 %>% dplyr::select(id.samp, bio5.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio6 %>% dplyr::select(id.samp, bio6.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio7 %>% dplyr::select(id.samp, bio7.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio10 %>% dplyr::select(id.samp, bio10.current), by = "id.samp")
datasetTempCurrentBioclim_258L <- left_join(datasetTempCurrentBioclim_258L, bio11 %>% dplyr::select(id.samp, bio11.current), by = "id.samp")

mysave(datasetTempCurrentBioclim_258L, dir = "./outputs/Temperature", overwrite = TRUE)
