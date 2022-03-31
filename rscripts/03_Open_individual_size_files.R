rm(list=ls())

##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(tidyverse)

mypath <- rprojroot::find_package_root_file
source(mypath("rfunctions", "misc.R"))
source_dir(mypath("rfunctions"))

##--------------
## LOAD DATASETS
##--------------
myload(ind_size, sampling_protocol, station, dir = mypath("outputs"))
