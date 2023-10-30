# Header -----------------------------------------------------------------------

# PROJECT: Test OneDrive
# AUTHOR: Eva Phillips
# DATE: 2023-10-30

# PURPOSE: Test read/write to OneDrive shared folder

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(grid)
library(gridExtra)
library(readxl)

#adjust to reflect your file path
path = file.path("/Users", "evaphillips", "Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/eviction_tracking_system", fsep="/")
setwd(path)


# Test ------------------------------------------------------------------------

#read in a dataset
ets_all_sites <- read.csv("all_sites_monthly_2020_2021.csv")
#filter
ets_houston <- ets_all_sites %>% filter(city == "Houston, TX")
#write csv to file path
write_csv(ets_houston, "ets_houston-TEST.csv")