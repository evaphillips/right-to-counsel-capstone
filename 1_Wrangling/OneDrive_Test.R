# Header -----------------------------------------------------------------------

# PROJECT: Test OneDrive
# AUTHOR: Eva Phillips
# DATE: 2023-10-30

# PURPOSE: Test read/write to OneDrive shares folder

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(grid)
library(gridExtra)
library(readxl)

path = file.path("/Users", "evaphillips", "Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/eviction_tracking_system", fsep="/")

setwd(path)


# Test ------------------------------------------------------------------------

ets_all_sites <- read.csv("all_sites_monthly_2020_2021.csv")

ets_houston <- ets_all_sites %>% filter(city == "Houston, TX")

write_csv(ets_houston, "ets_houston-TEST.csv")