# Header -----------------------------------------------------------------------

# PROJECT: ETS Data Validation
# AUTHOR: Eva Phillips
# DATE: 2023-010-30

# PURPOSE: Explore and validate Eviction Tracking data from Eviction Lab


# Setup ------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(grid)
library(gridExtra)
library(readxl)
library(lubridate)

# ------------------------------------------------------------------------


path = file.path("/Users", "evaphillips", "Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/baseline_2000-2018", fsep="/")
setwd(path)


#read in a dataset
all_tracts <- read.csv("tract_proprietary_valid_2000_2018.csv")

#NYC
nyc_tracts <- all_tracts %>% filter(county %in% c('Kings County', 'New York County', 'Queens County', 'Bronx', 'Richmond County') & state == 'New York')

nyc_tracts_sum <- nyc_tracts  %>%
  group_by(county, year) %>%
  summarise(filings = sum(filings, na.rm = TRUE), 
            avg_rt = mean(filing_rate, na.rm=TRUE), 
            ) 

nyc_tracts_sum %>% filter(year >=2010) %>%
  ggplot(aes(x = year,
             y = filings)) +
  geom_line(aes(color = county)) +
  labs(title = "Total Filings by Year")

nyc_tracts_sum  %>% filter(year >=2010) %>%
  ggplot(aes(x = year,
             y = avg_rt)) +
  geom_line(aes(color = county)) +
  labs(title = "Avg Filing Rate by Year")
