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


path = file.path("/Users", "evaphillips", "Library/CloudStorage/OneDrive-nyu.edu/right-to-counsel-capstone-data/01_Eviction/eviction_tracking_system", fsep="/")
setwd(path)


#read in a dataset
ets_all_sites <- read.csv("all_sites_monthly_2020_2021.csv")

#Cleveland
ets_cleveland <- ets_all_sites %>% filter(city == "Cleveland, OH")

class(ets_cleveland$month)

ets_cleveland_year <- ets_cleveland %>%
  group_by(`month`) %>%
  summarise(filings_2020 = sum(filings_2020),
            filings_avg = sum(filings_avg, na.rm = T)) %>%
  mutate(month = mdy(month)) %>%
  pivot_longer(cols = filings_2020:filings_avg,
             names_to = "year",
             values_to = "filings",
             names_prefix = "filings_") %>% 
  mutate(year = recode(year,
                       '2020' = "2020-2023",
                       avg = "2016-2019 comparison")) 

ets_cleveland_year %>% filter(year == "2020-2023") %>%
  ggplot(aes(x = month,
             y = filings)) +
  geom_line() +
  labs(title = "Cleveland Monthly Eviction Filings")

#Kansas City
ets_KS <- ets_all_sites %>% filter(city == "Kansas City, MO")
ets_KS_year <- ets_KS %>%
  group_by(`month`) %>%
  summarise(filings_2020 = sum(filings_2020),
            filings_avg = sum(filings_avg, na.rm = T)) %>%
  mutate(month = mdy(month)) %>%
  pivot_longer(cols = filings_2020:filings_avg,
               names_to = "year",
               values_to = "filings",
               names_prefix = "filings_") %>% 
  mutate(year = recode(year,
                       '2020' = "2020-2023",
                       avg = "2016-2019 comparison")) 

ets_KS_year %>% filter(year == "2020-2023") %>%
  ggplot(aes(x = month,
             y = filings)) +
  geom_line() +
  labs(title = "Kansas City Monthly Eviction Filings")


#Kansas City
ets_NOLA <- ets_all_sites %>% filter(city == "New Orleans, LA")
ets_NOLA_year <- ets_NOLA %>%
  group_by(`month`) %>%
  summarise(filings_2020 = sum(filings_2020),
            filings_avg = sum(filings_avg, na.rm = T)) %>%
  mutate(month = mdy(month)) %>%
  pivot_longer(cols = filings_2020:filings_avg,
               names_to = "year",
               values_to = "filings",
               names_prefix = "filings_") %>% 
  mutate(year = recode(year,
                       '2020' = "2020-2023",
                       avg = "2016-2019 comparison")) 

ets_NOLA_year %>% filter(year == "2020-2023") %>%
  ggplot(aes(x = month,
             y = filings)) +
  geom_line() +
  labs(title = "NOLA Monthly Eviction Filings")

#Kansas City
ets_PA <- ets_all_sites %>% filter(city == "Philadelphia, PA")
ets_PA_year <- ets_PA %>%
  group_by(`month`) %>%
  summarise(filings_2020 = sum(filings_2020),
            filings_avg = sum(filings_avg, na.rm = T)) %>%
  mutate(month = mdy(month)) %>%
  pivot_longer(cols = filings_2020:filings_avg,
               names_to = "year",
               values_to = "filings",
               names_prefix = "filings_") %>% 
  mutate(year = recode(year,
                       '2020' = "2020-2023",
                       avg = "2016-2019 comparison")) 

ets_PA_year %>% filter(year == "2020-2023") %>%
  ggplot(aes(x = month,
             y = filings)) +
  geom_line() +
  labs(title = "Philadelphia Monthly Eviction Filings")



