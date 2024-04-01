
# ------------------------------------------------------------------------------

# PROJECT: Universal Prek
# AUTHOR: Anuska 
# DATE: 2023-12-14

# PURPOSE: Generate table 1 descriptive stats

# Setup ------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(sf)
library(gtsummary)
library(survey)

df <- read.csv("/Users/anuskacorbin/Desktop/universal-pre-k/0_Data/filtered_sample_prek_allcities.csv") 

df <- df %>% filter(city %in% c('NYC', 'SD'))

#df_test <- df %>% filter(city == 'NYC') %>% select(HHWT) %>% sum()

df <- df %>% 
  mutate(MARST_clean = case_when(MARST == 1 ~ 'Married',
                                            MARST == 2 ~ 'Married',
                                            .default = 'Single')) %>%
  mutate(SEX_clean = case_when(SEX == 1 ~ 'Male',
                                 SEX == 2 ~ 'Female')) %>%
  mutate(MIGRATE_clean = case_when(MIGRATE1 == 1 ~ 'Non-movers',
                                   .default = 'Movers')) %>%
  mutate(EMPSTAT_clean = case_when(EMPSTAT == 1 ~ 'Employed',
                                   EMPSTAT == 2 ~ 'Unemployed',
                                   EMPSTAT == 3 ~ 'Not in Labor Force')) %>%
  mutate(RACE_clean = case_when(RACE == 1 ~ 'White',
                                   RACE == 2 ~ 'Black',
                                   RACE == 3 ~ 'American Indian or Alaska Native',
                                   RACE == 6 ~ 'Other Asian or Pacific Islander',
                                   RACE == 7 ~ 'Other race',
                                   RACE == 8 ~ 'Biracial',
                                   RACE == 9 ~ 'Three or more major races',
                                  .default = 'Asian'))
                              
  
  
  
  
  df_svy <- survey::svydesign(~1, weights = ~HHWT, data = df) %>%
    tbl_svysummary(by = city, 
                   include = c(MIGRATE_clean,SEX_clean,AGE,MARST_clean,EMPSTAT_clean,RACE_clean,NCHILD,YNGCH,POVERTY,HHINCOME,FTOTINC),
                   label = list(
                     MIGRATE_clean ~ "Migration",
                     SEX_clean ~ "Sex",
                     AGE ~ "Age",
                     MARST_clean ~ "Marital Status",
                     EMPSTAT_clean ~ "Employment Status",
                     RACE_clean ~ "Race",
                     NCHILD ~ "Number of Children",
                     YNGCH ~ "Youngest Child Age",
                     POVERTY ~ "Poverty Status",
                     HHINCOME ~ "Total Household Income",
                     FTOTINC ~ "Total Family Income")) %>% 
                      modify_header(
                        label = '**Demographic**',
                        stat_1 = '**New York City**',
                        stat_2 = '**San Diego**'
                      ) %>%
                      modify_spanning_header(all_stat_cols() ~ "**City**")
 
  df_svy
  
 
  --------------------
  