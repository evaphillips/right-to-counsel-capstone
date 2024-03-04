
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
                                   EMPSTAT == 3 ~ 'Not in Labor Force'))
  
  
  
  df_svy <- survey::svydesign(~1, weights = ~HHWT, data = df) %>%
    tbl_svysummary(by = city, 
                   include = c(MIGRATE_clean,SEX_clean,AGE,MARST_clean,EMPSTAT_clean,NCHILD,ELDCH,YNGCH,FTOTINC),
                   label = list(
                     MIGRATE_clean ~ "Migration",
                     SEX_clean ~ "Sex",
                     AGE ~ "Age",
                     MARST_clean ~ "Marital Status",
                     EMPSTAT_clean ~ "Employment Status",
                     NCHILD ~ "Number of Children",
                     ELDCH ~ "Eldest Child Age",
                     YNGCH ~ "Youngest Child Age",
                     FTOTINC ~ "Family Total Income"))
 
  df_svy
  

  --------------------
  
  
  tbl_svysummary(
    by = city,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)",
    include = c(YEAR,NCHILD,ELDCH,YNGCH,SEX,AGE,MARST,RACE,HISPAN,HISPAND,EDUC,EMPSTAT,FTOTINC,POVERTY,MIGRATE1),
    label = list(
      NCHILD ~ "Number of Children",
      ELDCH ~ "Eldest Child Age",
      YNGCH ~ "Youngest Child Age",
      MARST ~ "Marital Status",
      EDUC ~ "Educational Attainment",
      EMPSTAT ~ "Employment Status",
      FTOTINC ~ "Family Total Income"
      )
  ) %>%
  add_p() %>%
  add_overall() %>%
    modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6") ~ "**City**"))

install.packages("summarize")
summarize(filtered_sample_prek_allcities, N_hat = sum(HHWT))

install.packages("svydesign")
library(svydesign)
sample_design <- svydesign(data = filtered_sample_prek_allcities, 
                           strata = ~STRATA, id = ~CLUSTER, nest = TRUE,
                           weights = ~HHWT)