# ------------------------------------------------------------------------------

# PROJECT: Universal Prek
# AUTHOR: Eva Phillips
# DATE: 2023-12-14

# PURPOSE: Plot Parallel Trends

# Setup ------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(ipumsr)
library(did)

# TRENDS DATA ------------------------------------------------------------------  

sample <- read.csv("/Users/evaphillips/Documents/GitHub/universal-pre-k/0_Data/filtered_sample_prek_allcities.csv")%>%
  filter(YEAR >= 2009) %>%
  mutate(moved = case_when(MIGRATE1 ==2 ~ 1,
                           MIGRATE1 == 2 ~ 1,
                           MIGRATE1 == 4 ~ 1,
                           .default = 0),
         moved_HHWT = moved*HHWT) 

#means are the same as the proportion moved, since our variable is binary
sample_summary <- sample %>%
  group_by(YEAR, city) %>%
  summarise(total = sum(HHWT),
            total_moved = sum(moved_HHWT),
            move_rate = round(total_moved/total *100, 2)) %>%
  mutate(treat = case_when(YEAR < 2014 ~ 0,
                           YEAR >= 2014 & city == 'NYC' ~ 1, 
                           YEAR >= 2014 & city != 'NYC' ~ 0)) %>%
  mutate(City = case_when(city == 'NYC' ~ 'New York City, NY', 
                          city == 'SD'~ 'San Diego, CA', 
                          .default = city))

#create plots
cities <-unique(sample_summary$City)


for (i in cities) {
  nyc_city <- sample_summary %>% filter(City == 'New York City, NY' | City == i) %>% filter(YEAR<2020)
  plot <- 
    ggplot(nyc_city, aes(YEAR, y = move_rate, color = City)) +
    geom_line(size = .75, linetype = 2) + 
    geom_vline(xintercept = 2014) +
    geom_point(data = nyc_city, size = 2.5, alpha = 0.4) +
    geom_smooth(data=dplyr::filter(nyc_city, YEAR<2015), method = 'lm',se = FALSE)+
    scale_x_continuous(breaks = seq(2009, 2019, 1)) +
    labs(title = "Mobility Rates of Families with PreK-Aged Children") + 
    ylab("Percent Moved")+
    xlab("Year") +
    theme_bw()
  print(plot)
}

# 
# cities <-unique(sample_summary$city)
# 
# 
# for (i in cities) {
#   nyc_city <- sample_summary %>% filter(city == 'NYC' | city == i) %>% filter(YEAR<2020)
#   plot <- 
#     ggplot(nyc_city, aes(YEAR, y = move_rate, color = city)) +
#     geom_line(size = .75, linetype = 2) + 
#     geom_vline(xintercept = 2014) +
#     geom_point(data = nyc_city, size = 2.5, alpha = 0.4) +
#     geom_smooth(data=dplyr::filter(nyc_city, YEAR<2015), method = 'lm',se = FALSE)+
#     scale_x_continuous(breaks = seq(2009, 2019, 1)) +
#     labs(title = "Mobility Rates of Families with PreK-Aged Children") + 
#     ylab("Percent Moved")+
#     xlab("Year") +
#     theme_bw()
#   print(plot)
# }



# test pre-trends

sample_clean <- sample %>% 
  select('SERIAL', 'YEAR', 'city', 'moved', 'moved_HHWT', 'HHWT', 'PUMA') %>%
  mutate(treat = case_when(YEAR < 2014 ~ 0,
                           YEAR >= 2014 & city == 'NYC' ~ 1, 
                           YEAR >= 2014 & city != 'NYC' ~ 0)) %>%
  mutate(first_treat = case_when(treat == 1 ~ 2014,
                                .default = 0)) 

test <- sample_clean %>% filter(city == 'NYC' | city == 'LA') %>%
  mutate(id = case_when(city=='NYC' ~ 1,
                        city == 'LA' ~2))

test_puma <- test %>%
  group_by(YEAR, city, PUMA) %>%
  summarise(total = sum(HHWT),
            total_moved = sum(moved_HHWT),
            move_rate = round(total_moved/total *100, 2)) %>%
  mutate(treat = case_when(YEAR < 2014 ~ 0,
                           YEAR >= 2014 & city == 'NYC' ~ 1, 
                           YEAR >= 2014 & city != 'NYC' ~ 0)) %>%
  mutate(id = case_when(city=='NYC' ~ 1,
                        city == 'LA' ~2)) %>%
  mutate(treat = case_when(YEAR < 2014 ~ 0,
                           YEAR >= 2014 & city == 'NYC' ~ 1, 
                           YEAR >= 2014 & city != 'NYC' ~ 0)) %>%
  mutate(first_treat = case_when(treat == 1 ~ 2014,
                                 .default = 0))

did_att_gt <- att_gt(yname = "move_rate",       #column with outcome variable
                     tname = "YEAR",        #column with time period
                     idname = "id",     #the individual (cross-sectional unit) id name
                     gname = "first_treat",  #variable that indicated when an observation is treated, 0 for units in the untreated group.
                     data = test_puma,           #The name of the data.frame that contains the data
                     panel = FALSE,
                     allow_unbalanced_panel = TRUE,
                     bstrap = FALSE,    #whether to compute standard errors
                     cband = FALSE)    #whether to compute confidence intervals
summary(did_att_gt)

data(mpdta)

out1 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=NULL,
               data=mpdta)
summary(out1)
