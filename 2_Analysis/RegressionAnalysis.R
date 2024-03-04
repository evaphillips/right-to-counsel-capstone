# ------------------------------------------------------------------------------

# PROJECT: Universal Prek
# AUTHOR: Eva Phillips
# DATE: 2023-12-14

# PURPOSE: Run Regression

# Setup ------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(ipumsr)


# TRENDS DATA ------------------------------------------------------------------  

filtered_sample_prek_all <- read.csv("/Users/evaphillips/Documents/GitHub/universal-pre-k/0_Data/filtered_sample_prek_allcities.csv")

summary_acs_1_yr_trend <- filtered_sample_prek_all %>%
  filter(YEAR >= 2009) %>%
  #  filter(city == 'NYC' | city == 'LA') %>%
  group_by(YEAR, city, PUMA) %>%
  summarise(Total = sum(HHWT),
            moved = sum(HHWT[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
            move_rate = moved/Total *100) %>%
  mutate(flag = case_when(YEAR <2014 ~ 'Pre',
                          YEAR >= 2014 ~"Post")) 


#FIT MODEL -- fit a linear model for the dif-in-dif relationship

fit_model <- function(city_list, summary_data){
  
  summary_data <- summary_data[summary_data$city %in% city_list, ]
  summary_data$city <- factor(summary_data$city, levels = city_list)
  mod <- lm(move_rate ~ 
              YEAR + #slope for year
              city +  #intercept for city
              flag +  #intercept for pre/post
              YEAR:city + #interaction between family_group x year
              YEAR:flag + #interaction between year x pre/post (allows slope change)
              YEAR:city:flag, #interaction between year x family_group x flag
            data = as.data.frame(summary_data)) #specify data
  summary(mod) 
  
  #VISUALIZATION -- create fake data for prediction and plotting
  acs_pdata <- data.frame(YEAR = rep(seq(2009, 2019, length.out = 20), times = 2),
                          city = rep(city_list, each = 20))
  acs_pdata$flag <- "Pre"
  acs_pdata[acs_pdata$YEAR >= 2014, "flag"] <- "Post"
  acs_pdata <- rbind(acs_pdata, data.frame(YEAR = rep(2014, 4), 
                                           city = rep(city_list, each = 2),
                                           flag = rep(c("Pre", "Post"), times = 2)))
  
  #use linear model to predict move rate from the fake data
  acs_pdata$move_rate <- predict.lm(mod, acs_pdata, se.fit = TRUE)$fit
  acs_pdata
  
  ggplot(data = acs_pdata, aes(x = YEAR, y = move_rate, color = city, linetype = flag)) + 
    geom_vline(xintercept = 2014) +
    geom_line(size = 1) + 
    geom_point(data = summary_data, size = 2.5) +
    labs(title = "Mobility Rates by Year, ACS 1-Year Estimates") + 
    scale_x_continuous(breaks = 2009:2019, expand = c(0,0)) +
    theme_bw()
  
}

fit_model(c('PHX', 'NYC'), summary_acs_1_yr_trend)
fit_model(c('LA', 'NYC'), summary_acs_1_yr_trend)

#next step is to test whether the difference in the slope is statistically significant 
filtered_sample_prek_all <- read.csv("filtered_sample_prek_only.csv")
filtered_sample_prek_all$moved <- 0
filtered_sample_prek_all




-----------------
  
  
  
  
  city_list <- c('PHX', 'NYC')
summary_data <- summary_acs_1_yr_trend
summary_data$YEAR <- summary_data$YEAR - 2014 #to fix the intercepts at 2014
summary_data$flag_2 <- "Pre"  #flag only for NYC, used to fix intercept for comparison city
summary_data[summary_data$city == "NYC" & summary_data$YEAR >= 0, "flag_2"] <- "Post"
summary_data$flag <- factor(summary_data$flag, levels = c("Pre", "Post"))
summary_data$flag_2 <- factor(summary_data$flag_2, levels = c("Pre", "Post"))

#fit the model
summary_data <- summary_data[summary_data$city %in% city_list, ]
summary_data$city <- factor(summary_data$city, levels = city_list)
mod <- lm(move_rate ~ 
            YEAR + #slope for year
            city +  #intercept for city
            flag_2 + 
            YEAR:city + #interaction between family_group x year
            YEAR:flag:city, #interaction between year x pre/post (allows slope change)
          data = as.data.frame(summary_data)) #specify data
summary(mod) 

#VISUALIZATION -- create fake data for prediction and plotting
acs_pdata <- data.frame(YEAR = rep(seq(-5, 5, length.out = 20), times = 2),
                        city = rep(city_list, each = 20))
acs_pdata$flag <- "Pre"
acs_pdata[acs_pdata$YEAR >= 0, "flag"] <- "Post"
acs_pdata$flag_2 <- "Pre"
acs_pdata[acs_pdata$city == "NYC" & acs_pdata$YEAR >= 0, "flag_2"] <- "Post"
acs_pdata <- rbind(acs_pdata, data.frame(YEAR = rep(0, 4), 
                                         city = rep(city_list, each = 2),
                                         flag = rep(c("Pre", "Post"), times = 2),
                                         flag_2 = c("Pre", "Pre", "Pre", "Post")))

#use linear model to predict move rate from the fake data
acs_pdata$move_rate <- predict.lm(mod, acs_pdata, se.fit = TRUE)$fit
acs_pdata

ggplot(data = acs_pdata, aes(x = YEAR, y = move_rate, color = city, linetype = flag)) + 
  geom_vline(xintercept = 0) +
  geom_line(size = 1) + 
  geom_point(data = summary_data, size = 2.5, alpha = 0.4) +
  labs(title = "Mobility Rates by Year, ACS 1-Year Estimates") + 
  scale_x_continuous(breaks = -5:5, expand = c(0,0)) +
  theme_bw()



