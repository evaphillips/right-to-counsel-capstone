# ------------------------------------------------------------------------------

# PROJECT: IPUMS CPS API
# AUTHOR: Eva Phillips
# DATE: 2023-12-14

# PURPOSE: Pull CPS Data

# Setup ------------------------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot")
# install.packages("stringr")
# install.packages("purrr")
# install.packages("DR")
# install.packages("sf")
# install.packages("ipumsr")

library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(ipumsr)

# Documentation ----------------------------------------------------------------

# sample IDs can be found here: https://usa.ipums.org/usa-action/samples/sample_ids
# variable mnemonics are in the IPUMS CPS extract building interface https://usa.ipums.org/usa-action/variables/group

# API Call ---------------------------------------------------------------------

set_ipums_api_key("59cba10d8a5da536fc06b59d3c1220c246484f59b68f143fd663a3aa", save = TRUE)

# create a new extract object
# c("us2005a","us2006a","us2007a","us2009a","us2009e","us2009a","us2009e","us2009a","us2009e", "us2010a","us2010e", "us2011a","us2011e", "us2012a","us2012e","us2013a","us2013e",
#   "us2014a","us2014c","us2015a","us2015c","us2016a","us2016c", "us2017a","us2017c", "us2018a","us2018c",
#   "us2019a","us2019c", "us2020a","us2020c"),

ACS_extract_1y_05_15 <- define_extract_usa(
  "Capstone Data Pull 2.12.24 IPUMS USA 1-year ACS - DC",
  c("us2005a","us2006a","us2007a", "us2008a", "us2009a","us2010a", "us2011a", "us2012a","us2013a","us2014a","us2015a","us2016a"),
  c(
    # Geographic Identifiers and Household IDs
    "STATEFIP",#State FIPS code
    "COUNTYFIP", #FIPS county code
    "DENSITY", #population weighted density of PUMA
    "PUMA", # Public Use Microdata Area
    
    # Weights
    "HHWT", #household weight
    "PERWT",

    # Family relationships/structure
    "MOMLOC",#Person number of first mother (P)
    "MOMRULE",#Rule for linking first mother (P)
    "SPLOC", #Person number of spouse (P)
    "SPRULE", #Rule for linking spouse (P)
    "NCHILD", #Number of own children in household (P)
    "NCHLT5", #Number of own children under age 5 in hh (P)
    "FAMUNIT", #Family unit membership (P)
    "YNGCH", #Age of youngest own child in household (P)
    "ELDCH", #Age of the eldest own child in household (P)
    "RELATE", #Relationship to household head (P)
    
    # Economic variables
    "HHINCOME", #total household income (H)
    "POVERTY", #Poverty status (P)
    "OWNERSHP", #Ownership of dwelling (H)
    "FTOTINC", #total family income (P)
   # "PUBHOUS", #public housing -- only in CPS
   # "RENTSUB", #government rental subsidy -- only in CPS
    "MULTGEN",

    # Demographics
    "AGE", 
    "SEX",
    "RACE",
    "EDUC", #educational attainment
    "HISPAN", #Hispanic origin
    "MARST", #Marital status
    "EMPSTAT", #employment status
    
    # Migration variables 
    "MIGRATE1") #Migration status, 1 year
)


  
#submit the extract to IPUMS USA
ACS_extract_1y_05_15 <- ACS_extract_1y_05_15 %>%
   submit_extract() %>%
   wait_for_extract() %>%
   download_extract() %>%
   read_ipums_micro()


# CITY FILTER ------------------------------------------------------------------

ACS_1_year_cities_DC <- ACS_extract_1y_05_15 %>%
  mutate(city = case_when(
    STATEFIP == 36 & COUNTYFIP %in% c(5,61, 81, 85, 47) ~ "NYC",
    STATEFIP == 11 ~ "DC",
    STATEFIP == 24 & COUNTYFIP == 005 ~ "BAL", 
    STATEFIP == 48 & COUNTYFIP == 201 ~ "HOU",
    STATEFIP == 17 & PUMA %in% c(3501, 3502, 3503, 3504, 3505, 3506, 3507, 3508, 
                                  3509,3510, 3511, 3512, 3513, 3514, 3514, 3516, 
                                 3517, 3518, 3519) ~ "CHI",
    STATEFIP == 42 & COUNTYFIP == 101 ~ "PHI",
    STATEFIP == 06 & COUNTYFIP == 037 ~ "LA",
    STATEFIP == 06 & COUNTYFIP == 073 ~ "SD",
    STATEFIP == 04 & PUMA %in% c(0112, 0113, 0114, 0115, 0116, 0117, 0118, 0119, 
                                  0120,0121,0122, 0123, 0125, 0128, 0129) ~ "PHX"))




#SAMPLE FILTERS ------------------------

# sample filters:
## heads of households
## renters (look at 0s and negatives)
## remove households receiving housing subsidy
## try two ways:
#  - households with only UPK eligible children
#  - households with children under 18 (min 1 UPK eligible)
  
# RELATE: 0101 = head of household, 0301 = child, 0303 = step child

#SAMPLE 1 -- pre-k age children only
filtered_sample_prek_only_DC <- ACS_1_year_cities_DC %>%
  filter(!is.na(city), # Filter only for our cities
         RELATE == 1, # Head of household
         OWNERSHP == 2,# Renters
         NCHLT5 > 0, # Number of own children under age 5 is > 1
         ELDCH < 5)  # Oldest child is under 5

#write.csv('filtered_sample_prek_only.csv')

#SAMPLE 2 -- pre-k age children, other children under 18 years
filtered_sample_prek_18_DC <- ACS_1_year_cities_DC %>%
  filter(!is.na(city), # Filter only for our cities
         RELATE== 1, # Head of household
         OWNERSHP == 2, # Renters
         NCHLT5 > 0, # Number of own children under age 5 is > 1
         ELDCH < 19)  # Oldest child is 18 or under

#write.csv('filtered_sample_prek_18.csv')

#SAMPLE 3 -- pre-k age children all household
filtered_sample_prek_all_DC <- ACS_1_year_cities_DC %>%
  filter(!is.na(city), # Filter only for our cities
         RELATE== 1, # Head of household
         OWNERSHP == 2, # Renters
         NCHLT5 > 0) # Number of own children under age 5 is > 1

#write.csv(filtered_sample_prek_all, 'filtered_sample_prek_18.csv')

# 
# summary_acs_1yr <- filtered_sample_ACS_1yr %>%
#   group_by(YEAR) %>%
#   summarise(Total = sum(HHWT),
#             Children_5 = sum(HHWT[fam_group == 1]),
#             Chilren_6_11 = sum(HHWT[fam_group == 2]),
#             Children_11_18 = sum(HHWT[fam_group == 3]),
#             renter = sum(HHWT[OWNERSHP == 2]),
#             moved_within_state = sum(HHWT[MIGRATE1 ==2]),
#             moved_outside_state = sum(HHWT[MIGRATE1 == 3]))
# 
# write.csv(summary_acs_1yr, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_year_wgt.csv")
# 
# summary_acs_1yr_2012 <- filtered_sample_ACS_1yr %>%
#   filter(fam_group == 1 | fam_group ==2) %>%
#   filter(YEAR == 2012) %>%
#   group_by(fam_group) %>%
#   summarise(Total = sum(HHWT),
#             PCT_renter = sum(HHWT[OWNERSHP == 2])/Total *100,
#             moved = sum(HHWT[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
#             move_rate = moved/Total *100)
# 
# write.csv(summary_acs_1yr_2012, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_2012_wgt.csv")
# 
# summary_acs_1yr_2016 <- filtered_sample_ACS_1yr %>%
#   filter(fam_group == 1 | fam_group ==2) %>%
#   filter(YEAR == 2016) %>%
#   group_by(fam_group) %>%
#   summarise(Total = sum(HHWT),
#             PCT_renter = sum(HHWT[OWNERSHP == 2])/Total *100,
#             moved = sum(HHWT[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
#             move_rate = moved/Total *100)
# 
# 
# write.csv(summary_acs_1yr_2016, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_2016_wgt.csv")


# TRENDS DATA ------------------------------------------------------------------  

 summary_acs_1_yr_trend <- filtered_sample_prek_all_DC %>%
   group_by(YEAR, city, PUMA) %>%
   summarise(Total = sum(HHWT),
             moved = sum(HHWT[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
             move_rate = moved/Total *100) %>%
   mutate(flag = case_when(YEAR <2008 ~ 'Pre',
                           YEAR >= 2008 ~"Post")) 


city_list <- c('LA', 'DC')
summary_data <- summary_acs_1_yr_trend
summary_data$YEAR <- summary_data$YEAR - 2008 #to fix the intercepts at 2014
summary_data$flag_2 <- "Pre"  #flag only for NYC, used to fix intercept for comparison city
summary_data[summary_data$city == "DC" & summary_data$YEAR >= 0, "flag_2"] <- "Post"
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






# 
# summary_acs_1_yr_trend <- filtered_sample_prek_all %>%
#   group_by(YEAR, city) %>%
#   summarise(Total = sum(HHWT),
#             moved = sum(HHWT[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
#             move_rate = moved/Total *100) %>%
#   mutate(flag = case_when(YEAR <2008 ~ 'Pre',
#                           YEAR >= 2008 ~"Post")) 
# 
# 
# #FIT MODEL -- fit a linear model for the dif-in-dif relationship
# 
# fit_model <- function(city_list, summary_data){
#   
#   summary_data <- summary_data[summary_data$city %in% city_list, ]
#   summary_data$city <- factor(summary_data$city, levels = city_list)
#   mod <- lm(move_rate ~ 
#               YEAR + #slope for year
#               city +  #intercept for city
#               flag +  #intercept for pre/post
#               YEAR:city + #interaction between city x year
#               YEAR:flag + #interaction between year x pre/post (allows slope change)
#               YEAR:city:flag, #interaction between year x city x flag
#             data = as.data.frame(summary_data)) #specify data
#   print(summary(mod))
#   
#   #VISUALIZATION -- create fake data for prediction and plotting
#   acs_pdata <- data.frame(YEAR = rep(seq(2005, 2016, length.out = 20), times = 2),
#                           city = rep(city_list, each = 20))
#   acs_pdata$flag <- "Pre"
#   acs_pdata[acs_pdata$YEAR >= 2008, "flag"] <- "Post"
#   acs_pdata <- rbind(acs_pdata, data.frame(YEAR = rep(2008, 4), 
#                                            city = rep(city_list, each = 2),
#                                            flag = rep(c("Pre", "Post"), times = 2)))
#   
#   #use linear model to predict move rate from the fake data
#   acs_pdata$move_rate <- predict.lm(mod, acs_pdata, se.fit = TRUE)$fit
#   acs_pdata
#   
#   ggplot(data = acs_pdata, aes(x = YEAR, y = move_rate, color = city, linetype = flag)) + 
#     geom_vline(xintercept = 2008) +
#     geom_line(size = 1) + 
#     geom_point(data = summary_data, size = 2.5) +
#     labs(title = "Mobility Rates by Year, ACS 1-Year Estimates") + 
#     scale_x_continuous(breaks = 2005:2016, expand = c(0,0)) +
#     theme_bw()
#   
# }
# 
# fit_model(c('DC', 'PHX'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'LA'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'BAL'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'CHI'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'PHI'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'SD'), summary_acs_1_yr_trend)
# fit_model(c('DC', 'HOU'), summary_acs_1_yr_trend)
# #next step is to test whether the difference in the slope is statistically significant 
#   
  
                    