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

# see this webinar: https://www.youtube.com/watch?v=OT6upQ1dBgU 
# and https://v1.developer.ipums.org/docs/workflows/create_extracts/cps/ 

# sample IDs can be found here: https://usa.ipums.org/usa-action/samples/sample_ids
# variable mnemonics are in the IPUMS CPS extract building interface https://cps.ipums.org/cps-action/variables/group 

# API Call ---------------------------------------------------------------------

set_ipums_api_key("59cba10d8a5da536fc06b59d3c1220c246484f59b68f143fd663a3aa", save = TRUE)

# create a new extract object
# c("us2009a","us2009e", "us2010a","us2010e", "us2011a","us2011e", "us2012a","us2012e","us2013a","us2013e",
#   "us2014a","us2014c","us2015a","us2015c","us2016a","us2016c", "us2017a","us2017c", "us2018a","us2018c",
#   "us2019a","us2019c", "us2020a","us2020c"),

ACS_extract_1yr <- define_extract_usa(
  "Capstone Data Pull 12.17.23 IPUMS USA 1-year ACS",
  c("us2009a", "us2010a", "us2011a", "us2012a","us2013a","us2014a","us2015a","us2016a", "us2017a", "us2018a","us2019a", "us2020a"),
  c(
    # Geographic Identifiers and Household IDs
    "STATEFIP",#State FIPS code
    "COUNTYFIP", #FIPS county code
    "DENSITY", #population weighted density of PUMA
    "PUMA", # Public Use Microdata Area

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

    # Demographics
    "AGE", 
    "SEX",
    "RACE",
    "HISPAN", #Hispanic origin
    "MARST", #Marital status
    "EMPSTAT", #employment status
    
    # Migration variables 
    "MIGRATE1") #Migration status, 1 year
)


  
#submit the extract to IPUMS USA
ACS_extract_09_20_1yr <- ACS_extract_1yr %>%
   submit_extract() %>%
   wait_for_extract() %>%
   download_extract() %>%
   read_ipums_micro()


# FILTER DATA ------------------------------------------------------------------

ACS_1_year_NYC <- ACS_extract_09_20_1yr %>% 
  filter(STATEFIP == 36) %>% 
  filter(COUNTYFIP %in% c(5,61, 81, 85, 47))
  

# filter for renters
# RELATE: 0101 = head of household, 0301 = child, 0303 = step child
# need to make a flag for heads of households who have children between the ages of 5-10

filtered_sample_ACS_1yr <- ACS_1_year_NYC %>%
  mutate(fam_group = case_when(NCHLT5 > 0 ~ "1",
                               YNGCH > 5 & YNGCH <= 11 ~ "2",
                               YNGCH > 10 & YNGCH <= 18 ~ "3",
                               YNGCH == 5 ~ "4")) %>%
  filter(RELATE== 1,
         YNGCH <= 18) #head of household


summary_acs_1yr <- filtered_sample_ACS_1yr %>%
  group_by(YEAR) %>%
  summarise(Total = n(),
            Children_5 = length(SERIAL[fam_group == 1]),
            Chilren_6_11 = length(SERIAL[fam_group == 2]),
            Children_11_18 = length(SERIAL[fam_group == 3]),
            renter = length(SERIAL[OWNERSHP == 2]),
            moved_within_statw = length(SERIAL[MIGRATE1 ==2]),
            moved_outside_state = length(SERIAL[MIGRATE1 == 3]))

write.csv(summary_acs_1yr, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_year.csv")

summary_acs_1yr_2012 <- filtered_sample_ACS_1yr %>%
  filter(fam_group == 1 | fam_group ==2) %>%
  filter(YEAR == 2012) %>%
  group_by(fam_group) %>%
  summarise(Total = n(),
            PCT_renter = length(SERIAL[OWNERSHP == 2])/Total *100,
            moved = length(SERIAL[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
            move_rate = moved/Total *100)

write.csv(summary_acs_1yr_2012, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_2012.csv")

summary_acs_1yr_2016 <- filtered_sample_ACS_1yr %>%
  filter(fam_group == 1 | fam_group ==2) %>%
  filter(YEAR == 2016) %>%
  group_by(fam_group) %>%
  summarise(Total = n(),
            PCT_renter = length(SERIAL[OWNERSHP == 2])/Total *100,
            moved = length(SERIAL[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
            move_rate = moved/Total *100)


write.csv(summary_acs_1yr_2016, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/ACS_summary_2016.csv")


# TRENDS DATA ------------------------------------------------------------------  


summary_acs_1_yr_trend <- filtered_sample_ACS_1yr %>%
  filter(YEAR != 2020) %>%
  filter(fam_group == 1 | fam_group ==2) %>%
  group_by(YEAR, fam_group) %>%
  summarise(Total = n(),
            moved = length(SERIAL[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
            move_rate = moved/Total *100) %>%
  mutate(fam_group = case_when(fam_group == 1 ~ "Households with children under age 5",
                               fam_group == 2 ~ "Households with children aged 6-11"),
         flag = case_when(YEAR <2014 ~ 'Pre',
                          YEAR >= 2014 ~"Post")) 

#FIT MODEL -- fit a linear model for the dif-in-dif relationship
mod <- lm(move_rate ~ 
            YEAR + #slope for year
            fam_group +  #intercept for fam_group
            flag +  #intercept for pre/post
            YEAR:fam_group + #interaction between family_group x year
            YEAR:flag + #interaction between year x pre/post (allows slope change)
            YEAR:fam_group:flag, #interaction between year x family_group x flag
          data = as.data.frame(summary_acs_1_yr_trend)) #specify data
summary(mod) 

#VISUALIZATION -- create fake data for prediction and plotting
acs_pdata <- data.frame(YEAR = rep(seq(2009, 2019, length.out = 20), times = 2),
                    fam_group = rep(c("Households with children under age 5", "Households with children aged 6-11"), each = 20))
acs_pdata$flag <- "Pre"
acs_pdata[acs_pdata$YEAR >= 2014, "flag"] <- "Post"
acs_pdata <- rbind(acs_pdata, data.frame(YEAR = rep(2014, 4), 
                                         fam_group = rep(c("Households with children under age 5", "Households with children aged 6-11"), each = 2),
                                         flag = rep(c("Pre", "Post"), times = 2)))

#use linear model to predict move rate from the fake data
acs_pdata$move_rate <- predict.lm(mod, acs_pdata, se.fit = TRUE)$fit
acs_pdata

ggplot(data = acs_pdata, aes(x = YEAR, y = move_rate, color = fam_group, linetype = flag)) + 
  geom_vline(xintercept = 2014) +
  geom_line(size = 1) + 
  geom_point(data = summary_acs_1_yr_trend, size = 2.5) +
  labs(title = "Mobility Rates by Year, ACS 1-Year Estimates") + 
  scale_x_continuous(breaks = 2009:2019, expand = c(0,0)) +
  theme_bw()


  
  
  
                    