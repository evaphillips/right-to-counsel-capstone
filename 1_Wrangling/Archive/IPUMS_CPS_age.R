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

# sample IDs can be found here: https://cps.ipums.org/cps-action/samples/sample_ids
# variable mnemonics are in the IPUMS CPS extract building interface https://cps.ipums.org/cps-action/variables/group 

# API Call ---------------------------------------------------------------------

set_ipums_api_key("59cba10d8a5da536fc06b59d3c1220c246484f59b68f143fd663a3aa", save = TRUE)

# create a new extract object
# other things I might want to add: whether respondent lives in public housing or receives a housing subsidy
MarchBMS_extract <- define_extract_cps(
  "Capstone Data Pull 12.14.23",
  c("cps2009_03b","cps2010_03b","cps2011_03b", "cps2012_03b", "cps2013_03b", "cps2014_03b", "cps2015_03b", "cps2016_03b", 
    "cps2017_03b", "cps2018_03b", "cps2019_03b", "cps2020_03b"),
  c(
    # Geographic Identifiers and Household IDs
    "STATEFIP",#State FIPS code
    "COUNTY", #FIPS county code
    "MARBASECIDH", #Unique identifier for linking March Basic to ASEC
    "HRHHID", #Household ID, part 1
    "HRHHID2", #Household ID, part 2
    
    # Linking
    "MOMLOC",#Person number of first mother
    "MOMRULE",#Rule for linking first mother
    "SPLOC", #Person number of spouse 
    "SPRULE", #Rule for linking spouse
    
    # Household structure
    "NCHILD", #Number of own children in household
    "NCHLT5", #Number of own children under age 5 in hh
    "FAMUNIT", #Family unit membership
    "YNGCH", #Age of youngest own child in household
    "NSIBS", #Number of own siblings in household
    "RELATE", #Relationship to household head
    "FAMINC", #Family income of householder
    
    # Person characteristics (I think these are March BMS, can maybe find ones in ASEC)
    "AGE", 
    "SEX",
    "RACE",
    "HISPAN", #Hispanic origin
    "MARST", #Marital status
    "EMPSTAT") #Employment status, from BMS
)

ASEC_extract <- define_extract_cps(
  "Capstone Data Pull 12.14.23",
  c("cps2009_03s","cps2010_03s","cps2011_03s", "cps2012_03s", "cps2013_03s", "cps2014_03s", "cps2015_03s", "cps2016_03s", 
    "cps2017_03s", "cps2018_03s", "cps2019_03s", "cps2020_03s"),
  c(
    # Geographic Identifiers and Household IDs
    "ASECOVERH", #Identifier for ASEC oversample - Household
    
    # Economic variables
    "FTOTVAL", #Total family income (from ASEC)
    "OFFPOV", #Official Poverty Status
    "OWNERSHP", #Ownership of dwelling
    "HHINCOME", #Total household income
    "RENTSUB", #Paying lower rent due to government subsidy
    "PUBHOUS", #Living in public housing
    
    # Migration variables 
    "MIGRATE1", #Migration status, 1 year
    "WHYMOVE") #Reason for move

)

#submit the extract to IPUMS CPS
MarchBMS_09_20 <- MarchBMS_extract %>%
   submit_extract() %>%
   wait_for_extract() %>%
   download_extract() %>%
   read_ipums_micro()

ASEC_09_20 <- ASEC_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() %>%
  read_ipums_micro()


# JOIN DATA --------------------------------------------------------------------

# Not everyone in the ASEC will have a match in the MBS, but everyone in the MBS should have a match in the ASEC sample. 
# The best way to merge these two files is using CPSIDP instead of PERNUM. 
# Individuals in the ASEC that do not match to the March Basic will have CPSIDP=0. These individuals are in the ASEC oversample.
# To merge a March Basic file with the corresponding ASEC file at the person level, you should use YEAR, MONTH, and CPSIDP


#filter for ASEC where CPSIDP != 0

ASEC_09_20_filter <- ASEC_09_20 %>% filter(CPSIDP != 0)
ASEC_MBMS_merge <- merge(ASEC_09_20_filter, data, by=c('YEAR', 'MONTH', 'CPSIDP'))


# FILTER DATA ------------------------------------------------------------------


ASEC_MBMS_merge_NYC <- ASEC_MBMS_merge %>% 
  filter(STATEFIP == 36) %>% 
  filter(COUNTY %in% c(36047,36061, 36081, 36085, 36005))


# filter for renters
# RELATE: 0101 = head of household, 0301 = child, 0303 = step child
# need to make a flag for heads of households who have children between the ages of 5-10

filtered_sample <- ASEC_MBMS_merge_NYC %>%
  mutate(fam_group = case_when(NCHLT5 > 0 ~ "1",
                               YNGCH > 5 & YNGCH <= 11 ~ "2",
                               YNGCH > 10 & YNGCH <= 18 ~ "3",
                               YNGCH == 5 ~ "4")) %>%
  mutate(moved = case_when(MIGRATE1 > 1 & MIGRATE1 < 9 ~ 1),
         forced_move = case_when(WHYMOVE == 19 ~ 1))%>%
  filter(RELATE== 101, #head of household
         YNGCH <= 18)  #youngest child is 18 or younger
#OWNERSHP == 22, #with cash rent 


summary <- filtered_sample %>%
  group_by(YEAR) %>%
  summarise(Total = n(),
            Children_5 = length(SERIAL.x[fam_group == 1]),
            Chilren_6_11 = length(SERIAL.x[fam_group == 2]),
            Children_11_18 = length(SERIAL.x[fam_group == 3]),
            renter = length(SERIAL.x[OWNERSHP == 22]),
            sub_hous = length(SERIAL.x[RENTSUB == 2]),
            moved = length(SERIAL.x[moved==1]),
            forced_move = sum(forced_move, na.rm=TRUE),
            housing_move = length(SERIAL.x[WHYMOVE == 12 | WHYMOVE==13 | WHYMOVE==11]))


write.csv(summary, "/Users/evaphillips/Documents/GitHub/universal-pre-k/1_Wrangling/1_Data_Review/CPS_summary_year.csv")

print(summary, n=48)

# for 2012 and 2016 
# with children < 5
# with children age 6-11
# with children 11 - 18
# renter
# subsidized 
# moved
# forced move (19)
# cheaper housing or other housing reason (12, 13)
# better neighborhood (11)
# n sample size


# TRENDS DATA ------------------------------------------------------------------  


summary_CPS_trend <- filtered_sample %>%
  filter(fam_group == 1 | fam_group ==2) %>%
  group_by(YEAR, fam_group) %>%
  summarise(Total = n(),
            moved = length(SERIAL.x[MIGRATE1 ==2 | MIGRATE1 == 3 | MIGRATE1 == 4]),
            move_rate = moved/Total *100)

summary_CPS_plot <- summary_CPS_trend %>% 
  filter(YEAR != 2020) %>%
  mutate(fam_group = case_when(fam_group == 1 ~ "Households with children under age 5",
                               fam_group == 2 ~ "Households with children aged 6-11"),
         flag = case_when(YEAR <2014 ~ 'Pre',
                          YEAR >= 2014 ~"Post")) 

summary_CPS_plot$fam_group <- as.factor(summary_CPS_plot$fam_group)
cps_mod <- lm(move_rate ~ YEAR + fam_group + flag + YEAR:fam_group + YEAR:flag + YEAR:fam_group:flag, 
          data = as.data.frame(summary_CPS_plot))
summary(cps_mod)

cps_pdata <- data.frame(YEAR = rep(seq(2009, 2019, length.out = 20), times = 2),
                    fam_group = rep(c("Households with children under age 5", "Households with children aged 6-11"), each = 20))

cps_pdata$flag <- "Pre"
cps_pdata[cps_pdata$YEAR >= 2014, "flag"] <- "Post"
cps_pdata <- rbind(cps_pdata, data.frame(YEAR = rep(2014, 4), 
                                         fam_group = rep(c("Households with children under age 5", "Households with children aged 6-11"), each = 2),
                                         flag = rep(c("Pre", "Post"), times = 2)))

cps_pdata$move_rate <- predict.lm(cps_mod, cps_pdata, se.fit = TRUE)$fit
cps_pdata

ggplot(data = cps_pdata, aes(x = YEAR, y = move_rate, color = fam_group, linetype = flag)) + 
  geom_vline(xintercept = 2014) +
  geom_line(size = 1) + 
  geom_point(data = summary_CPS_plot, size = 2.5) +
  labs(title = "Mobility Rates by Year, CPS ASEC Survey") + 
  scale_x_continuous(breaks = 2009:2019, expand = c(0,0)) +
  theme_bw()



  ggplot(aes(x = YEAR,
             y = move_rate)) +
  geom_line(aes(color = fam_group)) +
  labs(title = "Mobility Rates by Year, CPS") +
  geom_smooth(aes(color= fam_group), method="lm", formula = ) + 
  geom_vline(xintercept = 2014)

  
                    