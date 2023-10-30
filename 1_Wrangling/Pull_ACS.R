# Header -----------------------------------------------------------------------

# PROJECT: Pull ACS Indicators
# AUTHOR: Eva Phillips
# DATE: 2023-08-29

# PURPOSE: Sample script to pull ACS data using the tidycensus API
# Read more about the tidycensus package here:
# https://walker-data.com/tidycensus/ and  https://csde-uw.github.io/tidycensus-tutorial/ 

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(grid)
library(gridExtra)
library(readxl)

# EXAMPLE 1  ------------------------------------------------------------------------


# Census API Key, you need to create your own and paste here
census_api_key = "c32dfbf7d25fe9558fd11bf021780457970f96ff"

# Pull list of census codes
v21 <- load_variables(2021, "acs5", cache = TRUE)
write_csv(v21, "v21.csv") #use this to identify the codes for variables of interest

#Create list of geographies of interest
state_list = c("NE", "NY")

ACS_2021 <- get_acs(
  geography = "tract",
  state = state_list,
  year = 2021,
  survey = "acs5",
  output = "wide",
  geometry = FALSE,
  moe_sum = FALSE,
  key = census_api_key,
  variables = c(
    pop_num = "B01003_001E", 
    pop_race_asian = "B03002_006E",
    pop_race_black = "B03002_004E", 
    pop_race_hisp = "B03002_012E",
    pop_race_white = "B03002_003E", # 
    pop_povstatusdetermined = "B17020_001E", # Total pop from poverty level universe
    pop_belowpov = "B17020_002E", # Income in the past 12 months below poverty level
    med_gross_rent = "B25031_001E" 
    
  ) 
) 

ACS_2021 <- ACS_2021 %>% mutate('GEOID' = as.numeric(GEOID))


# EXAMPLE 2 ------------------------------------------------------------------------

city_list <- c(
  "Olympia, WA" = "53067", # using thurston county data since we lack CSA/place data
  "Tacoma, WA" = "5370000",
  "Los Angeles, CA" = "0644000",  
  "San Francisco, CA" = "06075", # using SF county data since it is equivalent to the place geo
  "Philadelphia, PA" = "4260000",
  "Boston, MA" = "2507000"
)

years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022) 

variable_list <- load_variables(2022, "acs1")

acs_indics_place <- map_dfr(
  c(2010:2019, 2021, 2022),
  function(year) {
    get_acs(
      geography = "place",
      variables = c("rent_gross_med" = "B25064_001",
                    "rent_contract_med" = "B25058_001",
                    "q75_contract" = "B25059_001",
                    
                    
                    # the following three are moderate
                    "rent_burden_30_35_num" = "B25070_007",
                    "rent_burden_35_40_num" = "B25070_008",
                    "rent_burden_40_50_num" = "B25070_009",
                    
                    "rent_burden_sev_num" = "B25070_010",
                    
                    # the denominator should be (total - to_subtract)
                    "rent_burden_universe_total" = "B25070_001",
                    "rent_burden_universe_to_subtract" = "B25070_011",
                    
                    "renter_hh_inc" = "B25119_003", # appears to already have been adjusted for 2022
                    
                    "pop_asian_num" = "C03002_006",
                    "pop_black_num" = "C03002_004",
                    "pop_hisp_num" = "C03002_012",
                    "pop_white_num" = "C03002_003",
                    
                    "pop_num" = "B01001_001",
                    
                    "units_occ_rent_num" = "B25003_003",
                    "units_vac_rent_num" = "B25004_002",
                    "units_occ_num" = "B25002_002"),
      year = year,
      survey = "acs1"
    ) %>%
      filter(GEOID %in% city_list) %>%
      mutate(year = year)
  }
) %>% 
  dplyr::select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(vacancy_rate = ((units_vac_rent_num / (units_occ_rent_num + units_vac_rent_num))*100),
         renter_occupied = ((units_occ_rent_num / (units_occ_num))*100))



