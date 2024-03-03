------------------------------------------------------------------------------
  
  # Event study
  
  # Setup ------------------------------------------------------------------------
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("purrr")
#install.packages("sf")
#install.packages("ipumsr")
#install.packages("tidyr")
#install.packages("tibble")

library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(ipumsr)
library(tidyr)

# Documentation ----------------------------------------------------------------

# sample IDs can be found here: https://usa.ipums.org/usa-action/samples/sample_ids
# variable mnemonics are in the IPUMS CPS extract building interface https://usa.ipums.org/usa-action/variables/group

# API Call ---------------------------------------------------------------------

set_ipums_api_key("59cba10d8a5da536fc06b59d5cae6eb507764cabbc0de684e3dc0014", save = TRUE, overwrite = TRUE)

# create a new extract object
# c("us2009a","us2009e", "us2010a","us2010e", "us2011a","us2011e", "us2012a","us2012e","us2013a","us2013e",
#   "us2014a","us2014c","us2015a","us2015c","us2016a","us2016c", "us2017a","us2017c", "us2018a","us2018c",
#   "us2019a","us2019c", "us2020a","us2020c")

ACS_extract_1yr <- define_extract_usa(
  "Capstone Data Pull 12.17.23 IPUMS USA 1-year ACS",
  c("us2009a", "us2010a", "us2011a", "us2012a","us2013a","us2014a","us2015a","us2016a", "us2017a", "us2018a","us2019a", "us2020a"),
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



#submit the extract to IPUMS USA #1022 pm
ACS_extract_09_20_1yr <- ACS_extract_1yr %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() %>%
  read_ipums_micro()

# CITY FILTER ------------------------------------------------------------------

ACS_1_year_cities <- ACS_extract_09_20_1yr %>%
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
## households with at least one pre-k age child

# RELATE: 0101 = head of household, 0301 = child, 0303 = step child


filtered_sample_prek_all <- ACS_1_year_cities %>%
  filter(!is.na(city), # Filter only for our cities
         RELATE == 1, # Head of household
         OWNERSHP == 2, # Renters
         NCHLT5 > 0) # Number of own children under age 5 is > 1

#write.csv(filtered_sample_prek_all, 'filtered_sample_prek_allcities.csv')

#---------------------------------------
#EVENT STUDY
#---------------------------------------
# filter dataset to only NYC and PHI
phi <- subset(filtered_sample_prek_all, city %in% c("NYC", "PHI"))

# create a flag for 1 or 0 if it's NYC
phi$city_ind <- ifelse(phi$city == "NYC", 1, 0)

# create a flag for post-period
phi$post <- ifelse(phi$YEAR >= 2014,1,0)


# make MIGRATE1 binary
phi <- phi %>%
  mutate(migrate_binary = ifelse(MIGRATE1 %in% c(2, 3, 4), 1, 0))

# create a flag for year
prepended_value <- "INX"
phi$Year_INX <- paste0(prepended_value, phi$YEAR)
phi_wide <- pivot_wider(phi, names_from = Year_INX, values_from = Year_INX, values_fn = list(Year_INX = length), values_fill = 0)

# estimate the regression
es <- lm(migrate_binary ~ ., data = select(phi_wide
                                           , migrate_binary
                                           , HHINCOME, SEX, AGE # control vars
                                           , starts_with("INX"))
)

#summary(es)

# get standard errors
out = summary(es)
se = out$coefficients[ , 2] #extract 2nd column from the coefficients object in out

# filter to only the time variables, and scale standard errors
inx_se <- se[grep("^INX", names(se))]
inx_se <- inx_se * 1.96
#names(inx_se)

# get coefficients
df1 <- data.frame(es$coefficients)
df1 <- rownames_to_column(df1, var = "Name")

# df
se_df <- data.frame(Name = names(inx_se), Value = inx_se)
merged_df <- merge(se_df, df1, by = "Name")
# clean up df
merged_df$Year <- as.integer(gsub("INX", "", merged_df$Name)) # replace INX with integer
merged_df$Name <- NULL #get rid of 'Name' now that we have Year
colnames(merged_df)[colnames(merged_df) == "es.coefficients"] <- "Coefficient"
colnames(merged_df)[colnames(merged_df) == "Value"] <- "Std_Error"

ggplot(data = merged_df, mapping = aes(x = Year, y = Coefficient)) +
  geom_line(linetype = "dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin = (Coefficient-Std_Error), ymax = (Coefficient+Std_Error)), width = 0.2) +
  labs(title = "Coefficient with Error Bars",
       x = "Year",
       y = "Coefficient")

#---------------------------------------
#MODEL
#---------------------------------------

#oooooh boy

head(phi)

model <- lm(migrate_binary ~ 
           YEAR +
           post + #flag for pre/post
           city_ind + #flag for treatment vs. control
           YEAR:city_ind + # interaction
           YEAR:post +
           YEAR:post:city_ind +
           HHINCOME +
           AGE + 
           SEX,
         data = phi)

summary(model)





#### EVA'S code

# TRENDS DATA ------------------------------------------------------------------  

filtered_sample_prek_all <- read.csv("filtered_sample_prek_all.csv")

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