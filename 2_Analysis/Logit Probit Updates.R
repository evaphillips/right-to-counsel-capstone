library(stargazer)
library(gtsummary)
library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(ipumsr)
library(tidyr)

df <- read.csv("/Users/anuskacorbin/Desktop/universal-pre-k/0_Data/filtered_sample_prek_allcities.csv") 

# Event Study ---------------------------------------------------------------------

# remove scientific notation
options(scipen = 999, digits = 10)

# filter dataset to only NYC and control (San Diego in this case)
control <- subset(df, city %in% c("NYC", "SD"))

# apply weights (HHWT)
control <- control[rep(seq_len(nrow(control)), control$HHWT), ]

# create a flag for 1 or 0 if it's NYC 
control$city_ind <- ifelse(control$city == "NYC", 1, 0)

# create a flag for post-period
control$post <- ifelse(control$YEAR >= 2014, 1, 0)

# Adding Binary Variables and Dummies-------------------------------------------

#Make MIGRATE1 binary
control <- control %>%
  mutate(migrate_binary = ifelse(MIGRATE1 %in% c(2, 3, 4), 1, 0))

#Binary variable for Sex
control$MALE <- ifelse(control$SEX == 1, 1, 0)

#Binary Dummies for Race
control$WHITE<- ifelse(control$RACE == 1, 1, 0)
control$BLACK <- ifelse(control$RACE == 2, 1, 0)
control$INDIAN_ALASKA <- ifelse(control$RACE == 3, 1, 0)
control$ASIAN_PACIFIC <- ifelse(control$RACE %in% c(4, 5, 6), 1, 0)
control$OTHER_RACE <- ifelse(control$RACE == 7, 1, 0)
control$MULTIPLE_RACE <- ifelse(control$RACE %in% c(8,9), 1, 0)

#Binary variable for married people
control$married1 <- ifelse(control$MARST == 1, 1,0)

control$MARRIED <- ifelse(control$MARST %in% c(1,2), 1, 0)

#Binary variable for employed people
control$EMPLOYED <- ifelse(control$EMPSTAT == 1, 1, 0)

#Binary variable for not in labor force
control$NOTLABOR <- ifelse(control$EMPSTAT == 3, 1, 0)

#Dummy variables for numbers of generation in the family
control$ONE_GEN <- ifelse(control$MULTGEN == 1, 1, 0)
control$TWO_GEN <- ifelse(control$MULTGEN == 2, 1, 0)
control$THREE_MORE_GEN <- ifelse(control$MULTGEN == 3, 1, 0)

# create a flag for year
prepended_value <- "INX"
control$Year_INX <- paste0(prepended_value, control$YEAR)
control_wide <- pivot_wider(control, names_from = Year_INX, values_from = Year_INX, values_fn = list(Year_INX = length), values_fill = 0)

lapply(control[c('MARRIED', 'EMPLOYED', 'NOTLABOR')], unique)



# Logit model with controls --------------------------------------------------------------------
logit <- glm(migrate_binary ~ 
               post + #flag for pre/post
               city_ind + #flag for treatment vs. control
               post:city_ind +
               HHINCOME + # control variable
               AGE + # control
               MALE +
               POVERTY +
               NCHILD +
               WHITE + 
               BLACK +
               INDIAN_ALASKA +
               ASIAN_PACIFIC +
               OTHER_RACE +
               MULTIPLE_RACE +
               MARRIED +
               EMPLOYED +
               NOTLABOR +
               ONE_GEN +
               TWO_GEN +
               THREE_MORE_GEN,
             family=binomial (link = "logit"), 
             data = control)

summary(logit)

# Probit model with controls ---------------------------------------------------------
probit <- glm(migrate_binary ~ 
                post + #flag for pre/post
                city_ind + #flag for treatment vs. control
                post:city_ind +
                HHINCOME + # control variable
                AGE + # control
                MALE +
                POVERTY +
                NCHILD +
                WHITE + 
                BLACK +
                INDIAN_ALASKA +
                ASIAN_PACIFIC +
                OTHER_RACE +
                MULTIPLE_RACE +
                MARRIED +
                EMPLOYED +
                NOTLABOR +
                ONE_GEN +
                TWO_GEN +
                THREE_MORE_GEN,
              family=binomial (link = "probit"), 
              data = control)

summary(probit)




