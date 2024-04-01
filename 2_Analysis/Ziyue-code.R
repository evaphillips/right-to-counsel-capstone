# Adding Binary Variables and Dummies-------------------------------------------
#Binary variable for Sex
control$MALE <- ifelse(control$SEX == 1, 1, 0)

#Binary Dummies for Race
control$ WHITE<- ifelse(control$RACE == 1, 1, 0)
control$BLACK <- ifelse(control$RACE == 2, 1, 0)
control$INDIAN_ALASKA <- ifelse(control$RACE == 3, 1, 0)
control$ASIAN_PACIFIC <- ifelse(control$RACE %in% c(4, 5, 6), 1, 0)
control$OTHER_RACE <- ifelse(control$RACE == 7, 1, 0)
control$MULTIPLE_RACE <- ifelse(control$RACE %in% c(8,9), 1, 0)

#Binary variable for married people
control$MARRIED <- factor(control$MARST %in% c(1,2), 1, 0)

#Binary variable for employed people
control$EMPLOYED <- factor(control$EMPSTAT == 1, 1, 0)

#Binary variable for not in labor force
control$NOTLABOR <- factor(control$EMPSTAT == 3, 1, 0)

#Dummy variables for numbers of generation in the family
control$ONE_GEN <- factor(control$MULTGEN == 1, 1, 0)
control$TWO_GEN <- factor(control$MULTGEN == 2, 1, 0)
control$THREE_MORE_GEN <- factor(control$MULTGEN == 3, 1, 0)



