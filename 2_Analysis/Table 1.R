library(gtsummary)

install.packages("survey")
library(survey)
data(filtered_sample_prek_allcities, package = "survey")

svy_filtered_sample_prek_allcities <-
  survey::svydesign(
    id = ~CLUSTER,
    weights = ~HHWT,
    data = filtered_sample_prek_allcities
  )

install.packages("tbl_svysummary")
library("tbl_svysummary")
svy_filtered_sample_prek_allcities %>%
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
      EDUC ~ "Years of Education",
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