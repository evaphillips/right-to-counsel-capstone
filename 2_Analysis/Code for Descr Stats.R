

library(tidyverse)
library(tidycensus)
library(grid)
library(gridExtra)
library(readxl)

filtered_sample_prek_18 %>% 
  select(!c(YEAR,SAMPLE,SERIAL,CBSERIAL,HHWT,CLUSTER,STATEFIP,COUNTYFIP,DENSITY,
            PUMA,STRATA,GQ,OWNERSHP,OWNERSHPD,MULTGEN,MULTGEND,PERNUM,PERWT,FAMUNIT,
            MOMLOC,MOMRULE,SPLOC,SPRULE,NCHLT5,RELATE,RELATED,RACED,EMPSTAT,EMPSTATD,
            MIGRATE1,MIGRATE1D)) %>% 
  tbl_summary(by = city, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                                                                                                                                                                                                                                 all_categorical() ~ "{n} / {N} ({p}%)"
                                                                                                                                                                                                                                                                                                                 ), digits = all_continuous() ~ 2) %>% add_p() %>% add_overall() %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Location**")
  
  