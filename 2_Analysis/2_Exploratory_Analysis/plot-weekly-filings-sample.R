# Sample R code to plot weekly filings
# For Milwaukee until week 24 (06/13/2020)

# library(dplyr)
# library(tidyr)
# library(ggplot2)

mke_tract_week_2020 %>%   
  group_by(week, week_date) %>% 
  summarize(filings_2020 = sum(filings_2020),
            filings_avg = sum(filings_avg, na.rm = T)) %>%
  pivot_longer(cols = filings_2020:filings_avg,
               names_to = "year",
               values_to = "filings",
               names_prefix = "filings_") %>% 
  mutate(year = recode(year,
                       avg = "2012-2016")) %>% 
  ggplot(aes(x = week,
             y = filings)) +
  geom_line(aes(color = year)) +
  labs(title = "Milwaukee Weekly Eviction Filings")