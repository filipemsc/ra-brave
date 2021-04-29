library(dplyr)

VD_BR = list.files("input/clean/", pattern ="*.rds", full.names = T) %>% 
  purrr::map_df(readRDS)

## Testes de robustez da base

VD_BR %>% 
  count(school_victim)

VD_BR %>%
  count(race_victim)

VD_BR %>%
  count(sex_victim)

VD_BR %>% 
  count(crime) 

VD_BR %>%
  count(motivation) %>% View()

library(ggplot2)
VD_BR %>% filter(crime =="feminicidio") %>% count(state, year) %>% ggplot() + geom_col(aes(x=year, y = n)) + facet_wrap(~state)
