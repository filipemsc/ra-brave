library(readxl)
library(dplyr)
library(readr)
library(tidylog)
library(tidyverse)

municipios = read_csv("input/diretorio_municipios.csv")

CE_2018_2020 = read_excel("input/raw/CE_2018-2020.xlsx")

CE_Tratado =
  CE_2018_2020 %>%
  tidyr::separate(DATA, c("Data", "Hora"), " ") %>%
  transmute(id_ocorr = NA,
            year = lubridate::year(Data),
            month = lubridate::month(Data), 
            day = lubridate::day(Data), 
            city = stringr::str_to_lower(`MUNICÍPIO`) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
            neighbour = NA,
            state = "CE",
            crime = stringr::str_to_lower(`NATUREZA`) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"), 
            sex_victim = GÊNERO,
            age_victim = IDADE, 
            race_victim = NA,
            school_victim = stringr::str_to_lower(`ESCOLARIDADE`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"), 
            motivation = NA) %>% 
  mutate(sex_victim = gsub("Masculino","M", sex_victim),
         sex_victim = gsub("Feminino", "F", sex_victim)) %>% 
  mutate(race_victim = na_if(race_victim,"ni")) %>%
  mutate(city = gsub("itapaje","itapage", city),
         city = gsub("dep. irapuan pinheiro", "deputado irapuan pinheiro", city)) %>%
  mutate(crime = case_when(crime == "feminicidio" ~ "feminicidio", TRUE ~ "homicidio"))

municipios = municipios %>% rename(city = municipio, state = estado_abrev)

CE_Tratado = CE_Tratado %>% left_join(municipios %>% mutate(city = city %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>%
              stringr::str_to_lower()) %>%
            select(city, state, id_municipio, id_estado),
          by=c("state", "city")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(CE_Tratado, "input/clean/T_CE_2018-2020.rds")

