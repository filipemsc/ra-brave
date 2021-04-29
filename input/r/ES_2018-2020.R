library(readxl)
library(dplyr)
library(readr)

ES_2018_2020 = readxl::read_excel("input/raw/ES_2018-2020.xlsx")

municipios = read_csv("input/diretorio_municipios.csv")

ES_Tratado = ES_2018_2020 %>%
  mutate(CRIME = case_when(FEMINICÍDIO == "FEMINICIDIO" ~ "feminicidio", TRUE ~"homicidio")) %>% 
  transmute(year = lubridate::year(DATA),
            month = lubridate::month(DATA), 
            day = lubridate::day(DATA), 
            city = stringr::str_to_lower(MUN_OBT) %>% stringi::stri_trans_general(str = ., 
                                                                                  id = "Latin-ASCII"),
            neighbour = NA,
            state = "ES",
            crime = CRIME, 
            sex_victim = SEXO,
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = NA) %>%
  mutate(race_victim = na_if(race_victim, "n inf")) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(ES_Tratado, "input/clean/T_ES_2018-2020.rds")
