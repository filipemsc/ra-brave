library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

SC_2018_2020 = read_excel("input/raw/SC_2018-2020.xlsx")

SC_Tratado = 
SC_2018_2020 %>% 
  mutate(SISTEMA = case_when(`VIOLÊNCIA DOMÉSTICA` == "SIM" ~ "feminicidio", TRUE ~"homicidio")) %>%
  transmute(year = lubridate::year(`DATA`),
            month = lubridate::month(`DATA`), 
            day = lubridate::day(`DATA`), 
            city = stringr::str_to_lower(MUNICÍPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            neighbour = NA,
            state = "SC",
            crime = stringr::str_to_lower(SISTEMA), 
            sex_victim =case_when(`SEXO`== "MASC" ~ "M", SEXO == "FEM"~ "F"),
            age_victim = `IDADE`, 
            race_victim = stringr::str_to_lower(`ETNIA/RAÇA`) %>% stringi::stri_trans_general(str = ., 
                                                                                              id = "Latin-ASCII"),
            school_victim = NA, 
            motivation = stringr::str_to_lower(MOTIVAÇÃO) %>% stringi::stri_trans_general(str = ., 
                                                                                          id = "Latin-ASCII")) %>% 
  mutate(age_victim = na_if(age_victim, 999)) %>%
  mutate(race_victim = na_if(race_victim, "nao informada")) %>% 
  mutate(motivation = na_if(motivation, "nao informada"),
         motivation = na_if(motivation, "outra")) %>% 
  mutate(city = gsub("luis alves", "luiz alves", city),
         city = gsub("herval d´oeste", "herval d'oeste", city)) %>% 
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(SC_Tratado, "input/clean/T_SC_2018-2020.rds")
