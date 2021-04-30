library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

RN_2018_2020 = read_excel("input/raw/RN_2018-2020.xlsx")

RN_Tratado = RN_2018_2020 %>%
  transmute(id_ocorr = NA, 
    year = lubridate::year(DATA),
    month = lubridate::month(DATA), 
    day = lubridate::day(DATA),
    city = stringr::str_to_lower(`MUNICIPIO REF`),
    state = "RN",
    crime = case_when(`TIPO DE MORTE` == "HOMICIDIO DOLOSO" ~ "homicidio", 
                      `TIPO DE MORTE` == "FEMINICIDIO" ~ "feminicidio"),
    race_victim = stringr::str_to_lower(ETNIA),
    sex_victim = case_when(GENERO == "MASCULINO"~"M", GENERO == "FEMININO"~ "F"),
    age_victim = `IDADE`,
    school_victim = stringr::str_to_lower(ESCOLARIDADE) %>%  stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII"),
    neighbour = NA, 
    motivation = stringr::str_to_lower(`ANÁLISE MACROCAUSA`) %>%  stringi::stri_trans_general(str = ., 
                                                                                              id = "Latin-ASCII")) %>%
  mutate(city = gsub("gov dix-sept rosado", "governador dix-sept rosado", city), 
         city = gsub("assu", "acu", city),
         city = gsub("arez", "ares", city), 
         city = gsub("passa-e-fica", "passa e fica", city),
         city = gsub("vila-flor", "vila flor", city), 
         city = gsub("olho d'agua do borges", "olho-d'agua do borges",city),
         city = gsub("boa saude", "januario cicco", city),
         city = gsub("campo grande", "augusto severo",city),
         city = gsub("serra caiada","presidente juscelino",city)) %>%
  filter(!is.na(year)) %>%
  mutate(age_victim = na_if(age_victim,"NI") %>% as.numeric(),
         race_victim = na_if(race_victim, "ignorada"),
         school_victim = na_if(school_victim, "ignorada")) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                          id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(RN_Tratado, "input/clean/T_RN_2018-2020.rds")
