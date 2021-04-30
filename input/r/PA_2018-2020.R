library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

PA_2018_2020 = read_excel("input/raw/PA_2018-2020.xlsx", skip = 1)

PA_Tratado = 
PA_2018_2020 %>% 
  transmute(id_ocorr = NA,
    year = lubridate::year(`DATA FATO`),
            month = lubridate::month(`DATA FATO`), 
            day = lubridate::day(`DATA FATO`), 
            city = stringr::str_to_lower(MUNICIPIOS),
            neighbour = stringr::str_to_lower(BAIRROS),
            state = "PA",
            crime = stringr::str_to_lower(DELITO), 
            sex_victim = `VIT SEXO`,
            age_victim = `VIT IDADE`, 
            race_victim = NA,
            school_victim = stringr::str_to_lower(`VIT GRAU INST`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"), 
            motivation = stringr::str_to_lower(`CAUSA PRESUMIVEL`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>% 
  mutate(city = gsub("altamira/castelo dos sonhos", "altamira",city), 
         city = gsub("santa izabel do para","santa isabel do para", city)) %>%
  mutate(crime = gsub("femicidio","homicidio", crime)) %>% 
  mutate(sex_victim = na_if(sex_victim, "NI"),
         age_victim = na_if(age_victim, 66666),
         age_victim = na_if(age_victim,99999),
         age_victim = na_if(age_victim,9999),
         school_victim = na_if(school_victim,"nao informado")) %>% 
  mutate(school_victim = gsub("ensino ","", school_victim)) %>% 
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(PA_Tratado, "input/clean/T_PA_2018-2020.rds")

