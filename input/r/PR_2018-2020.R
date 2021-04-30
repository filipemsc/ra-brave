library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

PR_2018_2020 = read_excel("input/raw/PR_2018-2020.xlsx")

PR_Tratado =
PR_2018_2020 %>%  
  transmute(id_ocorr = NA,
    year = lubridate::year(`Data do Fato`),
            month = lubridate::month(`Data do Fato`), 
            day = lubridate::day(`Data do Fato`), 
            city = stringr::str_to_lower(municipio),
            neighbour = NA,
            state = "PR",
            crime = stringr::str_to_lower(Natureza) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"), 
            sex_victim = `sexo`,
            age_victim = parse_number(`Idade`), 
            race_victim = stringr::str_to_lower(raca_cor),
            school_victim = NA, 
            motivation = stringr::str_to_lower(`motivacao`) %>% stringi::stri_trans_general(str = ., 
                                                                                            id = "Latin-ASCII")) %>% 
  mutate(race_victim = na_if(race_victim,"ni"),
         race_victim = na_if(race_victim,"np")) %>% 
  mutate(motivation = na_if(motivation, "ni"), 
         motivation = na_if(motivation, "np"))%>% 
  mutate(sex_victim = na_if(sex_victim, "ni"), 
         sex_victim = na_if(sex_victim, "NP")) %>%
  mutate(race_victim = gsub("indigina", "indigena", race_victim)) %>%
  mutate(city = gsub("sao jorge do oeste","sao jorge d'oeste", city),
         city = gsub("itapejara do oeste","itapejara d'oeste", city),
         city = gsub("diamante do oeste", "diamante d'oeste", city),
         city = gsub("perola do oeste","perola d'oeste", city)) %>%
  mutate(crime = gsub("homicidio doloso", "homicidio", crime)) %>% 
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(PR_Tratado, "input/clean/T_PR_2018-2020.rds")
