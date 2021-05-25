library(dplyr)

municipios = data.table::fread("input/diretorio_municipios.csv", encoding = "UTF-8")

RJ_H_2018_2020= list.files("input/raw", pattern = "^RJ_H.*csv", full.names=T) %>% 
  purrr::map_df(~data.table::fread(.x))

RJ_Tratado = RJ_H_2018_2020 %>%
  filter(titulo_do== "Homicídio doloso") %>% 
  mutate(feminic = grepl("Feminicídio", titulo)) %>%
  mutate(data_fato = lubridate::dmy(data_fato)) %>%
  transmute(id_ocorr = controle,
            year = lubridate::year(data_fato),
            month = lubridate::month(data_fato), 
            day = lubridate::day(data_fato), 
            city = stringr::str_to_lower(municipio_fato) %>%
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"),
            neighbour = stringr::str_to_lower(bairro_fato),
            state = "RJ",
            crime = case_when(feminic == TRUE ~ "feminicidio", TRUE ~ "homicidio"),
            sex_victim = case_when(sexo == "masculino"~"M", 
                                   sexo == "feminino" ~ "F"),
            age_victim = idade, 
            race_victim = cor,
            school_victim = stringr::str_to_lower(escolaridade) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII"), 
            motivation = NA 
  ) %>% 
  mutate(race_victim = na_if(race_victim, "ignorado"),
         race_victim = na_if(race_victim, "<NA>"), 
         race_victim = na_if(race_victim, "sem informação")) %>%
  mutate(sex_victim = na_if(sex_victim, "<NA>")) %>%
  mutate(school_victim = na_if(school_victim, "ignorado"), 
         school_victim = na_if(school_victim, "sem informacao")) %>%
  filter(year %in% c(2018:2020)) %>%
  left_join(municipios %>% mutate(municipio2 = municipio %>% stringi::stri_trans_general(str = ., 
                                                                                         id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(RJ_Tratado, "input/clean/T_RJ_2018-2020.rds")
