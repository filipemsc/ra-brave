library(readxl)
library(dplyr)
library(readr)

municipios = read_csv("input/diretorio_municipios.csv")

municipio = read_excel("input/raw/PE_2018-2020.xlsx", sheet = "Cor da Pele e Motivacao")

cor_pele = read_excel("input/raw/PE_2018-2020.xlsx", sheet = "Cor da Pele e Motivacao") %>%
  group_by(CONCATENADO) %>% 
  mutate(n=n()) %>% 
  filter(n == 1) %>%
  select(-MUNICIPIO, - SEXO, -n, - IDADE)

PE_2018_2020 = left_join(municipio, cor_pele,  by = "CONCATENADO")

PE_Tratado = PE_2018_2020 %>%
  transmute(id_ocorr = NA,
            city = stringr::str_to_lower(MUNICIPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            year = Ano,
            month = Mês,
            day = Dia,
            neighbour = NA,
            state = "PE",
            crime = stringr::str_to_lower(`NATUREZA JURIDICA`), 
            sex_victim = SEXO,
            age_victim = `IDADE`, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = MOTIVACAO) %>%
  mutate(sex_victim = case_when(sex_victim == "MASCULINO" ~ "M", 
                                sex_victim == "FEMININO" ~ "F"))%>%
  mutate(crime = case_when(crime == "feminicidio" ~ "feminicidio", TRUE ~ "homicidio")) %>%
  mutate(city = gsub("itamaraca", "ilha de itamaraca",city),
       city = gsub("belem de sao francisco", "belem do sao francisco",city),
       city = gsub("lagoa do itaenga", "lagoa de itaenga",city), 
       city = gsub("sao caetano", "sao caitano",city))%>%
  mutate(race_victim = na_if(race_victim, "nao informado")) %>%
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2"))%>%
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(PE_Tratado, "input/clean/T_PE_2018-2020.rds")
