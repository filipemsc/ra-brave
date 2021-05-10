library(readxl)
library(dplyr)
library(readr)


MS_2018_2020 = readxl::read_excel("input/raw/MS_2018-2020.xlsx")
municipios = read_csv("input/diretorio_municipios.csv")

MS_Tratado = MS_2018_2020 %>%
  
  transmute(id_ocorr = NA,
    city = stringr::str_to_lower(MUNICÍPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            
            
            year = ANO,
            month = MÊS,
            day = DIA,
            neighbour = stringr::str_to_lower(SUBLOCALIZAÇÃO),
            state = "MS",
            crime = stringr::str_to_lower(DELITO), 
            sex_victim = SEXO,
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = Motivação) %>%
  mutate(city = gsub("amandina", "ivinhema",city),
         city = gsub("cabeceira do apa", "ponta pora",city),
         city = gsub("prudencio thomaz", "rio brilhante",city), 
         city = gsub("sanga puita", "ponta pora",city), 
         city = gsub("culturama", "fatima do sul",city), 
         city = gsub("panambi", "dourados",city), 
         city = gsub("itahum", "dourados",city), 
         city = gsub("anhandui", "campo grande",city)) %>%
  
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2"))%>%
  
relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
         sex_victim, age_victim, race_victim, school_victim, motivation)


saveRDS(MS_Tratado, "input/clean/T_MS_2018-2020.rds")
