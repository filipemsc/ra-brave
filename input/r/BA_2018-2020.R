library(readxl)
library(dplyr)
library(readr)

BA_2018_2020 = readxl::read_excel("input/raw/BA_2018-2020.xlsx")
municipios = read_csv("input/diretorio_municipios.csv")

BA_Tratado = BA_2018_2020 %>%
  transmute(id_ocorr = NA,
            city = stringr::str_to_lower(MUNICIPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                    id = "Latin-ASCII"),
            year = ANO,
            month = MÊS,
            day = DIA,
            neighbour = stringr::str_to_lower(SUBLOCALIZACAO),
            state = "BA",
            crime = stringr::str_to_lower(DELITO), 
            sex_victim = SEXO,
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = MOTIVACAO) %>%
  mutate(sex_victim = case_when(sex_victim == "Masculino" ~ "M", 
                                sex_victim == "Feminino" ~ "F"))%>%
  
  mutate(city = gsub("dias d avila", "dias d'ávila",city),
         city = gsub("santa luz", "santaluz",city),
         city = gsub("santaluzia", "santa luzia",city))%>%
  
 
  
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
            by=c("state"="estado_abrev", "city"="municipio2"))%>%
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)

saveRDS(BA_Tratado, "input/clean/T_MG_2018-2020.rds")