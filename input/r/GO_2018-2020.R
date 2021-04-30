library(readxl)
library(dplyr)
library(readr)

GO_2018_2020 = readxl::read_excel("input/raw/GO_2018-2020.xlsx")
municipios = read_csv("input/diretorio_municipios.csv")

GO_Tratado = GO_2018_2020 %>%

  transmute(city = stringr::str_to_lower(MUNICÍPIO) %>% stringi::stri_trans_general(str = ., 
                                                                                  id = "Latin-ASCII"),
            year = ANO,
            month = MÊS,
            day = DIA,
            neighbour = NA,
            state = "GO",
            crime = stringr::str_to_lower(DELITO), 
            sex_victim = SEXO,
            age_victim = IDADE, 
            race_victim = stringr::str_to_lower(CUTIS),
            school_victim = NA, 
            motivation = Motivação) %>%
  mutate(city = gsub("sao joao d´alianca", "sao joao d'alianca",city)) %>%
  left_join(municipios %>% mutate(municipio2 = stringi::stri_trans_general(str = municipio, 
                                                                           id = "Latin-ASCII") %>%
                                    stringr::str_to_lower()) %>%
              select(municipio2, estado_abrev, id_municipio, id_estado),
<<<<<<< HEAD
            by=c("state"="estado_abrev", "city"="municipio2")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)
=======
            by=c("state"="estado_abrev", "city"="municipio2"))
>>>>>>> main


saveRDS(GO_Tratado, "input/clean/T_GO_2018-2020.rds")


