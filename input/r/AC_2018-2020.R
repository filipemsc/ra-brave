library(readxl)
library(dplyr)
library(readr)
library(tidylog)
library(tidyverse)


municipios = read_csv("input/diretorio_municipios.csv")

AC_2018_2020 <- read_xlsx("input/raw/AC_2018-2020.xlsx")

AC_2018_2020 <- AC_2018_2020 %>%
 mutate(`Data Fato` = as.numeric(`Data Fato`),
        `Data óbito` = as.numeric(`Data óbito`),
   `Data óbito` = as.Date(`Data óbito`, origin = "1900-01-01"),
          `Data Fato` = as.Date(`Data Fato`, origin = "1900-01-01"))

AC_Tratado <-   AC_2018_2020 %>% 
  transmute(id_ocorr = BO, 
            year = lubridate::year(`Data Fato`), 
            month = lubridate::month(`Data Fato`), 
            day = lubridate::day(`Data Fato`),
            city = stringr::str_to_lower(Cidade) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
            neighbour = stringr::str_to_lower(Bairro) %>% 
              stringi::stri_trans_general(str = .,id = "Latin-ASCII"),
            state = "AC",
            crime = case_when(
              Motivação == "Feminicídio" ~ "feminicidio" , 
              TRUE ~ "homicidio"),
            sex_victim = `Sx. V`,
            age_victim = `Id. V` %>% parse_number(),
            race_victim = stringr::str_to_lower(`Pele`),
            school_victim = NA,
            motivation = stringr::str_to_lower(`Motivação`) %>% 
              stringi::stri_trans_general(str = ., id = "Latin-ASCII")) %>%
           mutate(race_victim = na_if(race_victim,"ni"),
                  city = gsub("santa rosa","santa rosa do purus", city))


municipios = municipios %>% rename(city = municipio, state = estado_abrev)


AC_Tratado = AC_Tratado %>% left_join(municipios %>% mutate(city = city %>% 
                                                              stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>%
                                                              stringr::str_to_lower()) %>%
                                        select(city, state, id_municipio, id_estado),
                                      by=c("state", "city")) %>% 
  relocate(id_ocorr, id_estado, state, id_municipio, city, neighbour, month, day, year, crime, 
           sex_victim, age_victim, race_victim, school_victim, motivation)


saveRDS(AC_Tratado, "input/clean/T_AC_2018-2020.rds")

